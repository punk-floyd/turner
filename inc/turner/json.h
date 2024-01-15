/**
 * @file    json.h
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   C++20 UTF-8 JSON encoding/decoding library
 * @date    2023-11-30
 *
 * https://github.com/punk-floyd/turner
 *
 * @copyright Copyright (c) 2023 Mike DeKoker
 *
 */
#ifndef zzz_I_assure_you_that_json_turner_dot_h_has_been_included
#define zzz_I_assure_you_that_json_turner_dot_h_has_been_included

#include <initializer_list>
#include <system_error>
#include <string_view>
#include <type_traits>
#include <charconv>
#include <concepts>
#include <optional>
#include <iterator>
#include <fstream>
#include <variant>
#include <ranges>
#include <string>
#include <vector>
#include <limits>
#include <memory>
#include <cmath>
#include <array>
#include <map>

namespace turner {

#ifdef TURNER_DEFAULT_ALLOW_INTEGER_DECODE
constexpr inline bool default_allow_integer_decode = true;
#else
constexpr inline bool default_allow_integer_decode = false;
#endif

/// turner::json specific error codes
enum class json_error {
    // Decode errors: [100,200)
    unexpected_token = 100,
    unterminated_array,
    unterminated_string,
    unterminated_json,
    trailing_garbage,
    expected_object_name,
    expected_colon,
    unknown_control_sequence,
    invalid_hex_char,
    number_out_of_range,
    not_a_token,
    not_a_number,
    nonunique_member_name,

    // Encode errors: [200,300)
    invalid_json_value = 200,
    number_nan,
    number_inf,
    unknown_encoding_disposition
};

// Defined way down below
inline const std::error_category& json_category() noexcept;

/// Make a std:error_code from a json_error
inline std::error_code make_error_code(json_error e) noexcept
{
    return std::error_code{static_cast<int>(e), json_category()};
}

/// Make a std:error_condition from a json_error
inline std::error_condition make_error_condition(json_error e) noexcept
{
    return std::error_condition{static_cast<int>(e), json_category()};
}

/// JSON encoding policy: adjusts encoding behavior
struct encode_policy {

    enum class Disposition : unsigned char {
        Fail,           ///< Fail encoding operation
        Null,           ///< Replace offending item with a JSON null
        Throw           ///< Throw system_error (json_error)
    };

    constexpr encode_policy() noexcept = default;

    // Construct using a common disposition
    constexpr explicit encode_policy(Disposition d) noexcept
        : value_invalid(d), number_nan(d), number_inf(d)
    {}

    /// What to do when value is invalid (default constructed)
    Disposition value_invalid{Disposition::Fail};
    /// What to do when a number is NaN
    Disposition number_nan{Disposition::Fail};
    /// What to do when a number is +/- infinity
    Disposition number_inf{Disposition::Fail};
};

/// JSON decoding policy: adjusts decoding behavior
struct decode_policy {

    constexpr decode_policy() noexcept = default;

    enum class NonUniqueDisposition : unsigned char {
        Overwrite,      ///< Overwrite the existing value
        Fail,           ///< Fail encoding operation
        Throw           ///< Throw system_error (json_error)
    };

    /// If true, expect to parse the whole input range
    bool    greedy{true};
    /// Allow decode to parse apparent integers as integers
    bool    allow_integer_decode{default_allow_integer_decode};
    /// What to do if we encounter an object member name that's already been parsed
    NonUniqueDisposition non_unique_member_name_disposition{NonUniqueDisposition::Overwrite};
};

class json
{
public:

    using nullptr_t = std::nullptr_t;

    // -- Construction

    /// Constructs a default object with no JSON data
    constexpr json() noexcept = default;

    /// Construct with JSON data decoded from the given range
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    json(InputIt first, Stop last,
        decode_policy policy = decode_policy{})
    {
        if (const auto res = decode(first, last, policy); !res) {
            throw std::system_error(make_error_code(res.err));
        }
    }

    /// Construct with JSON data decoded from the given range
    template <std::ranges::input_range R>
    explicit json(R&& range, decode_policy policy = decode_policy{})
    {
        if (const auto res = decode(range, policy); !res) {
            throw std::system_error(make_error_code(res.err));
        }
    }

    /// Construct with JSON data decoded from a NULL terminated raw string
    explicit json (const char* src, decode_policy policy = decode_policy{})
        : json(std::string_view(src), policy)
    { }

    json (nullptr_t, bool) = delete;

    // -- Decoding

    /// The result of a decode method
    template <std::input_iterator InputIt>
    struct decode_result {

        constexpr decode_result() = default;

        constexpr decode_result(InputIt input_it, json_error code = json_error{})
            : it(input_it), err(code)
        {}

        /// This object implicitly converts to bool: true means we're error free
        constexpr operator bool() const noexcept { return err == json_error{}; }

        InputIt     it{};   ///< End of decoded data or error location
        json_error  err{};  ///< Offending error code, or 0
    };

private:

    /// Parsing context used by the parsing implementation
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    struct parse_ctx {

        constexpr parse_ctx(const decode_policy& decode_policy,
            InputIt at, Stop end) noexcept
            : policy(decode_policy), it_at(at), it_end(end)
        {}

        /// We're empty if we have no more input
        [[nodiscard]] constexpr bool empty() const noexcept
            { return it_at == it_end; }

        /// We evaluate to true if we have no error
        constexpr explicit operator bool() const noexcept
            { return err == json_error{}; }

        decode_policy    policy;
        InputIt          it_at;      ///< Current location in input
        Stop             it_end;     ///< End if input
        json_error       err{};      ///< Parse error, or {}
    };

public:

    /**
     * @brief Decode JSON data in the given range
     *
     * @param first     The start of the range to decode
     * @param last      The end of the range to decode
     * @param greedy    If true, the method will attempt to decode the whole
     *  input range. If false, decoding will stop after successfully
     *  decoding the top level JSON value.
     *
     * @return Returns a decode_result containing results; @see decode_result
     */
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    constexpr auto decode(InputIt first, Stop last,
        decode_policy policy = decode_policy{})
        -> decode_result<InputIt>
    {
        parse_ctx p_ctx{policy, first, last};
        _value = parse_value(p_ctx);

        if (p_ctx && policy.greedy && !eat_whitespace(p_ctx)) {
            p_ctx.err = json_error::trailing_garbage;
        }

        return decode_result { p_ctx.it_at, p_ctx.err };
    }

    /// Parse JSON data in the given range
    template <std::ranges::input_range R>
    constexpr auto decode(R&& r, decode_policy policy = decode_policy{})
    {
        return decode(begin(r), end(r), policy);
    }

    /// Parse from a C string
    constexpr auto decode(const char* src, decode_policy policy = decode_policy{})
    {
        return decode(std::string_view(src), policy);
    }

    auto decode(nullptr_t, bool) = delete;

    /// Decode data from the given input stream
    auto decode_stream(std::istream& ifs, decode_policy policy = decode_policy{})
    {
        auto if_begin = std::istreambuf_iterator<char>{ifs};
        auto if_end   = std::istreambuf_iterator<char>{std::default_sentinel};
        return decode(if_begin, if_end, policy);
    }

    // -- Attributes

    class value;

    /// Access root value
    [[nodiscard]] constexpr       value&  get_value()       &  noexcept { return _value; }
    [[nodiscard]] constexpr const value&  get_value() const &  noexcept { return _value; }
    [[nodiscard]] constexpr       value&& get_value()       && noexcept { return std::move(_value); }

    // -- JSON value types

    using object = std::map<std::string, value, std::less<>>;
    using array  = std::vector<value>;
    using string = std::string;
    using number = double;
    using integer = int64_t;

    using object_ptr = std::unique_ptr<object>;
    using array_ptr  = std::unique_ptr<array>;

    using value_variant = std::variant <
        nullptr_t,      // null
        object_ptr,     // { ... }
        array_ptr,      // [ ... ]
        string,         //  "..."
        number,         // 123.456
        integer,        // 123  (non standard for decoding)
        bool            // true/false
    >;

    /// A JSON value: one of: string, number, object, array, Boolean, or null
    class value : public value_variant
    {
    public:

        // -- Construction

        // Default construction is a JSON null
        constexpr value() noexcept = default;

        // Construct from a value (pass through)
        template <class Arg>
            requires (!std::is_same_v<std::remove_cvref_t<Arg>, value> && std::is_constructible_v<value_variant, Arg>)
        constexpr value(Arg&& arg) noexcept(std::is_nothrow_constructible_v<value_variant, Arg>)
            : value_variant(std::forward<Arg>(arg))
        {}

        // -- Observers

        constexpr bool is_object() const noexcept { return is<object_ptr>(); }
        [[nodiscard]] constexpr       object_ptr&  get_object()       &           { return get<object_ptr>(); }
        [[nodiscard]] constexpr const object_ptr&  get_object() const &           { return get<object_ptr>(); }
        [[nodiscard]] constexpr       object_ptr&& get_object()       &&          { return std::move(get<object_ptr>()); }
        [[nodiscard]] constexpr       object_ptr*  get_object_if()       noexcept { return try_get<object_ptr>(); }
        [[nodiscard]] constexpr const object_ptr*  get_object_if() const noexcept { return try_get<object_ptr>(); }

        constexpr bool is_array()  const noexcept { return is<array_ptr>(); }
        [[nodiscard]] constexpr       array_ptr&   get_array()       &            { return get<array_ptr>(); }
        [[nodiscard]] constexpr const array_ptr&   get_array() const &            { return get<array_ptr>(); }
        [[nodiscard]] constexpr       array_ptr&&  get_array()       &&           { return std::move(get<array_ptr>()); }
        [[nodiscard]] constexpr       array_ptr*   get_array_if()       noexcept  { return try_get<array_ptr>(); }
        [[nodiscard]] constexpr const array_ptr*   get_array_if() const noexcept  { return try_get<array_ptr>(); }

        constexpr bool is_string() const noexcept { return is<string>(); }
        [[nodiscard]] constexpr       string&      get_string()       &            { return get<string>(); }
        [[nodiscard]] constexpr const string&      get_string() const &            { return get<string>(); }
        [[nodiscard]] constexpr       string&&     get_string()      &&            { return std::move(get<string>()); }
        [[nodiscard]] constexpr       string*      get_string_if()       noexcept  { return try_get<string>(); }
        [[nodiscard]] constexpr const string*      get_string_if() const noexcept  { return try_get<string>(); }

        constexpr bool is_number() const noexcept { return is<number>(); }
        [[nodiscard]] constexpr       number&      get_number()                    { return get<number>(); }
        [[nodiscard]] constexpr const number&      get_number() const              { return get<number>(); }
        [[nodiscard]] constexpr       number*      get_number_if()       noexcept  { return try_get<number>(); }
        [[nodiscard]] constexpr const number*      get_number_if() const noexcept  { return try_get<number>(); }

        constexpr bool is_integer() const noexcept { return is<integer>(); }
        [[nodiscard]] constexpr       integer&     get_integer()                   { return get<integer>(); }
        [[nodiscard]] constexpr const integer&     get_integer() const             { return get<integer>(); }
        [[nodiscard]] constexpr       integer*     get_integer_if()       noexcept { return try_get<integer>(); }
        [[nodiscard]] constexpr const integer*     get_integer_if() const noexcept { return try_get<integer>(); }

        constexpr bool is_bool()   const noexcept { return is<bool>();  }
        [[nodiscard]] constexpr       bool&        get_bool()                      { return get<bool>(); }
        [[nodiscard]] constexpr const bool&        get_bool() const                { return get<bool>(); }
        [[nodiscard]] constexpr       bool*        get_bool_if()       noexcept    { return try_get<bool>(); }
        [[nodiscard]] constexpr const bool*        get_bool_if() const noexcept    { return try_get<bool>(); }

        constexpr bool is_null()   const noexcept { return is<nullptr_t>(); }
        [[nodiscard]] constexpr       nullptr_t&   get_null()                      { return get<nullptr_t>(); }
        [[nodiscard]] constexpr const nullptr_t&   get_null() const                { return get<nullptr_t>(); }
        [[nodiscard]] constexpr       nullptr_t*   get_null_if()       noexcept    { return try_get<nullptr_t>(); }
        [[nodiscard]] constexpr const nullptr_t*   get_null_if() const noexcept    { return try_get<nullptr_t>(); }

        template <typename T>
        inline constexpr bool is() const noexcept
            { return std::holds_alternative<T>(*this); }

        // -- Encoding

        /// The result of an encode method
        template <std::output_iterator<char> OutputIt>
        struct encode_result {

            constexpr encode_result() = default;

            constexpr encode_result(OutputIt output_it, json_error code = json_error{})
                : it(output_it), err(code)
            {}

            /// This object implicitly converts to bool: true means we're error free
            constexpr operator bool() const noexcept { return err == json_error{}; }

            OutputIt    it{};   ///< End of encoded data
            json_error  err{};  ///< Offending error code, or 0
        };

        /// JSON encode value to an output iterator
        template <std::output_iterator<char> OutputIt>
        [[nodiscard]] constexpr inline auto encode(OutputIt it, encode_policy policy = encode_policy{}) const
            -> encode_result<OutputIt>
        {
            return encode_value(*this, it, policy);
        }

        /// JSON encode value to a string
        [[nodiscard]] std::string inline encode(encode_policy policy = encode_policy{}) const
        {
            std::string s;
            const auto res = encode(std::back_inserter(s), policy);
            if (!res) {
                return std::string{};
            }

            return s;
        }

        // -- Encoding implementation --------------------------------------

        /// Handle an encoding error based on specified policy
        template <std::output_iterator<char> OutputIt>
        static constexpr auto encode_error (json_error ec, OutputIt it,
            encode_policy policy, encode_policy::Disposition d) -> encode_result<OutputIt>
        {
            using Disposition = encode_policy::Disposition;

            switch (d) {
            case Disposition::Null: return encode_literal("null", it, policy);
            case Disposition::Fail: return encode_result{it, ec};
            case Disposition::Throw: throw std::system_error(make_error_code(ec));
            }

            return encode_result{it, json_error::unknown_encoding_disposition};
        }

        /// JSON-encode the given value to the given output iterator
        template <std::output_iterator<char> OutputIt>
        static constexpr inline auto encode_value (const value& val, OutputIt it,
            encode_policy policy = encode_policy{}) -> encode_result<OutputIt>
        {
            const auto visitor = [it, policy](const auto& v) -> encode_result<OutputIt> {
                if      constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, object_ptr>) {
                    return encode_object(*v.get(), it, policy);
                }
                else if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, array_ptr>) {
                    return encode_array(*v.get(), it, policy);
                }
                else if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, string>) {
                    return encode_string(v, it, policy);
                }
                else if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, number>) {
                    return encode_number(v, it, policy);
                }
                else if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, integer>) {
                    return encode_integer(v, it, policy);
                }
                else if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, bool>) {
                    return encode_literal(v ? "true" : "false", it, policy);
                }
                else if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, nullptr_t>) {
                    return encode_literal("null", it, policy);
                }
                else {
                    return encode_error(json_error::invalid_json_value, it, policy, policy.value_invalid);
                }
            };

            return std::visit(visitor, static_cast<const value_variant&>(val));
        }

        /// JSON-encode the given object to the given output iterator
        template <std::output_iterator<char> OutputIt>
        static auto encode_object(const object& val, OutputIt it,
            encode_policy policy = encode_policy{}) -> encode_result<OutputIt>
        {
            *it++ = '{';

            for (bool first = true; const auto& [mem_key,mem_val] : val) {
                if (first) {
                    first = false;
                }
                else {
                    *it++ = ',';
                }

                auto encode_result = encode_string(mem_key, it, policy);
                if (!encode_result) {
                    return encode_result;
                }
                it = encode_result.it;

                *it++ = ':';
                encode_result = encode_value(mem_val, it, policy);
                if (!encode_result) {
                    return encode_result;
                }
                it = encode_result.it;
            }

            *it++ = '}';

            return it;
        }

        /// JSON-encode the given array to the given output iterator
        template <std::output_iterator<char> OutputIt>
        static constexpr auto encode_array(const array& val, OutputIt it,
            encode_policy policy = encode_policy{}) -> encode_result<OutputIt>
        {
            *it++ = '[';

            for (bool first = true; const auto& e : val) {
                if (first) {
                    first = false;
                }
                else {
                    *it++ = ',';
                }

                auto encode_result = encode_value(e, it, policy);
                if (!encode_result) {
                    return encode_result;
                }
                it = encode_result.it;
            }

            *it++ = ']';

            return it;
        }

        /// JSON-encode the given string to the given output iterator
        template <std::output_iterator<char> OutputIt>
        static constexpr auto encode_string(const string& val, OutputIt it,
            [[maybe_unused]] encode_policy policy = encode_policy{}) -> encode_result<OutputIt>
        {
            *it++ = '"';

            for (const auto& c : val) {

                // Must escape double quote -> \" <-
                if (c == '"') {
                    *it++ = '\\';
                    *it++ = '"';
                }
                // Must escape reverse solidus -> \\ <-
                else if (c == '\\') {
                    *it++ = '\\';
                    *it++ = '\\';
                }
                // Must escape control codes -> \u00XX <-
                else if ((c >= 0) && (c <= 0x1F)) {
                    *it++ = '\\';
                    *it++ = 'u';
                    *it++ = '0';
                    *it++ = '0';
                    *it++ = (c & 0x10) ? '1' : '0';
                    const auto nibble = c & 0xF;
                    *it++ = (nibble < 10)
                        ? static_cast<char>('0' + nibble)
                        : static_cast<char>('A' + nibble - 10);
                }
                // Everything else is passed thru
                else {
                    *it++ = c;
                }
            }

            *it++ = '"';

            return it;
        }

        /// JSON-encode the given number to the given output iterator
        template <std::output_iterator<char> OutputIt>
        static constexpr auto encode_number(const number& val, OutputIt it,
            encode_policy policy = encode_policy{}) -> encode_result<OutputIt>
        {
            constexpr auto buf_len = std::numeric_limits<double>::max_digits10 + 8;

            if (std::isnan(val)) {
                return encode_error(json_error::number_nan, it,
                    policy, policy.number_nan);
            }
            if (std::isinf(val)) {
                return encode_error(json_error::number_nan, it,
                    policy, policy.number_inf);
            }

            std::array<char, buf_len> buf;
            auto* first = buf.data();
            auto* last  = buf.data() + buf.size();
            const auto [ptr, ec] = std::to_chars(first, last, val);
            for (const auto* p = first; p != ptr; *it++ = *p++) {}

            return it;
        }

        /// JSON-encode the given integer to the given output iterator
        template <std::output_iterator<char> OutputIt>
        static constexpr auto encode_integer(const integer& val, OutputIt it,
            encode_policy policy = encode_policy{}) -> encode_result<OutputIt>
        {
            constexpr auto buf_len = std::numeric_limits<integer>::digits10 + 2;

            std::array<char, buf_len> buf;
            auto* first = buf.data();
            auto* last  = buf.data() + buf.size();
            const auto [ptr, ec] = std::to_chars(first, last, val);
            for (const auto* p = first; p != ptr; *it++ = *p++) {}

            return it;
        }

        /// Output the given literal string to the given output iterator
        template <std::output_iterator<char> OutputIt>
        static constexpr inline auto encode_literal(std::string_view literal, OutputIt it,
        encode_policy policy = encode_policy{}) -> encode_result<OutputIt>
        {
            // This guy is used to "encode" true, false, and null

            for (auto c : literal) {
                *it++ = c;
            }

            return it;
        }

        // -- Implementation

        // Gang of five. Moving and copying okay, but we need custom copying
        // because std::unique_ptr isn't copyable.

        ~value() = default;
        value(value&&) noexcept             = default;
        value& operator=(value&&) noexcept  = default;

        value(const value& other)
            : value_variant(value_variant_deep_copy(other))
        {}

        value& operator=(const value& other)
        {
            static_cast<value_variant&>(*this) = value_variant_deep_copy(other);
            return *this;
        }

    private:

        // Return a copy of the given value_variant
        value_variant value_variant_deep_copy(const value_variant& other)
        {
            auto deep_copy = [](const auto& v) -> value_variant {
                if      constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, object_ptr>) {
                    return std::make_unique<object>(*v.get());
                }
                else if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, array_ptr>) {
                    return std::make_unique<array>(*v.get());
                }
                else {
                    return v;
                }
            };

            return std::visit(deep_copy, other);
        }

        // I'd use a concept here, but we can't forward declare an inner class
        // and I'd like to keep everything tucked into the class.
        template <typename T>
        struct type_json_value_type : std::bool_constant<
            std::is_same_v<std::remove_cv_t<T>, object_ptr> ||
            std::is_same_v<std::remove_cv_t<T>, array_ptr>  ||
            std::is_same_v<std::remove_cv_t<T>, string>     ||
            std::is_same_v<std::remove_cv_t<T>, number>     ||
            std::is_same_v<std::remove_cv_t<T>, integer>    ||
            std::is_same_v<std::remove_cv_t<T>, bool>       ||
            std::is_same_v<std::remove_cv_t<T>, nullptr_t>
        > {};

        template <typename T>
        static inline constexpr bool is_json_value_type = type_json_value_type<T>::value;

        /// Get value as lvalue reference
        template <typename T>
            requires (is_json_value_type<T>)
        constexpr T& get() & { return std::get<T>(*this); }

        /// Get value as const lvalue reference
        template <typename T>
            requires (is_json_value_type<T>)
        constexpr const T& get() const & { return std::get<T>(*this); }

        /// Get value as rvalue reference
        template <typename T>
            requires (is_json_value_type<T>)
        constexpr T&& get() && { return std::get<T>(std::move(*this)); }

        /// Get pointer to value if type matches or nullptr otherwise
        template <typename T>
            requires (is_json_value_type<T>)
        constexpr T* try_get() noexcept { return std::get_if<T>(this); }

        /// Get const pointer to value if type matches or nullptr otherwise
        template <typename T>
            requires (is_json_value_type<T>)
        constexpr const T* try_get() const noexcept { return std::get_if<T>(this); }
    };

    /// Make a JSON object
    static auto make_object()
    {
        return std::make_unique<object>();
    }

    /// Make a JSON array populated with zero or more values
    template <class... Values>
        requires (std::is_constructible_v<value, Values> && ...)
    static auto make_array(Values&&... values)
    {
        return std::make_unique<array>(
            std::initializer_list<value>{ std::forward<Values>(values)... });
    }

protected:

    // -- Parsing ----------------------------------------------------------

    // Defines a token type
    enum class TokenType {
        NotAToken, EndOfInput,
        ObjectStart, ObjectEnd, ArrayStart, ArrayEnd, Comma, Colon,
        String, Number, True, False, Null
    };

    // Extract the next token from the input stream
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr TokenType next_token(parse_ctx<InputIt, Stop>& p_ctx) const noexcept
    {
        // Try to match against a keyword token: "true", "false", or "null"
        const auto try_match = [&p_ctx](std::string_view word, TokenType t) noexcept
        {
            const auto* wit = word.begin();
            for (; !p_ctx.empty() && (wit != word.end()) && (*p_ctx.it_at == *wit); ++p_ctx.it_at, ++wit) {}
            if (wit == word.end()) {
                return t;
            }

            return TokenType::NotAToken;
        };

        // Bypass leading whitespace
        if (eat_whitespace(p_ctx)) {
            return TokenType::EndOfInput;
        }

        switch (*p_ctx.it_at) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        case '-': return TokenType::Number;
        case '"': ++p_ctx.it_at; return TokenType::String;
        case ':': ++p_ctx.it_at; return TokenType::Colon;
        case ',': ++p_ctx.it_at; return TokenType::Comma;
        case 't': return try_match("true",  TokenType::True);
        case 'f': return try_match("false", TokenType::False);
        case 'n': return try_match("null",  TokenType::Null);
        case '{': ++p_ctx.it_at; return TokenType::ObjectStart;
        case '}': ++p_ctx.it_at; return TokenType::ObjectEnd;
        case '[': ++p_ctx.it_at; return TokenType::ArrayStart;
        case ']': ++p_ctx.it_at; return TokenType::ArrayEnd;
        default : return TokenType::NotAToken;
        }
    }

    constexpr static inline bool is_whitespace(int ch) noexcept
    {
        return (ch == ' ') || (ch == '\n') || (ch == '\r') || (ch == '\t');
    }

    // Consume whitespace; returns true if we have no more input
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    constexpr inline bool eat_whitespace(parse_ctx<InputIt, Stop>& p_ctx) const noexcept
    {
        for (; !p_ctx.empty() && is_whitespace(*p_ctx.it_at); ++p_ctx.it_at) {}
        return p_ctx.empty();
    }

    /// Parse an explicit unicode point from JSON string: \uXXXX
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop, std::output_iterator<char> OutputIt>
    constexpr bool parse_unicode_point(parse_ctx<InputIt, Stop>& p_ctx, OutputIt&& out_it) const
    {
        const auto hex_char_value = [](char ch) -> std::optional<uint16_t> {
            if ((ch >= '0') && (ch <= '9')) { return  0 + (ch - '0'); }
            if ((ch >= 'a') && (ch <= 'f')) { return 10 + (ch - 'a'); }
            if ((ch >= 'A') && (ch <= 'F')) { return 10 + (ch - 'A'); }
            return std::nullopt;
        };

        auto& it = p_ctx.it_at;

        // Read in four hex digits
        uint16_t hex_val{};
        for (size_t i = 0; (it != p_ctx.it_end) && (i < 4); ++it,++i) {
            auto dig_val = hex_char_value(*it);
            if (!dig_val.has_value()) {
                p_ctx.err = json_error::invalid_hex_char;
                return false;
            }
            hex_val |= static_cast<uint16_t>(dig_val.value() << ((3 - i) << 2));
        }

        // Convert into UTF-8 byte(s) and emit. This can result in 1-3 bytes
        // of output depending on the hex value.
        if (hex_val <= 0x7F) {
            *out_it++ = static_cast<char>(hex_val);
        }
        else if (hex_val <= 0x7FF) {
            const unsigned b1 = 0b11000000 | (hex_val >> 6);
            const unsigned b2 = 0b10000000 | (hex_val & 0x3F);
            *out_it++ = static_cast<char>(b1);
            *out_it++ = static_cast<char>(b2);
        }
        else {
            const unsigned b1 = 0b11100000 |  (hex_val >> 12);
            const unsigned b2 = 0b10000000 | ((hex_val >>  6) & 0x3F);
            const unsigned b3 = 0b10000000 |  (hex_val        & 0x3F);
            *out_it++ = static_cast<char>(b1);
            *out_it++ = static_cast<char>(b2);
            *out_it++ = static_cast<char>(b3);
        }

        return true;
    }

    // Parse a string
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr string parse_string(parse_ctx<InputIt, Stop>& p_ctx) const
    {
        auto& it = p_ctx.it_at;
        string s{};
        char last_char{};

        for (bool advance = true; !p_ctx.empty(); advance ? ++it : it) {
            advance = true;

            if (*it == '\\') {
                if (last_char == '\\') {
                    s.append(1, '\\');
                    last_char = 0;
                }
                else {
                    last_char = *it;
                }

                continue;
            }
            if (last_char != '\\') {
                if (*it == '"') {
                    it++;
                    return s;
                }
                s.append(1, *it);
                last_char = *it;
                continue;
            }
            switch (*it) {
            case '"':
            case '/': s.append(1, *it);  break;
            case 'b': s.append(1, '\b'); break;
            case 'f': s.append(1, '\f'); break;
            case 'n': s.append(1, '\n'); break;
            case 'r': s.append(1, '\r'); break;
            case 't': s.append(1, '\t'); break;
            case 'u':   // Parse uXXXX -> U+XXXX
                ++it;
                if (!parse_unicode_point(p_ctx, std::back_inserter(s))) {
                    return s;
                }
                advance = false;
                break;
            default:
                p_ctx.err = json_error::unknown_control_sequence;
                return std::string{};
            }
            last_char = 0;
        }

        p_ctx.err = json_error::unterminated_string;
        return std::string{};
    }

    // Parse a number
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr value parse_number(parse_ctx<InputIt, Stop>& p_ctx) const
    {
        // We may be dealing with a single pass iterator and there's no
        // reliable upper bound on the length of the input string so we'll
        // need to buffer the data. Grab everything up to the next delimiter
        // and then decode the whole chunk.
        std::string num_buf;
        for (; p_ctx.it_at != p_ctx.it_end; ++p_ctx.it_at) {
            if (is_whitespace(*p_ctx.it_at)) {
                break;
            }
            if ((*p_ctx.it_at == ',') || (*p_ctx.it_at == '}') || (*p_ctx.it_at == ']')) {
                break;
            }
            num_buf.push_back(*p_ctx.it_at);
        }

        // std::from_chars wants const char* parameters, not iterators.
        // We need the pointer arithmetic here because last might be end()
        // which isn't dereferenceable.
        auto* first_char = num_buf.data();
        auto* last_char  = num_buf.data() + num_buf.size();

        if (p_ctx.policy.allow_integer_decode) {
            // If the number contains a decimal point or exponent, then it
            // cannot be an integer.
            if (num_buf.find_first_of(".eE") == std::string::npos) {
                integer n{};
                auto [ptr, ec] = std::from_chars(first_char, last_char, n);
                if (ec != std::errc{}) {
                    p_ctx.err = json_error::not_a_number;
                }
                return n;
            }
        }

        number n{};
        auto [ptr, ec] = std::from_chars(first_char, last_char, n);
        if (ec != std::errc{}) {
            p_ctx.err = json_error::not_a_number;
        }
        return n;
    }

    // Parse an object definition
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] object_ptr parse_object(parse_ctx<InputIt, Stop>& p_ctx) const
    {
        // The calling parser has already consumed the opening '{'

        auto ret = std::make_unique<object>();

        bool first_loop = true;

        while (true) {

            auto token = next_token(p_ctx);

            // This is only valid the first time through (empty object {})
            if (first_loop && (token == TokenType::ObjectEnd)) {
                break;
            }

            // Parse the string
            if (token != TokenType::String) {
                p_ctx.err = json_error::expected_object_name;
                break;
            }
            auto name = parse_string(p_ctx);
            if (!p_ctx) {
                break;
            }

            // Parse the ':'
            if (TokenType::Colon != next_token(p_ctx)) {
                p_ctx.err = json_error::expected_colon;
                break;
            }

            // Parse the value
            auto val = parse_value(p_ctx);
            if (!p_ctx) {
                break;
            }

            // Handle non-unique member name; e.g.: { "foo" : 1, "foo" : 2 }
            if (ret->contains(name)) {
                const auto dispo = p_ctx.policy.non_unique_member_name_disposition;
                if (dispo == decode_policy::NonUniqueDisposition::Throw)
                    throw std::system_error(make_error_code(json_error::nonunique_member_name));
                if (dispo == decode_policy::NonUniqueDisposition::Fail) {
                    p_ctx.err = json_error::nonunique_member_name;
                    break;
                }
                // Fall through and overwrite
            }

            // Insert the item into the map
            ret->insert_or_assign(std::move(name), std::move(val));

            // Next token must be ',' or '}'
            token = next_token(p_ctx);
            if (token == TokenType::ObjectEnd) {
                break;
            }
            if (token == TokenType::Comma) {
                first_loop = false;
                continue;
            }

            p_ctx.err = json_error::unexpected_token;
            break;
        }

        return ret;
    }

    // Parse an object definition
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] array_ptr parse_array(parse_ctx<InputIt, Stop>& p_ctx) const
    {
        // The calling parser has already consumed the opening '['

        auto ret = std::make_unique<array>();

        // First check for any empty array. This will simplify the general case below

        // Bypass leading whitespace
        if (eat_whitespace(p_ctx)) {
            p_ctx.err = json_error::unterminated_array;
            return ret;
        }
        if (*p_ctx.it_at == ']') {
            p_ctx.it_at++;
            return ret;
        }

        while (true) {

            // Parse the value
            auto val = parse_value(p_ctx);
            if (!p_ctx) {
                break;
            }

            // Append to the array
            ret->push_back(std::move(val));

            // Next token must be ',' or ']'
            const auto token = next_token(p_ctx);
            if (token == TokenType::ArrayEnd) {
                break;
            }
            if (token == TokenType::Comma) {
                continue;
            }

            p_ctx.err = json_error::unexpected_token;
            break;
        }

        return ret;
    }

    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr value parse_value(parse_ctx<InputIt, Stop>& p_ctx) const
    {
        switch (next_token(p_ctx)) {
        case TokenType::ObjectStart: return parse_object(p_ctx);
        case TokenType::ArrayStart:  return parse_array(p_ctx);
        case TokenType::String:      return parse_string(p_ctx);
        case TokenType::Number:      return parse_number(p_ctx);
        case TokenType::True:        return true;
        case TokenType::False:       return false;
        case TokenType::Null:        return nullptr;

        case TokenType::EndOfInput:
            p_ctx.err = json_error::unterminated_json;
            return {};
        case TokenType::NotAToken:
            p_ctx.err = json_error::not_a_token;
            return {};
        default:
            p_ctx.err = json_error::unexpected_token;
            return {};
        }
    }

private:

    value       _value{};
};

class json_category_impl : public std::error_category
{
public:

    [[nodiscard]] const char* name() const noexcept override
    {
        return "turner::json";
    }
    [[nodiscard]] std::string message(int ev) const override
    {
        switch (static_cast<json_error>(ev)) {
            case json_error::unexpected_token:
                return "Encountered an unexpected JSON token";
            case json_error::unterminated_array:
                return "Input stream ended while parsing an array";
            case json_error::unterminated_string:
                return "Input stream ended while parsing a string";
            case json_error::unterminated_json:
                return "Input stream ended while parsing JSON";
            case json_error::trailing_garbage:
                return "Non whitespace data found after parsing JSON data";
            case json_error::expected_object_name:
                return "Expected a JSON object name";
            case json_error::expected_colon:
                return "Expected a colon (:)";
            case json_error::unknown_control_sequence:
                return "Encountered an unknown control sequence (\\foo) in a string";
            case json_error::invalid_hex_char:
                return "Invalid Unicode escape";
            case json_error::number_out_of_range:
                return "JSON number is too large or too small";
            case json_error::not_a_token:
                return "Invalid JSON token";
            case json_error::not_a_number:
                return "Invalid JSON number";
            default:
                return "Unknown JSON error";
        }
    }
};

inline const std::error_category& json_category() noexcept
{
    const static json_category_impl instance{};
    return instance;
}

}   // end namespace turner

namespace std {
    template <>
    struct is_error_code_enum<turner::json_error> : public true_type {};
}

#endif // ifndef zzz_I_assure_you_that_json_turner_dot_h_has_been_included
