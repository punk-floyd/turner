/**
 * @file    json-turner.h
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   C++20 UTF-8 JSON parsing library
 * @date    2023-11-30
 *
 * @copyright Copyright (c) 2023 Mike DeKoker
 *
 */
#ifndef zzz_I_assure_you_that_json_turner_dot_h_has_been_included
#define zzz_I_assure_you_that_json_turner_dot_h_has_been_included

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
#include <memory>
#include <map>

#include "json-turner-error.h"

namespace turner {

class json
{
public:

    // -- Construction

    /// Constructs a default object with no JSON data
    constexpr json() noexcept = default;

    // -- Parsing

    /// The result of a parse method
    template <std::input_iterator InputIt>
    struct ParseResult {

        constexpr ParseResult() = default;

        constexpr ParseResult(InputIt input_it, std::error_code code = std::error_code{})
            : it(input_it), ec(code)
        {}

        operator bool() const noexcept { return !static_cast<bool>(ec); }

        InputIt             it{};   ///< End of parsed data or error spot
        std::error_code     ec{};   ///< Offending error code, or 0
    };

    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    struct ParseCtx {

        constexpr ParseCtx(InputIt at, Stop end) noexcept
            : it_at(at), it_end(end)
        {}

        constexpr bool empty() const noexcept
            { return it_at == it_end; }

        constexpr explicit operator bool() const noexcept
            { return err == json_error{}; }

        InputIt     it_at;
        Stop        it_end;
        json_error  err{};
    };

    /**
     * @brief Parse JSON data in the given range
     *
     * @param first     The start of the range to parse
     * @param last      The end of the range to parse
     * @param greedy    If true, the method will attempt to parse the whole
     *  input range. If false, parsing will stop after successfully parsing
     *  the top level JSON value.
     *
     * @return Returns a ParseResult containing parse results; @see ParseResult
     */
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    constexpr auto parse(InputIt first, Stop last, bool greedy = true)
        -> ParseResult<InputIt>
    {
        ParseCtx p_ctx{first, last};
        _value = parse_value(p_ctx);

        if (p_ctx && greedy && !eat_whitespace(p_ctx))
            p_ctx.err = json_error::trailing_garbage;

        return ParseResult { p_ctx.it_at, make_error_code(p_ctx.err) };
    }

    /// Parse JSON data in the given range
    template <std::ranges::input_range R>
    constexpr auto parse(R&& r, bool greedy = true)
    {
        return parse(std::ranges::begin(r), std::ranges::end(r), greedy);
    }

    std::error_code parse_file(const std::string& pathname)
    {
        try {
            std::ifstream ifs(pathname);
            ifs.exceptions(ifs.failbit | ifs.badbit);
            auto if_begin = std::istreambuf_iterator<char>{ifs};
            auto if_end   = std::istreambuf_iterator<char>{std::default_sentinel};
            return parse(if_begin, if_end, true).ec;
        }
        catch (std::ios_base::failure& e) {
            return e.code();
        }
    }

    // -- Attributes

    /// Returns true if this object contains valid JSON data
    constexpr bool is_valid() const noexcept
    {
        return _value.is_valid();
    }

    class value;

    /// Access root value
    constexpr       value&  get_value()       &  noexcept { return _value; }
    constexpr const value&  get_value() const &  noexcept { return _value; }
    constexpr       value&& get_value()       && noexcept { return std::move(_value); }

    // -- JSON value types

    class object;
    class array;

    using object_ptr = std::unique_ptr<object>;
    using array_ptr = std::unique_ptr<array>;
    using string = std::string;
    using number = double;

    using value_variant = std::variant <
        std::monostate, // Invalid value; used for default construction
        object_ptr,     // { ... }
        array_ptr,      // [ ... ]
        string,         //  "..."
        number,         // 123.456
        bool,           // true/false
        nullptr_t       // null
    >;

    class value : private value_variant
    {
    public:

        // -- Construction

        constexpr value() noexcept = default;

        // Construct from a value (pass through)
        template <class Arg>
            requires (!std::is_same_v<Arg, value> && std::is_constructible_v<value_variant, Arg>)
        constexpr value(Arg&& arg) noexcept(std::is_nothrow_constructible_v<value_variant, Arg>)
            : value_variant(std::forward<Arg>(arg))
        {}

        // -- Observers

        constexpr bool is_valid() const noexcept { return index() > 0; }

        constexpr bool is_object() const noexcept { return is<object_ptr>(); }
        constexpr       object_ptr&  get_object()       &           { return get<object_ptr>(); }
        constexpr const object_ptr&  get_object() const &           { return get<object_ptr>(); }
        constexpr       object_ptr&& get_object()       &&          { return std::move(get<object_ptr>()); }
        constexpr       object_ptr*  get_object_if()       noexcept { return try_get<object_ptr>(); }
        constexpr const object_ptr*  get_object_if() const noexcept { return try_get<object_ptr>(); }

        constexpr bool is_array()  const noexcept { return is<array_ptr>(); }
        constexpr       array_ptr&   get_array()       &            { return get<array_ptr>(); }
        constexpr const array_ptr&   get_array() const &            { return get<array_ptr>(); }
        constexpr       array_ptr&&  get_array()       &&           { return std::move(get<array_ptr>()); }
        constexpr       array_ptr*   get_array_if()       noexcept  { return try_get<array_ptr>(); }
        constexpr const array_ptr*   get_array_if() const noexcept  { return try_get<array_ptr>(); }

        constexpr bool is_string() const noexcept { return is<string>(); }
        constexpr       string&      get_string()       &            { return get<string>(); }
        constexpr const string&      get_string() const &            { return get<string>(); }
        constexpr       string&&     get_string()      &&            { return std::move(get<string>()); }
        constexpr       string*      get_string_if()       noexcept  { return try_get<string>(); }
        constexpr const string*      get_string_if() const noexcept  { return try_get<string>(); }

        constexpr bool is_number() const noexcept { return is<number>(); }
        constexpr       number&      get_number()       &            { return get<number>(); }
        constexpr const number&      get_number() const &            { return get<number>(); }
        constexpr       number&&     get_number()       &&           { return std::move(get<number>()); }
        constexpr       number*      get_number_if()       noexcept  { return try_get<number>(); }
        constexpr const number*      get_number_if() const noexcept  { return try_get<number>(); }

        constexpr bool is_bool()   const noexcept { return is<bool>();  }
        constexpr       bool&        get_bool()       &              { return get<bool>(); }
        constexpr const bool&        get_bool() const &              { return get<bool>(); }
        constexpr       bool&&       get_bool()       &&             { return std::move(get<bool>()); }
        constexpr       bool*        get_bool_if()       noexcept    { return try_get<bool>(); }
        constexpr const bool*        get_bool_if() const noexcept    { return try_get<bool>(); }

        constexpr bool is_null()   const noexcept { return is<nullptr_t>(); }
        constexpr       nullptr_t&   get_null()       &              { return get<nullptr_t>(); }
        constexpr const nullptr_t&   get_null() const &              { return get<nullptr_t>(); }
        constexpr       nullptr_t&&  get_null()       &&             { return std::move(get<nullptr_t>()); }
        constexpr       nullptr_t*   get_null_if()       noexcept    { return try_get<nullptr_t>(); }
        constexpr const nullptr_t*   get_null_if() const noexcept    { return try_get<nullptr_t>(); }

        template <typename T>
        inline constexpr bool is() const noexcept
            { return std::holds_alternative<T>(*this); }

    private:

        // I'd use a concept here, but we can't forward declare an inner class
        // and I'd like to keep everything tucked into the class.
        template <typename T>
        struct type_json_value_type : std::bool_constant<
            std::is_same_v<std::remove_cv_t<T>, object_ptr> ||
            std::is_same_v<std::remove_cv_t<T>, array_ptr>  ||
            std::is_same_v<std::remove_cv_t<T>, string>     ||
            std::is_same_v<std::remove_cv_t<T>, number>     ||
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

    class object : public std::map<std::string, value, std::less<>> {};
    class array  : public std::vector<value> {};

protected:

    // Defines a token type
    enum class TokenType {
        NotAToken, EndOfInput,
        ObjectStart, ObjectEnd, ArrayStart, ArrayEnd, Comma, Colon,
        String, Number, True, False, Null
    };

    // Extract the next token from the input stream
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr TokenType next_token(ParseCtx<InputIt, Stop>& p_ctx) const noexcept
    {
        // Try to match against a keyword token: "true", "false", or "null"
        const auto try_match = [&p_ctx](std::string_view word, TokenType t) noexcept
        {
            auto wit = word.begin();
            for (; !p_ctx.empty() && (wit != word.end()) && (*p_ctx.it_at == *wit); ++p_ctx.it_at, ++wit);
            if (wit == word.end())
                return t;

            return TokenType::NotAToken;
        };

        // Bypass leading whitespace
        if (eat_whitespace(p_ctx))
            return TokenType::EndOfInput;

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
        }

        return TokenType::NotAToken;
    }

    constexpr static inline bool is_whitespace(int ch) noexcept
    {
        return (ch == ' ') || (ch == '\n') || (ch == '\r') || (ch == '\t') || (ch == 0);
    }

    // Consume whitespace; returns true if we have no more input
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    constexpr inline bool eat_whitespace(ParseCtx<InputIt, Stop>& p_ctx) const noexcept
    {
        for (; !p_ctx.empty() && is_whitespace(*p_ctx.it_at); ++p_ctx.it_at);
        return p_ctx.empty();
    }

    /// Parse an explicit unicode point from JSON string: \uXXXX
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop, std::output_iterator<char> OutputIt>
    constexpr bool parse_unicode_point(ParseCtx<InputIt, Stop>& p_ctx, OutputIt&& out_it) const
    {
        const auto hex_char_value = [](char ch) -> std::optional<uint16_t> {
            if ((ch >= '0') && (ch <= '9')) return  0 + (ch - '0');
            if ((ch >= 'a') && (ch <= 'f')) return 10 + (ch - 'a');
            if ((ch >= 'A') && (ch <= 'F')) return 10 + (ch - 'A');
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
        if (hex_val <= 0x7F)
            *out_it++ = static_cast<char>(hex_val);
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
    [[nodiscard]] constexpr string parse_string(ParseCtx<InputIt, Stop>& p_ctx) const
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
                else
                    last_char = *it;

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
            case 'u': { // Parse uXXXX -> U+XXXX
                ++it;
                if (!parse_unicode_point(p_ctx, std::back_inserter(s)))
                    return s;
                advance = false;
                break;
            }
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
    [[nodiscard]] constexpr number parse_number(ParseCtx<InputIt, Stop>& p_ctx) const
    {
        // We may be dealing with a single pass iterator and there's no
        // reliable upper bound on the length of the input string so we'll
        // need to buffer the data. Grab everything up to the next delimiter
        // and then parse the whole chunk.
        std::string num_buf;
        for (; p_ctx.it_at != p_ctx.it_end; ++p_ctx.it_at) {
            if (is_whitespace(*p_ctx.it_at))
                break;
            if ((*p_ctx.it_at == ',') || (*p_ctx.it_at == '}') || (*p_ctx.it_at == ']'))
                break;
            num_buf.push_back(*p_ctx.it_at);
        }

        // std::from_chars wants const char* parameters, not iterators.
        // We need the pointer arithmetic here because last might be end()
        // which isn't dereferenceable.
        auto first_char = num_buf.data();
        auto last_char  = num_buf.data() + num_buf.size();

        number n{};
        auto [ptr, ec] = std::from_chars(first_char, last_char, n);
        if (ec != std::errc{})
            p_ctx.err = json_error::not_a_number;

        return n;
    }

    // Parse an object definition
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr object_ptr parse_object(ParseCtx<InputIt, Stop>& p_ctx) const
    {
        // The calling parser has already consumed the opening '{'

        auto ret = std::make_unique<object>();

        bool first_loop = true;

        while (1) {

            auto token = next_token(p_ctx);

            // This is only valid the first time through (empty object {})
            if (first_loop && (token == TokenType::ObjectEnd))
                break;

            // Parse the string
            if (token != TokenType::String) {
                p_ctx.err = json_error::expected_object_name;
                break;
            }
            auto name = parse_string(p_ctx);
            if (!p_ctx)
                break;

            // Parse the ':'
            if (TokenType::Colon != next_token(p_ctx)) {
                p_ctx.err = json_error::expected_colon;
                break;
            }

            // Parse the value
            auto val = parse_value(p_ctx);
            if (!p_ctx)
                break;

            // Insert the item into the map
            ret->insert(make_pair(std::move(name), std::move(val)));

            // Next token must be ',' or '}'
            token = next_token(p_ctx);
            if (token == TokenType::ObjectEnd)
                break;
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
    [[nodiscard]] constexpr array_ptr parse_array(ParseCtx<InputIt, Stop>& p_ctx) const
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

        while (1) {

            // Parse the value
            auto val = parse_value(p_ctx);
            if (!p_ctx)
                break;

            // Append to the array
            ret->push_back(std::move(val));

            // Next token must be ',' or ']'
            const auto token = next_token(p_ctx);
            if (token == TokenType::ArrayEnd)
                break;
            if (token == TokenType::Comma)
                continue;

            p_ctx.err = json_error::unexpected_token;
            break;
        }

        return ret;
    }

    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr value parse_value(ParseCtx<InputIt, Stop>& p_ctx) const
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

}   // end namespace turner

#endif // ifndef zzz_I_assure_you_that_json_turner_dot_h_has_been_included
