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

        explicit operator bool() const noexcept { return !error_str; }

        /// Points to location just after the end of the parsed data
        InputIt                     it;
        /// Description of parse error, or std::nullopt
        std::optional<std::string>  error_str;
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
    constexpr auto parse(InputIt first, Stop last, bool greedy = true) -> ParseResult<InputIt>
    {
        auto [val, parse_res] = parse_value(first, last);
        _value = std::move(val);

        if (parse_res && greedy) {
            parse_res.it = eat_whitespace(parse_res.it, last);
            if (parse_res.it != last)
                parse_res.error_str = "Trailing garbage";
        }

        return parse_res;
    }

    /// Parse JSON data in the given range
    template <std::ranges::input_range R>
    constexpr auto parse(R&& r, bool greedy = true)
    {
        return parse(std::ranges::begin(r), std::ranges::end(r), greedy);
    }

#if 1
    std::error_code parse_file(const std::string& pathname)
    {
        try {
            std::ifstream ifs(pathname);
            ifs.exceptions(ifs.failbit | ifs.badbit);
            auto if_begin = std::istreambuf_iterator<char>{ifs};
            auto if_end   = std::istreambuf_iterator<char>{std::default_sentinel};
            auto parse_res = parse(if_begin, if_end, true);
            if (!parse_res)
                return std::make_error_code(std::errc::argument_out_of_domain);   // MOOMOO fixme
        }
        catch (std::ios_base::failure& e) {
            return e.code();
        }

        return {};
    }
#endif

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

    template <std::input_iterator InputIt>
    struct TokenResult {
        InputIt     it;
        TokenType   tok;
    };

    // Extract the next token from the input stream
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr static auto next_token(InputIt first, Stop last) -> TokenResult<InputIt>
    {
        // Try to match against a keyword token: "true", "false", or "null"
        const auto try_match = [](InputIt it, Stop tm_last, std::string_view word, TokenType t)
        {
            auto wit = word.begin();
            auto cit = it;
            for (; (cit != tm_last) && (wit != word.end()) && (*cit == *wit); ++cit, ++wit);
            if (wit == word.end())
                return TokenResult{ cit, t };

            return TokenResult{ it, TokenType::NotAToken };
        };

        // Bypass leading whitespace
        auto it = eat_whitespace(first, last);
        if (it == last)
            return { last, TokenType::EndOfInput };

        switch (*it) {
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
        case '-': return {   it, TokenType::Number };
        case '"': return { ++it, TokenType::String };
        case ':': return { ++it, TokenType::Colon };
        case ',': return { ++it, TokenType::Comma };
        case 't': return try_match(it, last, "true",  TokenType::True);
        case 'f': return try_match(it, last, "false", TokenType::False);
        case 'n': return try_match(it, last, "null",  TokenType::Null);
        case '{': return { ++it, TokenType::ObjectStart };
        case '}': return { ++it, TokenType::ObjectEnd };
        case '[': return { ++it, TokenType::ArrayStart };
        case ']': return { ++it, TokenType::ArrayEnd };
        }

        return { it, TokenType::NotAToken };
    }

    constexpr static inline bool is_whitespace(int ch) noexcept
    {
        return (ch == ' ') || (ch == '\n') || (ch == '\r') || (ch == '\t') || (ch == 0);
    }

    // Consume whitespace
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr static inline InputIt eat_whitespace(InputIt first, Stop last) noexcept
    {
        auto it = first;
        for (; (it != last) && is_whitespace(*it); ++it);
        return it;
    }

    template <typename T, std::input_iterator InputIt>
    using InternalParseResult = std::pair<T, ParseResult<InputIt>>;

    /// Parse an explicit unicode point from JSON string: \uXXXX
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop, std::output_iterator<char> OutputIt>
    constexpr static auto parse_unicode_point(InputIt first, Stop last, OutputIt&& out_it)
        -> ParseResult<InputIt>
    {
        const auto hex_char_value = [](char ch) -> std::optional<uint16_t> {
            if ((ch >= '0') && (ch <= '9')) return  0 + (ch - '0');
            if ((ch >= 'a') && (ch <= 'f')) return 10 + (ch - 'a');
            if ((ch >= 'A') && (ch <= 'F')) return 10 + (ch - 'A');
            return std::nullopt;
        };

        auto it = first;

        // Read in four hex digits
        uint16_t hex_val{};
        for (size_t i = 0; (it != last) && (i < 4); ++it,++i) {
            auto dig_val = hex_char_value(*it);
            if (!dig_val.has_value())
                return { it, "Invalid hex char" };
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

        return { it, std::nullopt };
    }

    // Parse a string
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]]  static auto parse_string(InputIt first, Stop last)
        -> InternalParseResult<string, InputIt>
    {
        auto it = first;
        string s{};
        char last_char{};

        for (; it != last; ++it) {
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
                if (*it == '"')
                    return { std::move(s), { ++it, std::nullopt } };
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
                auto res = parse_unicode_point(++it, last, std::back_inserter(s));
                if (res.error_str)
                    return std::make_pair(s, res);
                std::advance(it, 4 - 1);    // -1 because of this loop (++it) MOOMOO FIXME (single pass iterator)
                break;
            }
            default:
                return { std::string{}, { it, "Unknown control sequence"} };
            }
            last_char = 0;

        }

        return { std::string{}, { last, std::string{"Unterminated string"} } };
    }

    // Parse a number
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr static auto parse_number(InputIt first, Stop last)
        -> InternalParseResult<number, InputIt>
    {
        // We may be dealing with a single pass iterator and there's no
        // reliable upper bound on the length of the input string so we'll
        // need to buffer the data. Grab everything up to the next delimiter
        // and then parse the whole chunk.
        std::string num_buf;
        auto it_end = first;
        for (; it_end != last; ++it_end) {
            if (is_whitespace(*it_end))
                break;
            if ((*it_end == ',') || (*it_end == '}') || (*it_end == ']'))
                break;
            num_buf.push_back(*it_end);
        }

        // std::from_chars wants const char* parameters, not iterators.
        // We need the pointer arithmetic here because last might be end()
        // which isn't dereferenceable.
        const char* first_char = num_buf.data();
        const char* last_char =  num_buf.data() + num_buf.size();

        number n{};
        auto [ptr, ec] = std::from_chars(first_char, last_char, n);
        if (ec == std::errc{})
            return { n, { it_end, std::nullopt } };

        const auto msg = (ec == std::errc::result_out_of_range)
            ? "Number is out of range" : "Not a number";
        return { number{}, { it_end, msg } };
    }

    // Parse an object definition
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr static auto parse_object(InputIt first, Stop last)
        -> InternalParseResult<object_ptr, InputIt>
    {
        // The calling parser has already consumed the opening '{'

        InternalParseResult<object_ptr, InputIt> ret{};
        ret.first = std::make_unique<object>();

        auto it = first;
        bool first_loop = true;

        while (1) {

            auto tok_res = next_token(it, last);

            // This is only valid the first time through (empty object {})
            if (first_loop && (tok_res.tok == TokenType::ObjectEnd)) {
                ret.second.it = tok_res.it;
                break;
            }

            // Parse the string
            if (tok_res.tok != TokenType::String) {
                ret.second.it        = tok_res.it;
                ret.second.error_str = "Expected object name";
                break;
            }
            auto [name, name_res] = parse_string(tok_res.it, last);
            if (name_res.error_str.has_value()) {
                ret.second = std::move(name_res);
                break;
            }

            // Parse the ':'
            tok_res = next_token(name_res.it, last);
            if (tok_res.tok != TokenType::Colon) {
                ret.second.it        = tok_res.it;
                ret.second.error_str = "Expected ':'";
                break;
            }

            // Parse the value
            auto [val, val_res] = parse_value(tok_res.it, last);
            if (val_res.error_str.has_value()) {
                ret.second = std::move(val_res);
                break;
            }

            // Insert the item into the map
            ret.first->insert(make_pair(std::move(name), std::move(val)));

            // Next token must be ',' or '}'
            tok_res = next_token(val_res.it, last);
            if (tok_res.tok == TokenType::ObjectEnd) {
                ret.second.it = tok_res.it;
                break;
            }
            if (tok_res.tok == TokenType::Comma) {
                first_loop = false;
                it = tok_res.it;
                continue;
            }

            ret.second.it        = tok_res.it;
            ret.second.error_str = "Unexpected token";
            break;
        }

        return ret;
    }

    // Parse an object definition
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr static auto parse_array(InputIt first, Stop last)
        -> InternalParseResult<array_ptr, InputIt>
    {
        // The calling parser has already consumed the opening '['

        InternalParseResult<array_ptr, InputIt> ret{};
        ret.first = std::make_unique<array>();

        // First check for any empty array. This will simplify the general case below

        // Bypass leading whitespace
        auto it = eat_whitespace(first, last);
        if (it == last) {
            ret.second.it        = last;
            ret.second.error_str = "Unterminated array";
            return ret;
        }
        if (*it == ']') {
            ret.second.it = ++it;
            return ret;
        }

        while (1) {

            // Parse the value
            auto [val, val_res] = parse_value(it, last);
            if (val_res.error_str.has_value()) {
                ret.second = std::move(val_res);
                break;
            }

            // Append to the array
            ret.first->push_back(std::move(val));

            // Next token must be ',' or ']'
            const auto tok_res = next_token(val_res.it, last);
            if (tok_res.tok == TokenType::ArrayEnd) {
                ret.second.it = tok_res.it;
                break;
            }
            if (tok_res.tok == TokenType::Comma) {
                it = tok_res.it;
                continue;
            }

            ret.second.it = tok_res.it;
            ret.second.error_str = "Unexpected token";
            break;
        }

        return ret;
    }

    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] constexpr static auto parse_value(InputIt first, Stop last)
        -> InternalParseResult<value, InputIt>
    {
        switch (auto [it, tok_type] = next_token(first, last); tok_type) {

        case TokenType::ObjectStart: {
            auto [obj_ptr, parse_res] = parse_object(it, last);
            return std::make_pair(std::move(obj_ptr), parse_res);
        }
        case TokenType::ArrayStart: {
            auto [arr_ptr, parse_res] = parse_array(it, last);
            return std::make_pair(std::move(arr_ptr), parse_res);
        }
        case TokenType::String: {
            auto [str, parse_res] = parse_string(it, last);
            return std::make_pair(std::move(str), parse_res);
        }
        case TokenType::Number: {
            auto [num, parse_res] = parse_number(it, last);
            return std::make_pair(num, parse_res);
        }

        case TokenType::True:
        case TokenType::False:
            return std::make_pair(tok_type == TokenType::True, ParseResult{ it, std::nullopt });
        case TokenType::Null:
            return std::make_pair(nullptr, ParseResult{ it, std::nullopt });

        case TokenType::EndOfInput:
            return std::make_pair(value{}, ParseResult{ it, std::string{ "No data" } });
        case TokenType::NotAToken:
            return std::make_pair(value{}, ParseResult{ it, std::string{ "Invalid JSON data" } });
        default:
            return std::make_pair(value{}, ParseResult{ it, std::string{ "Unexpected token" } });
        }
    }

private:

    value       _value{};
};

}   // end namespace turner

#endif // ifndef zzz_I_assure_you_that_json_turner_dot_h_has_been_included
