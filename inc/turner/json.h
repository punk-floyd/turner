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
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
#ifndef zzz_I_assure_you_that_json_dot_h_has_been_included
#define zzz_I_assure_you_that_json_dot_h_has_been_included

#include <initializer_list>
#include <system_error>
#include <string_view>
#include <type_traits>
#include <charconv>
#include <concepts>
#include <optional>
#include <iterator>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
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

// A local C++20 implementation of C++23 std::expected
#include "expected.h"
#include "turner_version.h"     // Defines library version

namespace turner {

inline constexpr std::string_view lib_version_string = TURNER_VER_STR;
inline constexpr int lib_version_major     = TURNER_VER_MAJ;
inline constexpr int lib_version_minor     = TURNER_VER_MIN;
inline constexpr int lib_version_sub_minor = TURNER_VER_SUB_MIN;
inline constexpr int lib_version_package   = TURNER_VER_PKG;

// -- JSON error codes and support -----------------------------------------

/// turner::json specific error codes
enum class json_error {             // NOLINT(readability-enum-initial-value,performance-enum-size)
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
    number_invalid,
    unknown_encoding_disposition
};

// turner::json hooks into the std::error_code framework. The rest of the
// implementation details are way down below.

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

// -- JSON encoding and decoding policies ----------------------------------

/// JSON encoding policy: adjusts encoding behavior
struct encode_policy {

    enum class Disposition : std::uint8_t {
        Fail,           ///< Fail encoding operation
        Null,           ///< Replace offending item with a JSON null
        Throw           ///< Throw system_error (json_error)
    };

    constexpr encode_policy() noexcept = default;

    // Construct using a common disposition
    constexpr explicit encode_policy(Disposition d) noexcept
        : value_invalid(d), number_invalid(d)
    {}

    /// What to do when value is invalid (default constructed)
    Disposition value_invalid{Disposition::Fail};
    /// What to do when a number is invalid (NaN or infinity)
    Disposition number_invalid{Disposition::Fail};

    // MDTODO : bool escape_whitespace{false};
};

/// JSON decoding policy: adjusts decoding behavior
struct decode_policy {

    constexpr decode_policy() noexcept = default;

    enum class NonUniqueDisposition : std::uint8_t {
        Overwrite,      ///< Overwrite the existing value
        Fail,           ///< Fail encoding operation
        Throw           ///< Throw system_error (json_error)
    };

    /// If true, expect to parse the whole input range
    bool    greedy{true};

    /// Allow decode to parse apparent integers as integers
#ifdef TURNER_DEFAULT_ALLOW_INTEGER_DECODE
    bool    allow_integer_decode{true};
#else
    bool    allow_integer_decode{false};
#endif

    /// What to do if we encounter an object member name that's already been parsed
    NonUniqueDisposition non_unique_member_name_disposition{NonUniqueDisposition::Overwrite};
};

// -- JSON pretty print support types --------------------------------------

// The line break style defines how objects and arrays are printed
enum class print_linebreak_style : std::uint8_t {
    OneLine,        ///< Print all members/elements on one line
    MultiLine       ///< Print each member/element on a new line
};

namespace imp {

    /// Defines an overload set for using the overload pattern with std::visit
    template <class... Ts>
    struct Overload : Ts... {
        using Ts::operator() ...;
    };

    // Deduction guide
    template<class... Ts> Overload(Ts...) -> Overload<Ts...>;

    // -- Line breaks ------------------------------------------------------

    // A type defines a linebreak style if defines a json_linebreak_style
    // member (either static or non-static). The linebreak style determines
    // how to print objects and arrays.
    template <class T>
    concept line_break_spec = requires(const T& t)
    {
        {t.json_linebreak_style}  -> std::convertible_to<print_linebreak_style>;
    };

    template <class T>           struct has_line_spec    : public std::false_type {};
    template <line_break_spec T> struct has_line_spec<T> : public std::true_type  {};

    // -- Indentation ------------------------------------------------------

    // A type defines the JSON indent string if it defines a json_indent
    // member (either static or non-static) that can be converted to a
    // std::string_view. The indent string is the basic unit of indentation
    // when displaying multiline output for objects or arrays.
    template <class T>
    concept indent_spec = requires(const T& t)
    {
        {t.json_indent}  -> std::convertible_to<std::string_view>;
    };

    template <class T>       struct has_indent    : public std::false_type {};
    template <indent_spec T> struct has_indent<T> : public std::true_type  {};

    // -- Padding ----------------------------------------------------------

    // A type defines the JSON padding string if it defines a json_padding
    // member (either static or non-static) that can be converted to a
    // std::string_view. The padding string is used between JSON tokens on
    // the same line.
    template <class T>
    concept padding_spec = requires(const T& t)
    {
        {t.json_padding}  -> std::convertible_to<std::string_view>;
    };

    template <class T>       struct has_padding    : public std::false_type {};
    template <indent_spec T> struct has_padding<T> : public std::true_type  {};

    // -- Printer wrapper --------------------------------------------------

    template <class T>          struct print_wrap    { T& printer; };
    template <indent_spec T>    struct print_wrap<T> { T& printer; std::size_t indent{}; };

}   // namespace imp

template <class T>
inline constexpr bool printer_has_indent = imp::has_indent<T>::value;

template <class T>
inline constexpr bool printer_has_padding = imp::has_padding<T>::value;

template <class T>
inline constexpr bool printer_has_linebreak = imp::has_line_spec<T>::value;

// -- Without further ado, the json class ----------------------------------

/// JSON encoder/decoder
class json
{
public:

    using nullptr_t = std::nullptr_t;

    // -- Construction

    /// Constructs a default object with no JSON data
    json() noexcept = default;

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

    // -- Decoding (Parsing JSON into data)

    /// The result of a decode method
    template <std::input_iterator InputIt>
    struct decode_result {

        constexpr decode_result() = default;

        constexpr decode_result(InputIt input_it, json_error code = json_error{})
            : it(input_it), err(code)
        {}

        /// This object implicitly converts to bool: true means we're error free
        constexpr explicit operator bool() const noexcept { return err == json_error{}; }

        InputIt     it{};   ///< End of decoded data or error location
        json_error  err{};  ///< Offending error code, or 0
    };

    // -- Helper factory routines

    // Make json object from the given range
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] static
    auto parse_json(InputIt first, Stop last, decode_policy policy = decode_policy{})
        -> expected<json, decode_result<InputIt>>
    {
        expected<json, decode_result<InputIt>> ret_val;
        auto res = ret_val.value().decode(first, last, policy);
        if (res)
            return ret_val;

        return unexpected{res};
    }

    // Make json object from the given range
    template <std::ranges::input_range R>
    [[nodiscard]] static
    auto parse_json(R&& r, decode_policy policy = decode_policy{})
    {
        return parse_json(std::begin(r), std::end(r), policy);
    }

    // Make JSON object from the given NULL-terminated C string
    [[nodiscard]] static
    auto parse_json(const char* src, decode_policy policy = decode_policy{})
    {
        return parse_json(std::string_view{src}, policy);
    }

    // Make JSON object from the given input stream
    [[nodiscard]] static
    auto parse_json(std::istream& ifs, decode_policy policy = decode_policy{})
    {
        return parse_json(std::istreambuf_iterator<char>{ifs},
            std::istreambuf_iterator<char>{std::default_sentinel}, policy);
    }

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
     * @param policy    The decoding policy parameters
     *
     * @return Returns a decode_result containing results; @see decode_result
     */
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    auto decode(InputIt first, Stop last,
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
    auto decode(R&& r, decode_policy policy = decode_policy{})
    {
        return decode(begin(r), end(r), policy);
    }

    /// Parse from a C string
    auto decode(const char* src, decode_policy policy = decode_policy{})
    {
        return decode(std::string_view(src), policy);
    }

    /// Decode data from the given input stream
    auto decode_stream(std::istream& ifs, decode_policy policy = decode_policy{})
    {
        return decode(std::istreambuf_iterator<char>{ifs},
            std::istreambuf_iterator<char>{std::default_sentinel}, policy);
    }

    // -- Attributes

    class value;

    // -- JSON value types

    template <class MapType> class MapWrap;

    using array  = std::vector<value>;
    using string = std::string;
    using number = double;
    using integer = std::int64_t;       // Non-standard
    using object = MapWrap<std::map<std::string, value, std::less<>>>;

    /// A thin wrapper around a std::map that exposes a few helper accessor routines
    template <class MapType>
    class MapWrap : public MapType
    {
    public:

        // -- Construction

        // Inherit base class constructors
        using MapType::MapType;

        // -- Accessor helpers for JSON objects

        /**
         * @brief Obtain the string value associated with given member name
         *
         * @param name              The member name of the value to look up
         * @param default_value     The default value to use if the named
         *  item does not exist in our map. If this value is std::nullopt
         *  and the named item does not exist then an unexpected will be
         *  returned describing the error.
         *
         * @retval expected     A copy of the string value associated with
         *  the named member.
         * @retval unexpected   A string describing the error, which will be
         *  one of the following: this value is not a JSON object, the named
         *  member does not exist in the map and no default value was
         *  provided, or the named member exists in the map, but it is not a
         *  string type.
         */
        [[nodiscard]] auto get_member_string(std::string_view name,
            std::optional<std::string> default_value = std::nullopt) const
                -> turner::expected<std::string, std::string>
        {
            return get_member_imp(name, std::move(default_value), "string");
        }

        /**
         * @brief Obtain the number value associated with given member name
         *
         * @param name              The member name of the value to look up
         * @param default_value     The default value to use if the named
         *  item does not exist in our map. If this value is std::nullopt
         *  and the named item does not exist then an unexpected will be
         *  returned describing the error.
         *
         * @retval expected     A copy of the number value associated with
         *  the named member.
         * @retval unexpected   A string describing the error, which will be
         *  one of the following: this value is not a JSON object, the named
         *  member does not exist in the map and no default value was
         *  provided, or the named member exists in the map, but it is not a
         *  number type.
         */
        [[nodiscard]] auto get_member_number(std::string_view name,
            std::optional<number> default_value = std::nullopt) const
                -> turner::expected<number, std::string>
        {
#ifdef TURNER_DEFAULT_ALLOW_INTEGER_DECODE
            return get_member_numeric(name, default_value);
#else
            return get_member_imp(name, default_value, "number");
#endif
        }

        /**
         * @brief Obtain the integer value associated with given member name
         *
         * @param name              The member name of the value to look up
         * @param default_value     The default value to use if the named
         *  item does not exist in our map. If this value is std::nullopt
         *  and the named item does not exist then an unexpected will be
         *  returned describing the error.
         *
         * @retval expected     A copy of the integer value associated with
         *  the named member.
         * @retval unexpected   A string describing the error, which will be
         *  one of the following: this value is not a JSON object, the named
         *  member does not exist in the map and no default value was
         *  provided, or the named member exists in the map, but it is not
         *  an integer type.
         */
        [[nodiscard]] auto get_member_integer(std::string_view name,
            std::optional<integer> default_value = std::nullopt) const
                -> turner::expected<integer, std::string>
        {
            return get_member_imp(name, default_value, "integer");
        }

        /**
         * @brief Obtain the Boolean value associated with given member name
         *
         * @param name              The member name of the value to look up
         * @param default_value     The default value to use if the named
         *  item does not exist in our map. If this value is std::nullopt
         *  and the named item does not exist then an unexpected will be
         *  returned describing the error.
         *
         * @retval expected     A copy of the Boolean value associated with
         *  the named member.
         * @retval unexpected   A string describing the error, which will be
         *  one of the following: this value is not a JSON object, the named
         *  member does not exist in the map and no default value was
         *  provided, or the named member exists in the map, but it is not a
         *  Boolean type.
         */
        [[nodiscard]] auto get_member_bool(std::string_view name,
            std::optional<bool> default_value = std::nullopt) const
                -> turner::expected<bool, std::string>
        {
            return get_member_imp(name, default_value, "Boolean");
        }

        /**
         * @brief Obtain the null value associated with given member name
         *
         * This is not a very interesting method. It exists solely to be
         * consistent with the other types.
         *
         * @param name              The member name of the value to look up
         * @param default_value     The default value to use if the named
         *  item does not exist in our map. If this value is std::nullopt
         *  and the named item does not exist then an unexpected will be
         *  returned describing the error.
         *
         * @retval expected     A copy of the null value associated with
         *  the named member.
         * @retval unexpected   A string describing the error, which will be
         *  one of the following: this value is not a JSON object, the named
         *  member does not exist in the map and no default value was
         *  provided, or the named member exists in the map, but it is not a
         *  null type.
         */
        [[nodiscard]] auto get_member_null(std::string_view name,
            std::optional<nullptr_t> default_value = std::nullopt) const
                -> turner::expected<nullptr_t, std::string>
        {
            // Including this one only for consistency
            return get_member_imp(name, default_value, "null");
        }

        /**
         * @brief Obtain the object value associated with given member name
         *
         * @param name              The member name of the value to look up
         * @param default_value     The default value to use if the named
         *  item does not exist in our map. If this value is std::nullopt
         *  and the named item does not exist then an unexpected will be
         *  returned describing the error.
         *
         * @retval expected     A copy of the object value associated with
         *  the named member.
         * @retval unexpected   A string describing the error, which will be
         *  one of the following: this value is not a JSON object, the named
         *  member does not exist in the map and no default value was
         *  provided, or the named member exists in the map, but it is not
         *  an object type.
         */
        [[nodiscard]] auto get_member_object(std::string_view name,
            std::optional<MapWrap> default_value = std::nullopt) const
                -> turner::expected<MapWrap, std::string>
        {
            // Note this returns a *copy* of the object
            return get_member_imp(name, std::move(default_value), "object");
        }

        /**
         * @brief Obtain the array value associated with given member name
         *
         * @param name              The member name of the value to look up
         * @param default_value     The default value to use if the named
         *  item does not exist in our map. If this value is std::nullopt
         *  and the named item does not exist then an unexpected will be
         *  returned describing the error.
         *
         * @retval expected     A copy of the array value associated with
         *  the named member.
         * @retval unexpected   A string describing the error, which will be
         *  one of the following: this value is not a JSON object, the named
         *  member does not exist in the map and no default value was
         *  provided, or the named member exists in the map, but it is not
         *  an array type.
         */
        [[nodiscard]] auto get_member_array(std::string_view name,
            std::optional<array> default_value = std::nullopt) const
                -> turner::expected<array, std::string>
        {
            // Note this returns a *copy* of the array
            return get_member_imp(name, std::move(default_value), "array");
        }

        // -- Implementation

        using MapType::operator[];
        using MapType::operator=;

    private:

        template <class T>
        [[nodiscard]] auto get_member_imp(std::string_view name,
            std::optional<T> default_value, std::string_view type_name) const
                -> turner::expected<T, std::string>
        {
            // Look up the named member in our map
            const auto it = this->find(name);
            if ((it == this->cend()) && default_value)
                return default_value.value();   // Not in map, use default value

            if ((it == this->cend()) || (!it->second.template is<T>())) {
                // Named member is not in map, or is not of the expected type
                std::string msg{"Missing required "};
                msg.append(type_name);
                msg.append(" member: ");
                msg.append(name);
                return turner::unexpected{std::move(msg)};
            }

            return it->second.template get<T>();
        }

#ifdef TURNER_DEFAULT_ALLOW_INTEGER_DECODE
        [[nodiscard]]
        auto get_member_numeric(std::string_view name,
            std::optional<number> default_value) const
                -> turner::expected<number, std::string>
        {
            // Look up the named member in our map
            const auto it = this->find(name);
            if ((it == this->cend()) && default_value)
                return default_value.value();   // Not in map, use default value

            if ((it == this->cend()) || !it->second.is_numeric()) {
                // Named member is not in map, or is not of the expected type
                std::string msg{"Missing required numeric member: "};
                msg.append(name);
                return turner::unexpected{std::move(msg)};
            }

            return it->second.get_as_number();
        }
#endif
    };

    /// Access root value
    [[nodiscard]]       value&  get_value()       &  noexcept { return _value; }
    [[nodiscard]] const value&  get_value() const &  noexcept { return _value; }
    [[nodiscard]]       value&& get_value()       && noexcept { return std::move(_value); }
    [[nodiscard]] const value&& get_value() const && noexcept { return std::move(_value); } // NOLINT(performance-move-const-arg)

    using value_variant = std::variant <
        nullptr_t,      // null
        object,         // { ... }
        array,          // [ ... ]
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

        // Inherit base class constructors
        using value_variant::value_variant;

        // -- Observers

        [[nodiscard]] constexpr bool is_object() const noexcept { return is<object>(); }
        [[nodiscard]]       object&      get_object()       &           { return get<object>(); }
        [[nodiscard]] const object&      get_object() const &           { return get<object>(); }
        [[nodiscard]]       object&&     get_object()       &&          { return std::move(get<object>()); }
        [[nodiscard]] const object&&     get_object() const &&          { return std::move(get<object>()); }
        [[nodiscard]]       object*      get_object_if()       noexcept { return try_get<object>(); }
        [[nodiscard]] const object*      get_object_if() const noexcept { return try_get<object>(); }

        [[nodiscard]] constexpr bool     is_array()  const noexcept { return is<array>(); }
        [[nodiscard]]       array&       get_array()       &            { return get<array>(); }
        [[nodiscard]] const array&       get_array() const &            { return get<array>(); }
        [[nodiscard]]       array&&      get_array()       &&           { return std::move(get<array>()); }
        [[nodiscard]] const array&&      get_array() const &&           { return std::move(get<array>()); }
        [[nodiscard]]       array*       get_array_if()       noexcept  { return try_get<array>(); }
        [[nodiscard]] const array*       get_array_if() const noexcept  { return try_get<array>(); }

        [[nodiscard]] constexpr bool is_string() const noexcept { return is<string>(); }
        [[nodiscard]]       string&      get_string()       &            { return get<string>(); }
        [[nodiscard]] const string&      get_string() const &            { return get<string>(); }
        [[nodiscard]]       string&&     get_string()       &&           { return std::move(get<string>()); }
        [[nodiscard]] const string&&     get_string() const &&           { return std::move(get<string>()); }
        [[nodiscard]]       string*      get_string_if()       noexcept  { return try_get<string>(); }
        [[nodiscard]] const string*      get_string_if() const noexcept  { return try_get<string>(); }

        [[nodiscard]] constexpr bool is_number() const noexcept { return is<number>(); }
        [[nodiscard]]       number&      get_number()                    { return get<number>(); }
        [[nodiscard]] const number&      get_number() const              { return get<number>(); }
        [[nodiscard]]       number*      get_number_if()       noexcept  { return try_get<number>(); }
        [[nodiscard]] const number*      get_number_if() const noexcept  { return try_get<number>(); }

        [[nodiscard]] constexpr bool is_integer() const noexcept { return is<integer>(); }
        [[nodiscard]]       integer&     get_integer()                   { return get<integer>(); }
        [[nodiscard]] const integer&     get_integer() const             { return get<integer>(); }
        [[nodiscard]]       integer*     get_integer_if()       noexcept { return try_get<integer>(); }
        [[nodiscard]] const integer*     get_integer_if() const noexcept { return try_get<integer>(); }

        [[nodiscard]] constexpr bool is_bool()   const noexcept { return is<bool>();  }
        [[nodiscard]]       bool&        get_bool()                      { return get<bool>(); }
        [[nodiscard]] const bool&        get_bool() const                { return get<bool>(); }
        [[nodiscard]]       bool*        get_bool_if()       noexcept    { return try_get<bool>(); }
        [[nodiscard]] const bool*        get_bool_if() const noexcept    { return try_get<bool>(); }

        [[nodiscard]] constexpr bool is_null()   const noexcept { return is<nullptr_t>(); }
        [[nodiscard]]       nullptr_t&   get_null()                      { return get<nullptr_t>(); }
        [[nodiscard]] const nullptr_t&   get_null() const                { return get<nullptr_t>(); }
        [[nodiscard]]       nullptr_t*   get_null_if()       noexcept    { return try_get<nullptr_t>(); }
        [[nodiscard]] const nullptr_t*   get_null_if() const noexcept    { return try_get<nullptr_t>(); }

#ifdef TURNER_DEFAULT_ALLOW_INTEGER_DECODE

        // When TURNER_DEFAULT_ALLOW_INTEGER_DECODE is enabled, numbers
        // without decimal points or exponents will be parsed as integers.
        // For those wanting to parse integers, this is great. However, this
        // does introduce a potential wrinkle for code that expects numbers.
        // For example, suppose a particular value represents a temperature
        // value. In most cases it's going to be some number-looking thing
        // that will be parsed as a number (e.g., 25.125, 45.625, etc.),
        // but sometimes the temperature will be (close to) a whole degree
        // (e.g., 25.00001) and depending on how the encoder implements
        // things (e.g., will it emit "25" or "25.0"?) it could get picked
        // up as a number or as an integer. This could be problematic for
        // code that is expecting a particular field to be a number. These
        // routines are intended to mitigate that problem.
        //
        // Note that integer decoding is a non-standard feature that one
        // needs to opt-in for, so none of this stuff applies to folks who
        // only deal with standard JSON numbers.

        /// Returns true if this value is either an integer or a number
        [[nodiscard]] constexpr bool is_numeric() const noexcept
            { return is_number() || is_integer(); }

        /// Returns this numeric value as a JSON number
        [[nodiscard]] number get_as_number() const
        {
            if (is_integer())
                return static_cast<number>(get_integer());

            return get_number();
        }
#endif

        template <typename T>
        [[nodiscard]] constexpr bool is() const noexcept
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
            constexpr explicit operator bool() const noexcept { return err == json_error{}; }

            OutputIt    it{};   ///< End of encoded data
            json_error  err{};  ///< Offending error code, or 0
        };

        // A default PrintPolicy that does nothing (no pretty printing)
        template <class CharT>
        struct NoPrettyPrint {};

        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        struct encode_context {
            OutputIt                            it;
            encode_policy                       policy;
            imp::print_wrap<PrintPolicy<char>>  print;
        };

        /// JSON encode value to an output iterator
        template <std::output_iterator<char> OutputIt,
            template <typename> class PrintPolicy = NoPrettyPrint>
        [[nodiscard]] auto encode(OutputIt it,
            encode_policy policy = encode_policy{}, PrintPolicy<char> print = {}) const
            -> encode_result<OutputIt>
        {
            encode_context<OutputIt, PrintPolicy> ctx {
                .it     = it,
                .policy = policy,
                .print  = {print}
            };

            return encode_value(*this, ctx);
        }

        /// JSON encode value to a string
        template <template <typename> class PrintPolicy = NoPrettyPrint>
        [[nodiscard]] std::string encode(encode_policy policy = encode_policy{},
            PrintPolicy<char> print = {}) const
        {
            std::string s;
            const auto res = encode(std::back_inserter(s), policy, print);
            if (!res) {
                return std::string{};
            }

            return s;
        }

        // -- Encoding implementation --------------------------------------

        /// Handle an encoding error based on specified policy
        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        static constexpr auto encode_error (json_error ec,
            encode_context<OutputIt, PrintPolicy>& ctx, encode_policy::Disposition d) -> encode_result<OutputIt>
        {
            using Disposition = encode_policy::Disposition;

            switch (d) {
            case Disposition::Null: return encode_literal("null", ctx);
            case Disposition::Fail: return encode_result{ctx.it, ec};
            case Disposition::Throw: throw std::system_error(make_error_code(ec));
            }

            return encode_result{ctx.it, json_error::unknown_encoding_disposition};
        }

        /// JSON-encode the given value to the given output iterator
        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        static auto encode_value (const value& val,
            encode_context<OutputIt, PrintPolicy>& ctx) -> encode_result<OutputIt>
        {
            return std::visit(imp::Overload {
                [&ctx](const object& v) { return encode_object(v, ctx);  },
                [&ctx](const array&  v) { return encode_array(v, ctx);   },
                [&ctx](const string& v) { return encode_string(v, ctx);  },
                [&ctx](number        v) { return encode_number(v, ctx);  },
                [&ctx](integer       v) { return encode_integer(v, ctx); },
                [&ctx](bool          v) { return encode_literal(v ? "true" : "false", ctx); },
                [&ctx](nullptr_t     v) { (void)v; return encode_literal("null", ctx); }
            }, static_cast<const value_variant&>(val));
        }

        /// JSON-encode the given object to the given output iterator
        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        static auto encode_object(const object& val,
            encode_context<OutputIt, PrintPolicy>& ctx) -> encode_result<OutputIt>
        {
            *ctx.it++ = '{';

/*
            // -- MOOMOO; goofing around with this
            if constexpr (printer_has_linebreak<PrintPolicy<char>>) {
                *ctx.it++ = '\n';
            }
            else if constexpr(printer_has_padding<PrintPolicy<char>>) {
                ctx.it = encode_literal(ctx.print.printer.json_padding, ctx);
            }
            // --
*/

            for (bool first = true; const auto& [mem_key,mem_val] : val) {
                if (first) {
                    first = false;
                }
                else {
                    *ctx.it++ = ',';
                }

                auto encode_result = encode_string(mem_key, ctx);
                if (!encode_result) {
                    return encode_result;
                }
                ctx.it = encode_result.it;

                *ctx.it++ = ':';
                encode_result = encode_value(mem_val, ctx);
                if (!encode_result) {
                    return encode_result;
                }
                ctx.it = encode_result.it;
            }

            *ctx.it++ = '}';

            return ctx.it;
        }

        /// JSON-encode the given array to the given output iterator
        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        static constexpr auto encode_array(const array& val,
            encode_context<OutputIt, PrintPolicy>& ctx) -> encode_result<OutputIt>
        {
            *ctx.it++ = '[';

            for (bool first = true; const auto& e : val) {
                if (first) {
                    first = false;
                }
                else {
                    *ctx.it++ = ',';
                }

                auto encode_result = encode_value(e, ctx);
                if (!encode_result) {
                    return encode_result;
                }
                ctx.it = encode_result.it;
            }

            *ctx.it++ = ']';

            return ctx.it;
        }

        /// JSON-encode the given string to the given output iterator
        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        static constexpr auto encode_string(const string& val,
            encode_context<OutputIt, PrintPolicy>& ctx) -> encode_result<OutputIt>
        {
            *ctx.it++ = '"';

            for (const auto& c : val) {

                // Must escape double quote -> \" <-
                if (c == '"') {
                    *ctx.it++ = '\\';
                    *ctx.it++ = '"';
                }
                // Must escape reverse solidus -> \\ <-
                else if (c == '\\') {
                    *ctx.it++ = '\\';
                    *ctx.it++ = '\\';
                }
                // Must escape control codes -> \u00XX <-
                else if ((c >= 0) && (c <= 0x1F)) {
                    *ctx.it++ = '\\';
                    *ctx.it++ = 'u';
                    *ctx.it++ = '0';
                    *ctx.it++ = '0';
                    *ctx.it++ = (c & 0x10) ? '1' : '0';
                    const auto nibble = c & 0xF;
                    *ctx.it++ = (nibble < 10)
                        ? static_cast<char>('0' + nibble)
                        : static_cast<char>('A' + nibble - 10);
                }
                // Everything else is passed thru
                else {
                    *ctx.it++ = c;
                }
            }

            *ctx.it++ = '"';

            return ctx.it;
        }

        /// JSON-encode the given number to the given output iterator
        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        static constexpr auto encode_number(const number& val,
            encode_context<OutputIt, PrintPolicy>& ctx) -> encode_result<OutputIt>
        {
            if (std::isnan(val) || std::isinf(val)) {
                return encode_error(json_error::number_invalid, ctx, ctx.policy.number_invalid);
            }

            constexpr auto buf_len = std::numeric_limits<double>::max_digits10 + 8;
            std::array<char, buf_len> buf;
#ifdef __cpp_lib_to_chars
            auto* first = buf.data();
            auto* last  = buf.data() + buf.size();
            const auto [ptr, ec] = std::to_chars(first, last, val);
            for (const auto* p = first; p != ptr; *ctx.it++ = *p++) {}
#else
            std::array<char, buf_len> buf2;
            const auto len1 = std::snprintf(buf.data(),  buf_len, "%f", val);
            const auto len2 = std::snprintf(buf2.data(), buf_len, "%g", val);
            if (len2 < len1)
                for (int i = 0; i<len2; ++i) *ctx.it++ = buf2[static_cast<unsigned>(i)];
            else
                for (int i = 0; i<len1; ++i) *ctx.it++ = buf [static_cast<unsigned>(i)];
#endif

            return ctx.it;
        }

        /// JSON-encode the given integer to the given output iterator
        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        static constexpr auto encode_integer(const integer& val,
            encode_context<OutputIt, PrintPolicy>& ctx) -> encode_result<OutputIt>
        {
            constexpr auto buf_len = std::numeric_limits<integer>::digits10 + 2;

            std::array<char, buf_len> buf;
            auto* first = buf.data();
            auto* last  = buf.data() + buf.size();
            const auto [ptr, ec] = std::to_chars(first, last, val);
            for (const auto* p = first; p != ptr; *ctx.it++ = *p++) {}

            return ctx.it;
        }

        /// Output the given literal string to the given output iterator
        template <std::output_iterator<char> OutputIt, template <typename> class PrintPolicy>
        static auto encode_literal(std::string_view literal,
            encode_context<OutputIt, PrintPolicy>& ctx) -> encode_result<OutputIt>
        {
            // This guy is used to "encode" true, false, and null

            for (auto c : literal) {
                *ctx.it++ = c;
            }

            return ctx.it;
        }

        // -- Implementation

        using value_variant::operator=;

        // I'd use a concept here, but we can't forward declare an inner class
        // and I'd like to keep everything tucked into the class.
        template <typename T>
        struct type_json_value_type : std::bool_constant<
            std::is_same_v<std::remove_cv_t<T>, object>     ||
            std::is_same_v<std::remove_cv_t<T>, array>      ||
            std::is_same_v<std::remove_cv_t<T>, string>     ||
            std::is_same_v<std::remove_cv_t<T>, number>     ||
            std::is_same_v<std::remove_cv_t<T>, integer>    ||
            std::is_same_v<std::remove_cv_t<T>, bool>       ||
            std::is_same_v<std::remove_cv_t<T>, nullptr_t>
        > {};

        template <typename T>
        static constexpr bool is_json_value_type = type_json_value_type<T>::value;

        /// Get value as lvalue reference
        template <typename T>
            requires (is_json_value_type<T>)
        [[nodiscard]] constexpr T& get() &
        { return std::get<T>(*this); }

        /// Get value as const lvalue reference
        template <typename T>
            requires (is_json_value_type<T>)
        [[nodiscard]] constexpr const T& get() const &
        { return std::get<T>(*this); }

        /// Get value as rvalue reference
        template <typename T>
            requires (is_json_value_type<T>)
        [[nodiscard]] constexpr T&& get() &&
        { return std::get<T>(std::move(*this)); }

        /// Get value as const rvalue reference
        template <typename T>
            requires (is_json_value_type<T>)
        [[nodiscard]] constexpr const T&& get() const &&
        { return std::get<T>(std::move(*this)); }

        /// Get pointer to value if type matches or nullptr otherwise
        template <typename T>
            requires (is_json_value_type<T>)
        [[nodiscard]] constexpr T* try_get() noexcept
        { return std::get_if<T>(this); }

        /// Get const pointer to value if type matches or nullptr otherwise
        template <typename T>
            requires (is_json_value_type<T>)
        [[nodiscard]] constexpr const T* try_get() const noexcept
        { return std::get_if<T>(this); }
    };

    /// Make a JSON array populated with zero or more values
    template <class... Values>
        requires (std::is_constructible_v<value, Values> && ...)
    static auto make_array(Values&&... values)
    {
        return json::array(std::initializer_list<value>{ std::forward<Values>(values)... });
    }

protected:

    // -- Parsing ----------------------------------------------------------

    // Defines a token type
    enum class TokenType : std::uint8_t {
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
            auto wit = word.cbegin(); // NOLINT(readability-qualified-auto)
            for (; !p_ctx.empty() && (wit != word.cend()) && (*p_ctx.it_at == *wit); ++p_ctx.it_at, ++wit) {}
            if (wit == word.cend()) {
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

    constexpr static bool is_whitespace(int ch) noexcept
    {
        return (ch == ' ') || (ch == '\n') || (ch == '\r') || (ch == '\t');
    }

    // Consume whitespace; returns true if we have no more input
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    bool eat_whitespace(parse_ctx<InputIt, Stop>& p_ctx) const noexcept
    {
        for (; !p_ctx.empty() && is_whitespace(*p_ctx.it_at); ++p_ctx.it_at) {}
        return p_ctx.empty();
    }

    /// Parse an explicit unicode point from JSON string: \uXXXX
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop, std::output_iterator<char> OutputIt>
    constexpr bool parse_unicode_point(parse_ctx<InputIt, Stop>& p_ctx, OutputIt&& out_it) const
    {
        const auto hex_char_value = [](char ch) -> std::optional<int> {
            if ((ch >= '0') && (ch <= '9')) { return  0 + (ch - '0'); }
            if ((ch >= 'a') && (ch <= 'f')) { return 10 + (ch - 'a'); }
            if ((ch >= 'A') && (ch <= 'F')) { return 10 + (ch - 'A'); }
            return std::nullopt;
        };

        auto& it = p_ctx.it_at;

        // Read in four hex digits
        uint16_t hex_val{};
        for (std::size_t i = 0; (it != p_ctx.it_end) && (i < 4); ++it,++i) {
            auto dig_val = hex_char_value(*it);
            if (!dig_val.has_value()) {
                p_ctx.err = json_error::invalid_hex_char;
                return false;
            }
            hex_val |= static_cast<uint16_t>(dig_val.value() << ((3 - i) << 2));
        }

        // Convert into UTF-8 byte(s) and emit. This can result in 1-3 bytes
        // of output depending on the hex value.
        if (hex_val <= 0x7F) {  // NOLINT(readability-magic-numbers)
            *out_it++ = static_cast<char>(hex_val);
        }
        else if (hex_val <= 0x7FF) {    // NOLINT(readability-magic-numbers)
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
        const auto* first_char = num_buf.data();
        const auto* last_char  = num_buf.data() + num_buf.size();

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

#ifdef __cpp_lib_to_chars
        number n{};
        auto [ptr, ec] = std::from_chars(first_char, last_char, n);
        if (ec != std::errc{}) {
            p_ctx.err = json_error::not_a_number;
        }
        return n;
#else
        char* endp;
        number n = std::strtod(first_char, &endp);
        if (n == HUGE_VAL) {
            p_ctx.err = json_error::number_out_of_range;
        }
        else if (*endp != '\0') {
            p_ctx.err = json_error::not_a_number;
        }
        return n;
#endif
    }

    // Parse an object definition
    template <std::input_iterator InputIt, std::sentinel_for<InputIt> Stop>
    [[nodiscard]] object parse_object(parse_ctx<InputIt, Stop>& p_ctx) const
    {
        // The calling parser has already consumed the opening '{'

        object ret;

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
            if (ret.contains(name)) {
                const auto disposition = p_ctx.policy.non_unique_member_name_disposition;
                if (disposition == decode_policy::NonUniqueDisposition::Throw)
                    throw std::system_error(make_error_code(json_error::nonunique_member_name));
                if (disposition == decode_policy::NonUniqueDisposition::Fail) {
                    p_ctx.err = json_error::nonunique_member_name;
                    break;
                }
                // Fall through and overwrite
            }

            // Insert the item into the map
            ret.insert_or_assign(std::move(name), std::move(val));

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
    [[nodiscard]] array parse_array(parse_ctx<InputIt, Stop>& p_ctx) const
    {
        // The calling parser has already consumed the opening '['

        array ret;

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
            ret.push_back(std::move(val));

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

    value       _value;
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

#endif // ifndef zzz_I_assure_you_that_json_dot_h_has_been_included
