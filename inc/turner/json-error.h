/**
 * @file    json-error.h
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   std::error_code and friends support
 * @date    2023-12-11
 *
 * @copyright Copyright (c) 2023 Mike DeKoker
 *
 */
#ifndef zzz_json_turner_error_included
#define zzz_json_turner_error_included

#include <system_error>
#include <stdexcept>
#include <string>

namespace turner {

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

    // Encode errors: [200,300)
    invalid_json_value = 200,
    number_nan,
    number_inf,
    unknown_encoding_disposition
};

class json_category_impl : public std::error_category
{
public:

    const char* name() const noexcept override
    {
        return "turner::json";
    }
    std::string message(int ev) const override
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
    static json_category_impl instance{};
    return instance;
}

inline std::error_code make_error_code(json_error e) noexcept
{
    return std::error_code{static_cast<int>(e), json_category()};
}

inline std::error_condition make_error_condition(json_error e) noexcept
{
    return std::error_condition{static_cast<int>(e), json_category()};
}

}   // End namespace turner

namespace std {
    template <>
    struct is_error_code_enum<turner::json_error> : public true_type {};
}

#endif // ifndef zzz_json_turner_error_included
