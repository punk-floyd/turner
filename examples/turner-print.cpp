/**
 * @file    turner-print.cpp
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   JSON pretty/ugly printer
 * @date    2023-12-07
 *
 * @copyright Copyright (c) 2023 Mike DeKoker
 *
 */
#include <string_view>
#include <iostream>
#include <turner/json.h>

using namespace turner;

static constexpr auto test_src =
    //R"|({"person":{"name":"Alice","age":25,"address":{"city":"San Francisco","zip":"94105"}},"occupation":"Software Engineer"})|";
    R"|({"person":{name:"Alice","age":25,"address":{"city":"San Francisco","zip":"94105"}},"occupation":"Software Engineer"})|";

template <class T = char>
struct basic_pretty_printer {
    // The unit of indentation for multiline output
    static constexpr std::string_view json_indent = "    ";
    // The padding to use between JSON tokens on the same line
    static constexpr std::string_view json_pad    = " ";
    // Print object/array members each on their own line
    static constexpr auto json_linebreak_style = print_linebreak_style::MultiLine;
};

static_assert(printer_has_indent<basic_pretty_printer<char>>);
static_assert(printer_has_padding<basic_pretty_printer<char>>);
static_assert(printer_has_linebreak<basic_pretty_printer<char>>);

int main([[maybe_unused]] int argc, [[maybe_unused]] char* argv[])  // NOLINT(bugprone-exception-escape)
{
    json data;

    const auto parsed = data.decode(test_src);
    if (!parsed) {
        std::cerr << "Failed to parse JSON: "
            << parsed.error_message() << '\n';
        return -1;
    }
    const auto& value = data.get_value();

    const auto output =
        value.encode(encode_policy{}, basic_pretty_printer{});

    std::cout << output << '\n';
}
