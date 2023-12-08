/**
 * @file    test.cpp
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   Catch2 tester for turner::json
 * @date    2023-12-07
 *
 * @copyright Copyright (c) 2023 Mike DeKoker
 *
 */
#include <catch2/catch_template_test_macros.hpp>
#include <catch2/catch_test_macros.hpp>
#include <json-turner.h>

/*
    A log of this test code uses raw string literals to specify JSON
    content. This makes it much easier to digest when looking at the code.
    For these, the JSON data is everything beween R"|( and )|"). That is:
        |------ JSON data ---------|
    R"|({"name":"value","moop":true})|")
*/

using namespace turner;

TEST_CASE ("Default construction", "[construction]") {

    json default_constructed{};

    REQUIRE (default_constructed.is_valid() == false);
}

// Simple check the JSON input parses successfully; parsed data not considered
static bool test_runtime_parse(std::string_view json_input)
{
    json uut;

    auto [it, error_str] = uut.parse(json_input);
    return !error_str.has_value();
}

TEST_CASE ("Simple runtime parsing check" "[parsing]") {

    json uut;

    SECTION ("Parse keywords") {
        REQUIRE (test_runtime_parse("true"));
        REQUIRE (test_runtime_parse("false"));
        REQUIRE (test_runtime_parse("null"));
    }

    // -- Numbers
    SECTION ("Parse numbers: Zero")
        { REQUIRE (test_runtime_parse("0")); }
    SECTION ("Parse numbers: Integer")
        { REQUIRE (test_runtime_parse("42")); }
    SECTION ("Parse numbers: Negative number")
        { REQUIRE (test_runtime_parse("-10")); }
    SECTION ("Parse numbers: Floating-point")
        { REQUIRE (test_runtime_parse("3.14159")); }
    SECTION ("Parse numbers: Scientific notation with positive exponent")
        { REQUIRE (test_runtime_parse("2.5e3")); }
    SECTION ("Parse numbers: Leading zero in a fraction")
        { REQUIRE (test_runtime_parse("0.25")); }
    SECTION ("Parse numbers: Negative zero")
        { REQUIRE (test_runtime_parse("-0")); }
    SECTION ("Parse numbers: Scientific notation with negative exponent")
        { REQUIRE (test_runtime_parse("6.022e-23")); }

    // -- Strings
    SECTION ("Parse strings: Basic string")
        { REQUIRE (test_runtime_parse(R"|("Hello, world!")|")); }
    SECTION ("Parse strings: String with Escape Characters")
        { REQUIRE (test_runtime_parse(R"|("This is a line break:\nSecond line.")|")); }
    SECTION ("Parse strings: String with Unicode Characters")
        { REQUIRE (test_runtime_parse(R"|("Unicode character: \u00E9")|")); }
    SECTION ("Parse strings: Empty String")
        { REQUIRE (test_runtime_parse(R"|("")|")); }
    SECTION ("Parse strings: String with Double Quotes")
        { REQUIRE (test_runtime_parse(R"|("Quoted string: \"This is in quotes.\"")|")); }
    SECTION ("Parse strings: String with Backslashes")
        { REQUIRE (test_runtime_parse(R"|("C:\\Path\\To\\File")|")); }
    SECTION ("Parse strings: String with Leading and Trailing Whitespace")
        { REQUIRE (test_runtime_parse(R"|("   Trimmed string   ")|")); }

    // -- Objects
    SECTION ("Parse objects: Empty object")
        { REQUIRE (test_runtime_parse(R"|("{}")|")); }
    SECTION ("Parse objects: Basic object")
        { REQUIRE (test_runtime_parse(R"|("{"name":"John Doe","age":30,"city":"New York"}")|")); }
    SECTION ("Parse objects: Nested object")
        { REQUIRE (test_runtime_parse(R"|("{"person":{"name":"Alice","age":25,"address":{"city":"San Francisco","zip":"94105"}},"occupation":"Software Engineer"}")|")); }
    SECTION ("Parse objects: Array of objects")
        { REQUIRE (test_runtime_parse(R"|("[{"id":1,"name":"Item 1"},{"id":2,"name":"Item 2"},{"id":3,"name":"Item 3"}]")|")); }
    SECTION ("Parse objects: Object with Array Property")
        { REQUIRE (test_runtime_parse(R"|("{"colors":["red","green","blue"],"status":"active"}")|")); }
    SECTION ("Parse objects: Boolean and null values")
        { REQUIRE (test_runtime_parse(R"|("{"isStudent":true,"hasCar":false,"grades":null}")|")); }

    // -- Arrays
    SECTION ("Parse arrays: Empty array")
        { REQUIRE (test_runtime_parse(R"|("[]")|")); }
    SECTION ("Parse arrays: Basic array")
        { REQUIRE (test_runtime_parse(R"|("["apple", "banana", "orange"]")|")); }
    SECTION ("Parse arrays: Array of numbers")
        { REQUIRE (test_runtime_parse(R"|("[42, 3.14, -7, 0]")|")); }
    SECTION ("Parse arrays: Array of objects")
        { REQUIRE (test_runtime_parse(R"|("[{"id": 1, "name": "Item 1"}, {"id": 2, "name": "Item 2"}, {"id": 3, "name": "Item 3"}]")|")); }
    SECTION ("Parse arrays: Nested arrays")
        { REQUIRE (test_runtime_parse(R"|("[[1, 2, 3], ["a", "b", "c"], [true, false, null]]")|")); }
    SECTION ("Parse arrays: Array with mixed types")
        { REQUIRE (test_runtime_parse(R"|("["John Doe", 30, true, null]")|")); }
}
