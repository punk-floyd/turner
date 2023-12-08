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
    A lot of this test code uses raw string literals to specify JSON
    content. This makes it much easier to digest when looking at the code.
    For these, the JSON data is everything between R"|( and )|"). That is:
        |------ JSON data ---------|
    R"|({"name":"value","tuba":true})|")
*/

using namespace turner;

TEST_CASE ("Default construction", "[construction]") {

    json default_constructed{};

    REQUIRE (default_constructed.is_valid() == false);
}

// Simple check the JSON input parses successfully; parsed data not considered
static bool test_runtime_parse(json& uut, std::string_view json_input)
{
    auto [it, error_str] = uut.parse(json_input);
    return !error_str.has_value();
}

TEST_CASE ("Simple runtime parsing check" "[parsing]") {

    SECTION ("Parse true") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "true"));
        REQUIRE (uut.get_value().is_bool());
    }

    SECTION ("Parse false") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "false"));
        REQUIRE (uut.get_value().is_bool());
    }

    SECTION ("Parse null") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "null"));
        REQUIRE (uut.get_value().is_null());
    }

    // -- Numbers
    SECTION ("Parse numbers: Zero") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "0"));
        REQUIRE (uut.get_value().is_number());
    }
    SECTION ("Parse numbers: Integer") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "42"));
        REQUIRE (uut.get_value().is_number());
    }
    SECTION ("Parse numbers: Negative number") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "-10"));
        REQUIRE (uut.get_value().is_number());
    }
    SECTION ("Parse numbers: Floating-point") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "3.14159"));
        REQUIRE (uut.get_value().is_number());
    }
    SECTION ("Parse numbers: Scientific notation with positive exponent") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "2.5e3"));
        REQUIRE (uut.get_value().is_number());
    }
    SECTION ("Parse numbers: Leading zero in a fraction") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "0.25"));
        REQUIRE (uut.get_value().is_number());
    }
    SECTION ("Parse numbers: Negative zero") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "-0"));
        REQUIRE (uut.get_value().is_number());
    }
    SECTION ("Parse numbers: Scientific notation with negative exponent") {
        json uut;
        REQUIRE (test_runtime_parse(uut, "6.022e-23"));
        REQUIRE (uut.get_value().is_number());
    }

    // -- Strings
    SECTION ("Parse strings: Basic string") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|("Hello, world!")|"));
        REQUIRE (uut.get_value().is_string());
    }
    SECTION ("Parse strings: String with Escape Characters") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|("This is a line break:\nSecond line.")|"));
        REQUIRE (uut.get_value().is_string());
    }
    SECTION ("Parse strings: String with Unicode Characters") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|("Unicode character: \u00E9")|"));
        REQUIRE (uut.get_value().is_string());
    }
    SECTION ("Parse strings: Empty String") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|("")|"));
        REQUIRE (uut.get_value().is_string());
    }
    SECTION ("Parse strings: String with Double Quotes") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|("Quoted string: \"This is in quotes.\"")|"));
        REQUIRE (uut.get_value().is_string());
    }
    SECTION ("Parse strings: String with Backslashes") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|("C:\\Path\\To\\File")|"));
        REQUIRE (uut.get_value().is_string());
    }
    SECTION ("Parse strings: String with Leading and Trailing Whitespace") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|("   Trimmed string   ")|"));
        REQUIRE (uut.get_value().is_string());
    }

    // -- Objects
    SECTION ("Parse objects: Empty object") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|({})|"));
        REQUIRE (uut.get_value().is_object());
    }
    SECTION ("Parse objects: Basic object") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|({"name":"John Doe","age":30,"city":"New York"})|"));
        REQUIRE (uut.get_value().is_object());
    }
    SECTION ("Parse objects: Nested object") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|({"person":{"name":"Alice","age":25,"address":{"city":"San Francisco","zip":"94105"}},"occupation":"Software Engineer"})|"));
        REQUIRE (uut.get_value().is_object());
    }
    SECTION ("Parse objects: Array of objects") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|([{"id":1,"name":"Item 1"},{"id":2,"name":"Item 2"},{"id":3,"name":"Item 3"}])|"));
        REQUIRE (uut.get_value().is_array());
    }
    SECTION ("Parse objects: Object with Array Property") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|({"colors":["red","green","blue"],"status":"active"})|"));
        REQUIRE (uut.get_value().is_object());
    }
    SECTION ("Parse objects: Boolean and null values") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|({"isStudent":true,"hasCar":false,"grades":null})|"));
        REQUIRE (uut.get_value().is_object());
    }

    // -- Arrays
    SECTION ("Parse arrays: Empty array") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|([])|"));
        REQUIRE (uut.get_value().is_array());
    }
    SECTION ("Parse arrays: Basic array") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|(["apple", "banana", "orange"])|"));
        REQUIRE (uut.get_value().is_array());
    }
    SECTION ("Parse arrays: Array of numbers") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|([42, 3.14, -7, 0])|"));
        REQUIRE (uut.get_value().is_array());
    }
    SECTION ("Parse arrays: Array of objects") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|([{"id": 1, "name": "Item 1"}, {"id": 2, "name": "Item 2"}, {"id": 3, "name": "Item 3"}])|"));
        REQUIRE (uut.get_value().is_array());
    }
    SECTION ("Parse arrays: Nested arrays") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|([[1, 2, 3], ["a", "b", "c"], [true, false, null]])|"));
        REQUIRE (uut.get_value().is_array());
    }
    SECTION ("Parse arrays: Array with mixed types") {
        json uut;
        REQUIRE (test_runtime_parse(uut, R"|(["John Doe", 30, true, null])|"));
        REQUIRE (uut.get_value().is_array());
    }
}

TEST_CASE ("Simple runtime parsing check (Greediness)" "[parsing]") {

    SECTION ("Greedy: Trailing whitespace is okay") {
        json uut;
        auto [it, error_str] = uut.parse("{}    ", true);
        REQUIRE(!error_str.has_value());
        REQUIRE(uut.get_value().is_object());
    }

    SECTION ("Non-greedy: Trailing whitespace is okay") {
        json uut;
        auto [it, error_str] = uut.parse("{}    ", false);
        REQUIRE(!error_str.has_value());
        REQUIRE(uut.get_value().is_object());
    }

    SECTION ("Greedy: Trailing non-whitespace is not-okay") {
        json uut;
        auto [it, error_str] = uut.parse("{}A");
        REQUIRE(error_str.has_value());
        REQUIRE(uut.get_value().is_object());   // Object is still parsed
        REQUIRE(*it == 'A');
    }

    SECTION ("Non-greedy: Trailing non-whitespace is okay") {
        json uut;
        auto [it, error_str] = uut.parse("{}A", false);
        REQUIRE(!error_str.has_value());
        REQUIRE(uut.get_value().is_object());
        REQUIRE(*it == 'A');
    }

    SECTION ("Greedy: Trailing non-whitespace is not-okay (with intermediate whitespace)") {
        json uut;
        auto [it, error_str] = uut.parse("{}   A");
        REQUIRE(error_str.has_value());
        REQUIRE(uut.get_value().is_object());   // Object is still parsed
        REQUIRE(*it == 'A');
    }

    SECTION ("Non-greedy: Trailing non-whitespace is okay (with intermediate whitespace)") {
        json uut;
        auto [it, error_str] = uut.parse("{}   A", false);
        REQUIRE(!error_str.has_value());
        REQUIRE(uut.get_value().is_object());
        REQUIRE(*it == ' ');        // Non-greedy so we should be at first space
    }
}
