/**
 * @file    test.cpp
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   Catch2 tester for turner::json
 * @date    2023-12-07
 *
 * @copyright Copyright (c) 2023 Mike DeKoker
 *
 */
#include <catch2/matchers/catch_matchers_floating_point.hpp>
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

TEST_CASE ("Simple runtime parsing check" "[parsing]") {

    json uut;

    SECTION ("Parse true") {
        REQUIRE (uut.parse("true"));
        REQUIRE (uut.get_value().is_bool());
        REQUIRE (uut.get_value().get_bool() == true);
    }

    SECTION ("Parse false") {
        REQUIRE (uut.parse("false"));
        REQUIRE (uut.get_value().is_bool());
        REQUIRE (uut.get_value().get_bool() == false);
    }

    SECTION ("Parse null") {
        REQUIRE (uut.parse("null"));
        REQUIRE (uut.get_value().is_null());
        REQUIRE (uut.get_value().get_null() == nullptr);
    }

    using namespace Catch::Matchers;

    // -- Numbers
    SECTION ("Parse numbers: Zero") {
        REQUIRE (uut.parse("0"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(0, 0.001));
    }
    SECTION ("Parse numbers: Integer") {
        REQUIRE (uut.parse("42"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(42, 0.001));
    }
    SECTION ("Parse numbers: Negative number") {
        REQUIRE (uut.parse("-10"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(-10, 0.001));
    }
    SECTION ("Parse numbers: Floating-point") {
        REQUIRE (uut.parse("3.14159"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(3.14159, 0.001));
    }
    SECTION ("Parse numbers: Scientific notation with positive exponent") {
        REQUIRE (uut.parse("2.5e3"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(2.5e3, 0.001));
    }
    SECTION ("Parse numbers: Leading zero in a fraction") {
        REQUIRE (uut.parse("0.25"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(0.25, 0.001));
    }
    SECTION ("Parse numbers: Negative zero") {
        REQUIRE (uut.parse("-0"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(0, 0.001));
    }
    SECTION ("Parse numbers: Scientific notation with negative exponent") {
        REQUIRE (uut.parse("6.022e-23"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(6.022e-23, 0.001));
    }

    // -- Strings
    SECTION ("Parse strings: Basic string") {
        REQUIRE (uut.parse(R"|("Hello, world!")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "Hello, world!");
    }
    SECTION ("Parse strings: String with Escape Characters") {
        REQUIRE (uut.parse(R"|("This is a line break:\nSecond line.")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "This is a line break:\nSecond line.");
    }
    SECTION ("Parse strings: String with Unicode Characters") {
        REQUIRE (uut.parse(R"|("Unicode character: \u00E9")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "Unicode character: \u00E9");
    }
    SECTION ("Parse strings: Empty String") {
        REQUIRE (uut.parse(R"|("")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string().empty());
    }
    SECTION ("Parse strings: String with Double Quotes") {
        REQUIRE (uut.parse(R"|("Quoted string: \"This is in quotes.\"")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "Quoted string: \"This is in quotes.\"");
    }
    SECTION ("Parse strings: String with Backslashes") {
        REQUIRE (uut.parse(R"|("C:\\Path\\To\\File")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "C:\\Path\\To\\File");
    }
    SECTION ("Parse strings: String with Leading and Trailing Whitespace") {
        REQUIRE (uut.parse(R"|("   Trimmed string   ")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "   Trimmed string   ");
    }

    // -- Objects
    SECTION ("Parse objects: Empty object") {
        REQUIRE (uut.parse(R"|({})|"));
        REQUIRE (uut.get_value().is_object());
        REQUIRE (uut.get_value().get_object()->empty());
    }
    SECTION ("Parse objects: Basic object") {
        REQUIRE (uut.parse(R"|({"name":"John Doe","age":30,"city":"New York"})|"));
        REQUIRE (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        REQUIRE (obj->size() == 3);
        REQUIRE (obj->contains("name"));
        REQUIRE (obj->contains("age"));
        REQUIRE (obj->contains("city"));
    }
    SECTION ("Parse objects: Nested object") {
        REQUIRE (uut.parse(R"|({"person":{"name":"Alice","age":25,"address":{"city":"San Francisco","zip":"94105"}},"occupation":"Software Engineer"})|"));
        REQUIRE (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        REQUIRE (obj->size() == 2);
        REQUIRE (obj->contains("occupation"));
        REQUIRE (obj->contains("person"));
    }
    SECTION ("Parse objects: Array of objects") {
        REQUIRE (uut.parse(R"|([{"id":1,"name":"Item 1"},{"id":2,"name":"Item 2"},{"id":3,"name":"Item 3"}])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 3);
    }
    SECTION ("Parse objects: Object with Array Property") {
        REQUIRE (uut.parse(R"|({"colors":["red","green","blue"],"status":"active"})|"));
        REQUIRE (uut.get_value().is_object());
        REQUIRE (uut.get_value().get_object()->size() == 2);
    }
    SECTION ("Parse objects: Boolean and null values") {
        REQUIRE (uut.parse(R"|({"isStudent":true,"hasCar":false,"grades":null})|"));
        REQUIRE (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        REQUIRE (obj->size() == 3);
        REQUIRE (obj->contains("isStudent"));
        REQUIRE (obj->contains("hasCar"));
        REQUIRE (obj->contains("grades"));
    }

    // -- Arrays
    SECTION ("Parse arrays: Empty array") {
        REQUIRE (uut.parse(R"|([])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->empty());
    }
    SECTION ("Parse arrays: Basic array") {
        REQUIRE (uut.parse(R"|(["apple", "banana", "orange"])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 3);
    }
    SECTION ("Parse arrays: Array of numbers") {
        REQUIRE (uut.parse(R"|([42, 3.14, -7, 0])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 4);
    }
    SECTION ("Parse arrays: Array of objects") {
        REQUIRE (uut.parse(R"|([{"id": 1, "name": "Item 1"}, {"id": 2, "name": "Item 2"}, {"id": 3, "name": "Item 3"}])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 3);
    }
    SECTION ("Parse arrays: Nested arrays") {
        REQUIRE (uut.parse(R"|([[1, 2, 3], ["a", "b", "c"], [true, false, null]])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 3);
    }
    SECTION ("Parse arrays: Array with mixed types") {
        REQUIRE (uut.parse(R"|(["John Doe", 30, true, null])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 4);
    }
}

TEST_CASE ("Simple runtime parsing check (Greediness)" "[parsing]") {

    json uut;

    SECTION ("Greedy: Trailing whitespace is okay") {
        const auto parse_res = uut.parse("{}    ", true);
        REQUIRE(static_cast<bool>(parse_res) == true);
        REQUIRE(uut.get_value().is_object());
    }

    SECTION ("Non-greedy: Trailing whitespace is okay") {
        const auto parse_res = uut.parse("{}    ", false);
        REQUIRE(static_cast<bool>(parse_res) == true);
        REQUIRE(uut.get_value().is_object());
    }

    SECTION ("Greedy: Trailing non-whitespace is not-okay") {
        const auto parse_res = uut.parse("{}A");
        REQUIRE(static_cast<bool>(parse_res) == false);
        REQUIRE(uut.get_value().is_object());   // Object is still parsed
        REQUIRE(*parse_res.it == 'A');
    }

    SECTION ("Non-greedy: Trailing non-whitespace is okay") {
        const auto parse_res = uut.parse("{}A", false);
        REQUIRE(static_cast<bool>(parse_res) == true);
        REQUIRE(uut.get_value().is_object());
        REQUIRE(*parse_res.it == 'A');
    }

    SECTION ("Greedy: Trailing non-whitespace is not-okay (with intermediate whitespace)") {
        const auto parse_res = uut.parse("{}   A");
        REQUIRE(static_cast<bool>(parse_res) == false);
        REQUIRE(uut.get_value().is_object());   // Object is still parsed
        REQUIRE(*parse_res.it == 'A');
    }

    SECTION ("Non-greedy: Trailing non-whitespace is okay (with intermediate whitespace)") {
        const auto parse_res = uut.parse("{}   A", false);
        REQUIRE(static_cast<bool>(parse_res) == true);
        REQUIRE(uut.get_value().is_object());
        REQUIRE(*parse_res.it == ' ');        // Non-greedy so we should be at first space
    }
}
