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
#include <catch2/matchers/catch_matchers.hpp>
#include <catch2/catch_test_macros.hpp>

#include <system_error>
#include <string_view>
#include <iterator>
#include <fstream>
#include <utility>
#include <string>
#include <cmath>

#include <turner/json-error.h>
#include <turner/json.h>

/*
    A lot of this test code uses raw string literals to specify JSON
    content. This makes it much easier to digest when looking at the code.
    For these, the JSON data is everything between R"|( and )|"). That is:
        |------ JSON data ---------|
    R"|({"name":"value","tuba":true})|")
*/

using namespace turner;

TEST_CASE ("Default construction", "[construction]") {

    json uut{};
    REQUIRE (uut.get_value().is_null());
}

TEST_CASE ("Simple runtime parsing check" "[parsing]") {

    json uut;

    SECTION ("Parse true") {
        REQUIRE (uut.decode("true"));
        REQUIRE (uut.get_value().is_bool());
        REQUIRE (uut.get_value().get_bool() == true);
    }

    SECTION ("Parse false") {
        REQUIRE (uut.decode("false"));
        REQUIRE (uut.get_value().is_bool());
        REQUIRE (uut.get_value().get_bool() == false);
    }

    SECTION ("Parse null") {
        REQUIRE (uut.decode("null"));
        REQUIRE (uut.get_value().is_null());
        REQUIRE (uut.get_value().get_null() == nullptr);
    }

    using namespace Catch::Matchers;

    constexpr double eps = 0.001;

    // -- Numbers
    SECTION ("Parse numbers: Zero") {
        REQUIRE (uut.decode("0"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(0, eps));
    }
    SECTION ("Parse numbers: Integer") {
        REQUIRE (uut.decode("42"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(42, eps));
    }
    SECTION ("Parse numbers: Negative number") {
        REQUIRE (uut.decode("-10"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(-10, eps));
    }
    SECTION ("Parse numbers: Floating-point") {
        REQUIRE (uut.decode("3.14159"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(3.14159, eps));
    }
    SECTION ("Parse numbers: Scientific notation with positive exponent") {
        REQUIRE (uut.decode("2.5e3"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(2.5e3, eps));
    }
    SECTION ("Parse numbers: Leading zero in a fraction") {
        REQUIRE (uut.decode("0.25"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(0.25, eps));
    }
    SECTION ("Parse numbers: Negative zero") {
        REQUIRE (uut.decode("-0"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(0, eps));
    }
    SECTION ("Parse numbers: Scientific notation with negative exponent") {
        REQUIRE (uut.decode("6.022e-23"));
        REQUIRE (uut.get_value().is_number());
        REQUIRE_THAT(uut.get_value().get_number(), WithinRel(6.022e-23, eps));
    }

    // -- Strings
    SECTION ("Parse strings: Basic string") {
        REQUIRE (uut.decode(R"|("Hello, world!")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "Hello, world!");
    }
    SECTION ("Parse strings: String with Escape Characters") {
        REQUIRE (uut.decode(R"|("This is a line break:\nSecond line.")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "This is a line break:\nSecond line.");
    }
    SECTION ("Parse strings: String with Unicode Characters") {
        REQUIRE (uut.decode(R"|("Unicode character: \u00E9")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "Unicode character: \u00E9");
    }
    SECTION ("Parse strings: Empty String") {
        REQUIRE (uut.decode(R"|("")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string().empty());
    }
    SECTION ("Parse strings: String with Double Quotes") {
        REQUIRE (uut.decode(R"|("Quoted string: \"This is in quotes.\"")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "Quoted string: \"This is in quotes.\"");
    }
    SECTION ("Parse strings: String with Backslashes") {
        REQUIRE (uut.decode(R"|("C:\\Path\\To\\File")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "C:\\Path\\To\\File");
    }
    SECTION ("Parse strings: String with Leading and Trailing Whitespace") {
        REQUIRE (uut.decode(R"|("   Trimmed string   ")|"));
        REQUIRE (uut.get_value().is_string());
        REQUIRE (uut.get_value().get_string() == "   Trimmed string   ");
    }

    // -- Objects
    SECTION ("Parse objects: Empty object") {
        REQUIRE (uut.decode(R"|({})|"));
        REQUIRE (uut.get_value().is_object());
        REQUIRE (uut.get_value().get_object()->empty());
    }
    SECTION ("Parse objects: Basic object") {
        REQUIRE (uut.decode(R"|({"name":"John Doe","age":30,"city":"New York"})|"));
        REQUIRE (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        REQUIRE (obj->size() == 3);
        REQUIRE (obj->contains("name"));
        REQUIRE (obj->contains("age"));
        REQUIRE (obj->contains("city"));
    }
    SECTION ("Parse objects: Nested object") {
        REQUIRE (uut.decode(R"|({"person":{"name":"Alice","age":25,"address":{"city":"San Francisco","zip":"94105"}},"occupation":"Software Engineer"})|"));
        REQUIRE (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        REQUIRE (obj->size() == 2);
        REQUIRE (obj->contains("occupation"));
        REQUIRE (obj->contains("person"));
    }
    SECTION ("Parse objects: Array of objects") {
        REQUIRE (uut.decode(R"|([{"id":1,"name":"Item 1"},{"id":2,"name":"Item 2"},{"id":3,"name":"Item 3"}])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 3);
    }
    SECTION ("Parse objects: Object with Array Property") {
        REQUIRE (uut.decode(R"|({"colors":["red","green","blue"],"status":"active"})|"));
        REQUIRE (uut.get_value().is_object());
        REQUIRE (uut.get_value().get_object()->size() == 2);
    }
    SECTION ("Parse objects: Boolean and null values") {
        REQUIRE (uut.decode(R"|({"isStudent":true,"hasCar":false,"grades":null})|"));
        REQUIRE (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        REQUIRE (obj->size() == 3);
        REQUIRE (obj->contains("isStudent"));
        REQUIRE (obj->contains("hasCar"));
        REQUIRE (obj->contains("grades"));
    }

    // -- Arrays
    SECTION ("Parse arrays: Empty array") {
        REQUIRE (uut.decode(R"|([])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->empty());
    }
    SECTION ("Parse arrays: Basic array") {
        REQUIRE (uut.decode(R"|(["apple", "banana", "orange"])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 3);
    }
    SECTION ("Parse arrays: Array of numbers") {
        REQUIRE (uut.decode(R"|([42, 3.14, -7, 0])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 4);
    }
    SECTION ("Parse arrays: Array of objects") {
        REQUIRE (uut.decode(R"|([{"id": 1, "name": "Item 1"}, {"id": 2, "name": "Item 2"}, {"id": 3, "name": "Item 3"}])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 3);
    }
    SECTION ("Parse arrays: Nested arrays") {
        REQUIRE (uut.decode(R"|([[1, 2, 3], ["a", "b", "c"], [true, false, null]])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 3);
    }
    SECTION ("Parse arrays: Array with mixed types") {
        REQUIRE (uut.decode(R"|(["John Doe", 30, true, null])|"));
        REQUIRE (uut.get_value().is_array());
        REQUIRE (uut.get_value().get_array()->size() == 4);
    }
}

TEST_CASE ("Simple runtime parsing check (Greediness)" "[parsing]") {

    json uut;

    SECTION ("Greedy: Trailing whitespace is okay") {
        REQUIRE(uut.decode("{}    ", true));
        REQUIRE(uut.get_value().is_object());
    }

    SECTION ("Non-greedy: Trailing whitespace is okay") {
        REQUIRE(uut.decode("{}    ", false));
        REQUIRE(uut.get_value().is_object());
    }

    SECTION ("Greedy: Trailing non-whitespace is not-okay") {
        const auto parse_res = uut.decode("{}A");
        REQUIRE(static_cast<bool>(parse_res) == false);
        REQUIRE(uut.get_value().is_object());   // Object is still parsed
        REQUIRE(*parse_res.it == 'A');
    }

    SECTION ("Non-greedy: Trailing non-whitespace is okay") {
        const auto parse_res = uut.decode("{}A", false);
        REQUIRE(static_cast<bool>(parse_res) == true);
        REQUIRE(uut.get_value().is_object());
        REQUIRE(*parse_res.it == 'A');
    }

    SECTION ("Greedy: Trailing non-whitespace is not-okay (with intermediate whitespace)") {
        const auto parse_res = uut.decode("{}   A");
        REQUIRE(static_cast<bool>(parse_res) == false);
        REQUIRE(uut.get_value().is_object());   // Object is still parsed
        REQUIRE(*parse_res.it == 'A');
    }

    SECTION ("Non-greedy: Trailing non-whitespace is okay (with intermediate whitespace)") {
        const auto parse_res = uut.decode("{}   A", false);
        REQUIRE(static_cast<bool>(parse_res) == true);
        REQUIRE(uut.get_value().is_object());
        REQUIRE(*parse_res.it == ' ');        // Non-greedy so we should be at first space
    }
}

TEST_CASE ("Input stream parsing check" "[parsing]") {

    json uut;

    std::ifstream ifs("sample.json");
    REQUIRE(ifs.is_open());             // This would be a test setup error
    REQUIRE(uut.decode_stream(ifs));
}

TEST_CASE ("Parsing from common string-like sources" "[parsing]") {

    static const char* raw_json_source = "{ \"Pink Fluid\" : \"Pigs (Three Different Ones)\" }";

    json uut;

    SECTION("string literal") {
        REQUIRE(uut.decode("{ \"Pink Fluid\" : \"Pigs (Three Different Ones)\" }"));
    }

    SECTION("const char* string") {
        REQUIRE(uut.decode(raw_json_source));
    }

    SECTION("Parse std::string") {
        std::string src(raw_json_source);
        REQUIRE(uut.decode(src));
    }

    SECTION("Parse std::string_view") {
        std::string_view src(raw_json_source);
        REQUIRE(uut.decode(src));
    }
}

TEST_CASE ("JSON encoding" "[encoding]") {

    SECTION("Encode literals") {
        REQUIRE(json::value{true}.encode()    == "true");
        REQUIRE(json::value{false}.encode()   == "false");
        REQUIRE(json::value{nullptr}.encode() == "null");
    }

    SECTION("Encode numbers") {
        REQUIRE(json::value{ 0.0}.encode() ==  "0");
        REQUIRE(json::value{-1.0}.encode() == "-1");
        REQUIRE(json::value{ 0.5}.encode() == "0.5");

        // Large positive number
        REQUIRE(json::value{1.23456789e+20}.encode() == "1.23456789e+20");
        // Small positive number
        REQUIRE(json::value{1.23456789e-20}.encode() == "1.23456789e-20");

        const json_encoding_policy p{json_encoding_policy::Disposition::Null};
        REQUIRE(json::value{std::nan("")}.encode(p) == "null");
    }

    SECTION("Encode string") {
        REQUIRE(json::value{"A simple string"}.encode() == "\"A simple string\"");
        REQUIRE(json::value{"\"double quotes\""}.encode() == "\"\\\"double quotes\\\"\"");
        REQUIRE(json::value{"\\reverse solidus\\"}.encode() == "\"\\\\reverse solidus\\\\\"");
        REQUIRE(json::value{"\u0001 thru \u001F"}.encode() == "\"\\u0001 thru \\u001F\"");
        REQUIRE(json::value{"\u0041BC"}.encode() == "\"ABC\"");
    }

    SECTION("Encode array") {
        REQUIRE(json::value{json::make_array()}.encode() == "[]");

        constexpr std::string_view encode_expect =
            R"|([true,false,null,0,"snake",{}])|";

        // Manually create an array
        auto a_ray = json::make_array();
        a_ray->emplace_back(true);
        a_ray->emplace_back(false);
        a_ray->emplace_back(nullptr);
        a_ray->emplace_back(0.0);
        a_ray->emplace_back("snake");
        a_ray->emplace_back(json::make_object());
        const json::value v1{std::move(a_ray)};
        REQUIRE(v1.encode() == encode_expect);

        // Use the make_array helper
        auto also_a_ray = json::make_array(true, false, nullptr, 0.0, "snake", json::make_object());
        const json::value v2{std::move(also_a_ray)};
        REQUIRE(v2.encode() == encode_expect);
    }

    SECTION("Encode object") {
        REQUIRE(json::value{json::make_object()}.encode() == "{}");

        auto obj = json::make_object();
        obj->emplace(std::make_pair("a", "dog"));
        obj->emplace(std::make_pair("b",  0.0));
        obj->emplace(std::make_pair("c",  nullptr));
        obj->emplace(std::make_pair("d",  true));
        obj->emplace(std::make_pair("e",  false));
        obj->emplace(std::make_pair("f",  json::make_array()));
        obj->emplace(std::make_pair("g",  json::make_object()));
        const json::value v{std::move(obj)};

        constexpr std::string_view encode_expect =
            R"|({"a":"dog","b":0,"c":null,"d":true,"e":false,"f":[],"g":{}})|";
        REQUIRE(v.encode() == encode_expect);
    }

    SECTION("json_encoding_policy dispositions"){

        const json::value bad_value{std::nan("")};

        // Disposition::Fail - Fail the operation
        const json_encoding_policy policy_fail{json_encoding_policy::Disposition::Fail};
        std::string s;
        auto res = bad_value.encode(std::back_inserter(s), policy_fail);
        REQUIRE(res.err == json_error::number_nan);

        // Disposition::Null - Encode the item as a JSON null
        const json_encoding_policy policy_null{json_encoding_policy::Disposition::Null};
        REQUIRE(bad_value.encode(policy_null) == "null");

        // Disposition::Throw - Throw system error (containing json_error)
        const json_encoding_policy policy_throw{json_encoding_policy::Disposition::Throw};
        REQUIRE_THROWS_AS(bad_value.encode(policy_throw), std::system_error);
    }
}
