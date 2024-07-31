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

#include <turner/json.h>

namespace {
    constexpr double eps = 0.001;   // epsilon for comparison
}

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
        CHECK   (uut.get_value().is_bool());
        CHECK   (uut.get_value().get_bool() == true);
    }

    SECTION ("Parse false") {
        REQUIRE (uut.decode("false"));
        CHECK   (uut.get_value().is_bool());
        CHECK   (uut.get_value().get_bool() == false);
    }

    SECTION ("Parse null") {
        REQUIRE (uut.decode("null"));
        CHECK   (uut.get_value().is_null());
        CHECK   (uut.get_value().get_null() == nullptr);
    }

    // -- Numbers

    using namespace Catch::Matchers;

    // Use explicit decode policy to forbid parsing integers as integers
    decode_policy no_int_decode{};
    no_int_decode.allow_integer_decode = false;

    SECTION ("Parse numbers: Zero") {
        REQUIRE (uut.decode("0", no_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(0, eps));
    }
    SECTION ("Parse numbers: Integer") {
        REQUIRE (uut.decode("42", no_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(42, eps));
    }
    SECTION ("Parse numbers: Negative number") {
        REQUIRE (uut.decode("-10", no_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(-10, eps));
    }
    SECTION ("Parse numbers: Floating-point") {
        REQUIRE (uut.decode("3.14159", no_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(3.14159, eps));
    }
    SECTION ("Parse numbers: Scientific notation with positive exponent") {
        REQUIRE (uut.decode("2.5e3", no_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(2.5e3, eps));
    }
    SECTION ("Parse numbers: Leading zero in a fraction") {
        REQUIRE (uut.decode("0.25", no_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(0.25, eps));
    }
    SECTION ("Parse numbers: Negative zero") {
        REQUIRE (uut.decode("-0", no_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(0, eps));
    }
    SECTION ("Parse numbers: Scientific notation with negative exponent") {
        REQUIRE (uut.decode("6.022e-23", no_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(6.022e-23, eps));
    }

    // -- Integers

    // Use explicit decode policy to allow parsing integers as integers
    decode_policy allow_int_decode{};
    allow_int_decode.allow_integer_decode = true;

    SECTION ("Parse integers: Zero") {
        REQUIRE (uut.decode("0", allow_int_decode));
        CHECK   (uut.get_value().is_integer());
        CHECK   (uut.get_value().get_integer() == 0);
    }
    SECTION ("Parse integers: Integer") {
        REQUIRE (uut.decode("42", allow_int_decode));
        CHECK   (uut.get_value().is_integer());
        CHECK   (uut.get_value().get_integer() == 42);
    }
    SECTION ("Parse integers: Negative integer") {
        REQUIRE (uut.decode("-42", allow_int_decode));
        CHECK   (uut.get_value().is_integer());
        CHECK   (uut.get_value().get_integer() == -42);
    }
    SECTION ("Parse integers: Floating-point is still a number") {
        REQUIRE (uut.decode("3.14159", allow_int_decode));
        CHECK   (uut.get_value().is_number());
        CHECK_THAT(uut.get_value().get_number(), WithinRel(3.14159, eps));
    }

    // -- Strings
    SECTION ("Parse strings: Basic string") {
        REQUIRE (uut.decode(R"|("Hello, world!")|"));
        CHECK   (uut.get_value().is_string());
        CHECK   (uut.get_value().get_string() == "Hello, world!");
    }
    SECTION ("Parse strings: String with Escape Characters") {
        REQUIRE (uut.decode(R"|("This is a line break:\nSecond line.")|"));
        CHECK   (uut.get_value().is_string());
        CHECK   (uut.get_value().get_string() == "This is a line break:\nSecond line.");
    }
    SECTION ("Parse strings: String with Unicode Characters") {
        REQUIRE (uut.decode(R"|("Unicode character: \u00E9")|"));
        CHECK   (uut.get_value().is_string());
        CHECK   (uut.get_value().get_string() == "Unicode character: \u00E9");
    }
    SECTION ("Parse strings: Empty String") {
        REQUIRE (uut.decode(R"|("")|"));
        CHECK   (uut.get_value().is_string());
        CHECK   (uut.get_value().get_string().empty());
    }
    SECTION ("Parse strings: String with Double Quotes") {
        REQUIRE (uut.decode(R"|("Quoted string: \"This is in quotes.\"")|"));
        CHECK   (uut.get_value().is_string());
        CHECK   (uut.get_value().get_string() == "Quoted string: \"This is in quotes.\"");
    }
    SECTION ("Parse strings: String with Backslashes") {
        REQUIRE (uut.decode(R"|("C:\\Path\\To\\File")|"));
        CHECK   (uut.get_value().is_string());
        CHECK   (uut.get_value().get_string() == "C:\\Path\\To\\File");
    }
    SECTION ("Parse strings: String with Leading and Trailing Whitespace") {
        REQUIRE (uut.decode(R"|("   Trimmed string   ")|"));
        CHECK   (uut.get_value().is_string());
        CHECK   (uut.get_value().get_string() == "   Trimmed string   ");
    }

    // -- Objects
    SECTION ("Parse objects: Empty object") {
        REQUIRE (uut.decode(R"|({})|"));
        CHECK   (uut.get_value().is_object());
        CHECK   (uut.get_value().get_object().empty());
    }
    SECTION ("Parse objects: Basic object") {
        REQUIRE (uut.decode(R"|({"name":"John Doe","age":30,"city":"New York"})|"));
        CHECK   (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        CHECK   (obj.size() == 3);
        CHECK   (obj.contains("name"));
        CHECK   (obj.contains("age"));
        CHECK   (obj.contains("city"));
    }
    SECTION ("Parse objects: Nested object") {
        REQUIRE (uut.decode(R"|({"person":{"name":"Alice","age":25,"address":{"city":"San Francisco","zip":"94105"}},"occupation":"Software Engineer"})|"));
        CHECK   (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        CHECK   (obj.size() == 2);
        CHECK   (obj.contains("occupation"));
        CHECK   (obj.contains("person"));
    }
    SECTION ("Parse objects: Array of objects") {
        REQUIRE (uut.decode(R"|([{"id":1,"name":"Item 1"},{"id":2,"name":"Item 2"},{"id":3,"name":"Item 3"}])|"));
        CHECK   (uut.get_value().is_array());
        CHECK   (uut.get_value().get_array().size() == 3);
    }
    SECTION ("Parse objects: Object with Array Property") {
        REQUIRE (uut.decode(R"|({"colors":["red","green","blue"],"status":"active"})|"));
        CHECK   (uut.get_value().is_object());
        CHECK   (uut.get_value().get_object().size() == 2);
    }
    SECTION ("Parse objects: Boolean and null values") {
        REQUIRE (uut.decode(R"|({"isStudent":true,"hasCar":false,"grades":null})|"));
        CHECK   (uut.get_value().is_object());
        const auto& obj = uut.get_value().get_object();
        CHECK   (obj.size() == 3);
        CHECK   (obj.contains("isStudent"));
        CHECK   (obj.contains("hasCar"));
        CHECK   (obj.contains("grades"));
    }

    // -- Arrays
    SECTION ("Parse arrays: Empty array") {
        REQUIRE (uut.decode(R"|([])|"));
        CHECK   (uut.get_value().is_array());
        CHECK   (uut.get_value().get_array().empty());
    }
    SECTION ("Parse arrays: Basic array") {
        REQUIRE (uut.decode(R"|(["apple", "banana", "orange"])|"));
        CHECK   (uut.get_value().is_array());
        CHECK   (uut.get_value().get_array().size() == 3);
    }
    SECTION ("Parse arrays: Array of numbers") {
        REQUIRE (uut.decode(R"|([42, 3.14, -7, 0])|"));
        CHECK   (uut.get_value().is_array());
        CHECK   (uut.get_value().get_array().size() == 4);
    }
    SECTION ("Parse arrays: Array of objects") {
        REQUIRE (uut.decode(R"|([{"id": 1, "name": "Item 1"}, {"id": 2, "name": "Item 2"}, {"id": 3, "name": "Item 3"}])|"));
        CHECK   (uut.get_value().is_array());
        CHECK   (uut.get_value().get_array().size() == 3);
    }
    SECTION ("Parse arrays: Nested arrays") {
        REQUIRE (uut.decode(R"|([[1, 2, 3], ["a", "b", "c"], [true, false, null]])|"));
        CHECK   (uut.get_value().is_array());
        CHECK   (uut.get_value().get_array().size() == 3);
    }
    SECTION ("Parse arrays: Array with mixed types") {
        REQUIRE (uut.decode(R"|(["John Doe", 30, true, null])|"));
        CHECK   (uut.get_value().is_array());
        CHECK   (uut.get_value().get_array().size() == 4);
    }
}

TEST_CASE ("Simple runtime parsing check (Greediness)" "[parsing]") {

    json uut;

    decode_policy policy_greedy{};
    policy_greedy.greedy = true;

    decode_policy policy_not_greedy{};
    policy_not_greedy.greedy = false;

    SECTION ("Greedy: Trailing whitespace is okay") {
        REQUIRE(uut.decode("{}    ", policy_greedy));
        CHECK  (uut.get_value().is_object());
    }

    SECTION ("Non-greedy: Trailing whitespace is okay") {
        REQUIRE(uut.decode("{}    ", policy_not_greedy));
        CHECK  (uut.get_value().is_object());
    }

    SECTION ("Greedy: Trailing non-whitespace is not-okay") {
        const auto parse_res = uut.decode("{}A", policy_greedy);
        REQUIRE(static_cast<bool>(parse_res) == false);
        CHECK  (uut.get_value().is_object());   // Object is still parsed
        CHECK  (*parse_res.it == 'A');
    }

    SECTION ("Non-greedy: Trailing non-whitespace is okay") {
        const auto parse_res = uut.decode("{}A", policy_not_greedy);
        REQUIRE(static_cast<bool>(parse_res) == true);
        CHECK  (uut.get_value().is_object());
        CHECK  (*parse_res.it == 'A');
    }

    SECTION ("Greedy: Trailing non-whitespace is not-okay (with intermediate whitespace)") {
        const auto parse_res = uut.decode("{}   A", policy_greedy);
        REQUIRE(static_cast<bool>(parse_res) == false);
        CHECK  (uut.get_value().is_object());   // Object is still parsed
        CHECK  (*parse_res.it == 'A');
    }

    SECTION ("Non-greedy: Trailing non-whitespace is okay (with intermediate whitespace)") {
        const auto parse_res = uut.decode("{}   A", policy_not_greedy);
        REQUIRE(static_cast<bool>(parse_res) == true);
        CHECK  (uut.get_value().is_object());
        CHECK  (*parse_res.it == ' ');        // Non-greedy so we should be at first space
    }
}

TEST_CASE ("Object member name uniqueness" "[parsing]") {

    // A JSON object with two members that have the same name
    const auto* source = R"|({"foo" : false, "foo" : true})|";

    json uut;

    SECTION ("NonUniqueDisposition::Overwrite") {

        decode_policy policy{};
        policy.non_unique_member_name_disposition =
            decode_policy::NonUniqueDisposition::Overwrite;

        REQUIRE(uut.decode(source, policy));
        CHECK  (uut.get_value().is_object());

        const auto& obj = uut.get_value().get_object();
        const auto it  = obj.find("foo");
        CHECK  (it != obj.cend());
        CHECK  (it->second.is_bool());
        CHECK  (it->second.get_bool());
    }

    SECTION ("NonUniqueDisposition::Fail") {

        decode_policy policy{};
        policy.non_unique_member_name_disposition =
            decode_policy::NonUniqueDisposition::Fail;

        const auto res = uut.decode(source, policy);
        REQUIRE(!res);
        CHECK  (res.err == json_error::nonunique_member_name);
    }

    SECTION ("NonUniqueDisposition::Throw") {

        decode_policy policy{};
        policy.non_unique_member_name_disposition =
            decode_policy::NonUniqueDisposition::Throw;

        REQUIRE_THROWS_AS(uut.decode(source, policy), std::system_error);
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
        CHECK  (uut.decode("{ \"Pink Fluid\" : \"Pigs (Three Different Ones)\" }"));
    }

    SECTION("const char* string") {
        CHECK  (uut.decode(raw_json_source));
    }

    SECTION("Parse std::string") {
        std::string src(raw_json_source);
        CHECK  (uut.decode(src));
    }

    SECTION("Parse std::string_view") {
        std::string_view src(raw_json_source);
        CHECK  (uut.decode(src));
    }
}

TEST_CASE ("JSON encoding" "[encoding]") {

    SECTION("Encode literals") {
        CHECK  (json::value{true}.encode()    == "true");
        CHECK  (json::value{false}.encode()   == "false");
        CHECK  (json::value{nullptr}.encode() == "null");
    }

    SECTION("Encode numbers") {
        CHECK  (json::value{ 0.0}.encode() ==  "0");
        CHECK  (json::value{-1.0}.encode() == "-1");
        CHECK  (json::value{ 0.5}.encode() == "0.5");

        // Large positive number
        //CHECK  (json::value{1.23456789e+20}.encode() == "1.23456789e+20");
        // Small positive number
        //CHECK  (json::value{1.23456789e-20}.encode() == "1.23456789e-20");

        const encode_policy p{encode_policy::Disposition::Null};
        CHECK  (json::value{std::nan("")}.encode(p) == "null");
    }

    SECTION("Encode integers") {
        // Note: JSON does not technically support integers, per-se.

        CHECK  (json::value{0}.encode() == "0");

        // Any integer value in the range +/- 2^53 can be represented in a
        // IEEE 754 double precision floating point value.
        CHECK  (json::value{ 9007199254740992LL}.encode() ==  "9007199254740992");
        CHECK  (json::value{-9007199254740992LL}.encode() == "-9007199254740992");
    }

    SECTION("Encode string") {
        CHECK  (json::value{"A simple string"}.encode() == "\"A simple string\"");
        CHECK  (json::value{"\"double quotes\""}.encode() == "\"\\\"double quotes\\\"\"");
        CHECK  (json::value{"\\reverse solidus\\"}.encode() == "\"\\\\reverse solidus\\\\\"");
        CHECK  (json::value{"\u0001 thru \u001F"}.encode() == "\"\\u0001 thru \\u001F\"");
        CHECK  (json::value{"\u0041BC"}.encode() == "\"ABC\"");
    }

    SECTION("Encode array") {
        CHECK  (json::value{json::make_array()}.encode() == "[]");

        constexpr std::string_view encode_expect =
            R"|([true,false,null,0,"snake",{}])|";

        // Manually create an array
        auto a_ray = json::make_array();
        a_ray.emplace_back(true);
        a_ray.emplace_back(false);
        a_ray.emplace_back(nullptr);
        a_ray.emplace_back(0.0);
        a_ray.emplace_back("snake");
        a_ray.emplace_back(json::object{});
        const json::value v1{std::move(a_ray)};
        CHECK  (v1.encode() == encode_expect);

        // Use the make_array helper
        auto also_a_ray = json::make_array(true, false, nullptr, 0.0, "snake", json::object{});
        const json::value v2{std::move(also_a_ray)};
        CHECK  (v2.encode() == encode_expect);
    }

    SECTION("Encode object") {
        CHECK  (json::value{json::object{}}.encode() == "{}");

        auto obj = json::object{};
        obj.emplace(std::make_pair("a", "dog"));
        obj.emplace(std::make_pair("b",  0.0));
        obj.emplace(std::make_pair("c",  nullptr));
        obj.emplace(std::make_pair("d",  true));
        obj.emplace(std::make_pair("e",  false));
        obj.emplace(std::make_pair("f",  json::make_array()));
        obj.emplace(std::make_pair("g",  json::object{}));
        const json::value v{std::move(obj)};

        constexpr std::string_view encode_expect =
            R"|({"a":"dog","b":0,"c":null,"d":true,"e":false,"f":[],"g":{}})|";
        CHECK  (v.encode() == encode_expect);
    }

    SECTION("encode_policy dispositions") {

        const json::value bad_value{std::nan("")};

        // Disposition::Fail - Fail the operation
        const encode_policy policy_fail{encode_policy::Disposition::Fail};
        std::string s;
        auto res = bad_value.encode(std::back_inserter(s), policy_fail);
        CHECK  (res.err == json_error::number_invalid);

        // Disposition::Null - Encode the item as a JSON null
        const encode_policy policy_null{encode_policy::Disposition::Null};
        CHECK  (bad_value.encode(policy_null) == "null");

        // Disposition::Throw - Throw system error (containing json_error)
        const encode_policy policy_throw{encode_policy::Disposition::Throw};
        REQUIRE_THROWS_AS(bad_value.encode(policy_throw), std::system_error);
    }
}

TEST_CASE ("JSON object member lookup") {

    using namespace Catch::Matchers;

    const     static json::string str_value = "dog";
    constexpr static json::number number_value = 0.0;
    constexpr static json::integer integer_value = 1000;

    auto obj = json::object{};
    obj.emplace(std::make_pair("a", str_value));
    obj.emplace(std::make_pair("b", number_value));
    obj.emplace(std::make_pair("c", nullptr));
    obj.emplace(std::make_pair("d", true));
    obj.emplace(std::make_pair("e", false));
    obj.emplace(std::make_pair("f", json::make_array()));
    obj.emplace(std::make_pair("g", json::object{}));
    obj.emplace(std::make_pair("h", integer_value));
    const json::value v{std::move(obj)};

    // Check lookups that should be there
    CHECK  (v.get_member_string("a"));
    CHECK  (v.get_member_string("a").value() == str_value);
    CHECK  (v.get_member_number("b"));
    CHECK_THAT(v.get_member_number("b").value(), WithinRel(number_value, eps));
    CHECK  (v.get_member_null("c"));
    CHECK  (v.get_member_null("c").value() == nullptr);
    CHECK  (v.get_member_bool("d"));
    CHECK  (v.get_member_bool("d").value() == true);
    CHECK  (v.get_member_bool("e"));
    CHECK  (v.get_member_bool("e").value() == false);
    CHECK  (v.get_member_array("f"));
    CHECK  (v.get_member_object("g"));
    CHECK  (v.get_member_integer("h"));
    CHECK  (v.get_member_integer("h").value() == integer_value);

    // Check bad lookups with default returns
    CHECK  (v.get_member_string("No", "default").value() == "default");
    CHECK_THAT(v.get_member_number("Non", 3.14159).value(), WithinRel(3.14159, eps));
    CHECK  (v.get_member_null("Nee", nullptr).value() == nullptr);
    CHECK  (v.get_member_bool("いいえ", true).value() == true);
    CHECK  (v.get_member_array("不", json::array{}).has_value());
    CHECK  (v.get_member_object("Нет", json::object{}).has_value());
    CHECK  (v.get_member_integer("E", 1234).value() == 1234);

    // Check bad lookup with no default
    CHECK_FALSE(v.get_member_string("Não"));
    CHECK_FALSE(v.get_member_number("아니요"));
    CHECK_FALSE(v.get_member_null("لا"));
    CHECK_FALSE(v.get_member_bool("Nej"));
    CHECK_FALSE(v.get_member_array(" Όχι"));
    CHECK_FALSE(v.get_member_object("नहीं"));
    CHECK_FALSE(v.get_member_integer("Hayır"));
}
