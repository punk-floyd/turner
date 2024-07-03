/**
 * @file    test-expected.cpp
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   Catch2 tester for turner::expected
 * @date    2024-04-05
 *
 * @copyright Copyright (c) 2024 Mike DeKoker
 *
 */
#include <initializer_list>
#include <string_view>
#include <type_traits>
#include <cstddef>
#include <string>
#include <utility>
#include <catch2/catch_test_macros.hpp>

#include <turner/expected.h>         // UUT

// NOLINTBEGIN(readability-function-cognitive-complexity)

TEST_CASE ("turner::unexpected") {

    using error_type = std::string;

    const error_type sample_error = "Chicago Cubs";
    const error_type sample_other = "Billy Summers is the book I'm reading right now";

    // No default construction for unexpected

    SECTION ("Construct with lvalue") {
        turner::unexpected<error_type> uut{sample_error};
        REQUIRE(uut.error() == sample_error);   // error() &
        const auto& alias = uut;
        REQUIRE(alias.error() == sample_error); // error() const&
    }

    SECTION ("Construct with rvalue") {
        const turner::unexpected<error_type> uut{std::string{sample_error}};
        REQUIRE(uut.error() == sample_error);   // error() &
    }

    SECTION ("Copy construction") {
        const turner::unexpected<error_type> src{sample_error};
        //NOLINTNEXTLINE(performance-unnecessary-copy-initialization)
        const turner::unexpected<error_type> uut{src};
        REQUIRE(src.error() == uut.error());
    }

    SECTION ("Move construction") {
        turner::unexpected<error_type> src{sample_error};
        const turner::unexpected<error_type> uut{std::move(src)};
        REQUIRE(uut.error() == sample_error);
        REQUIRE(src.error().empty());   // NOLINT(bugprone-use-after-move)
    }

    SECTION ("In place construction") {
        const turner::unexpected<std::string> uut{std::in_place, std::size_t{5}, 'a'};
        REQUIRE(uut.error() == "aaaaa");
    }

    SECTION ("In place construction (list)") {
        const turner::unexpected<std::string> uut{std::in_place, { 'M', 'i', 'k', 'e' }};
        REQUIRE(uut.error() == "Mike");
    }

    SECTION ("Copy assignment") {
        const turner::unexpected<error_type> src{sample_error};
        turner::unexpected<error_type> uut{sample_other};
        uut = src;
        REQUIRE(uut.error() == sample_error);
        REQUIRE(src.error() == sample_error);
    }

    SECTION ("Move assignment") {
        turner::unexpected<error_type> src{sample_error};
        turner::unexpected<error_type> uut{sample_other};
        uut = std::move(src);
        REQUIRE(uut.error() == sample_error);
        REQUIRE(src.error().empty());   // NOLINT(bugprone-use-after-move)
    }

    SECTION ("Move error() &&") {
        turner::unexpected<error_type> src{sample_error};
        const auto poached = std::move(src).error();
        REQUIRE(poached == sample_error);
        REQUIRE(src.error().empty());   // NOLINT(bugprone-use-after-move)
    }

    SECTION ("Move error() const &&") {
        const turner::unexpected<error_type> src{sample_error};
        const auto poached = const_cast<const turner::unexpected<error_type>&&>(src).error();
        REQUIRE(poached == sample_error);
    }

    SECTION ("swap (direct)") {
        turner::unexpected<error_type> uut_a{sample_error};
        turner::unexpected<error_type> uut_b{sample_other};
        uut_a.swap(uut_b);
        REQUIRE(uut_a.error() == sample_other);
        REQUIRE(uut_b.error() == sample_error);
    }

    SECTION ("swap (ADL)") {
        turner::unexpected<error_type> uut_a{sample_error};
        turner::unexpected<error_type> uut_b{sample_other};
        using std::swap;
        swap(uut_a, uut_b);
        REQUIRE(uut_a.error() == sample_other);
        REQUIRE(uut_b.error() == sample_error);
    }

    SECTION ("Operator ==") {
        turner::unexpected<error_type> uut_a{sample_error};
        turner::unexpected<error_type> uut_b{sample_error};
        turner::unexpected<error_type> uut_c{sample_other};
        REQUIRE(uut_a == uut_b);
        REQUIRE(uut_a != uut_c);    // Synthesized by compiler
    }
}

TEST_CASE ("turner::expected<!void,...> constructors") {

    using value_type = int; // Note void not yet supported
    using error_type = std::string;
    const value_type sample_value = 42;
    const error_type sample_error = "Chicago Cubs";

    // Requirements of value_type for this test case
    static_assert(!std::is_void_v<value_type>);

    // Requirements of turner::unexpected value type
    static_assert(
        !std::is_array_v<error_type> &&
         std::is_object_v<error_type> &&
        !std::is_const_v<error_type> &&
        !std::is_volatile_v<error_type>
    );

    SECTION ("Default construction (1)") {
        turner::expected<value_type, error_type> uut{};
        REQUIRE(uut.has_value());
        REQUIRE(uut);       // operator bool
    }

    SECTION ("Construct with expected value (6)") {
        turner::expected<value_type, error_type> uut{sample_value};
        REQUIRE(uut.has_value());
        REQUIRE(uut);       // operator bool
        REQUIRE(uut.value() == sample_value);
        REQUIRE(*uut == sample_value);
    }

    SECTION ("Construct with unexpected value lvalue (7)") {
        const turner::unexpected src{sample_error};
        turner::expected<value_type, error_type> uut{src};
        REQUIRE(uut.has_value() == false);
        REQUIRE(!uut);       // operator bool
        REQUIRE(uut.error() == sample_error);

        // Accessing value() should throw
        REQUIRE_THROWS_AS(uut.value(), turner::bad_expected_access<error_type>);
    }

    SECTION ("Construct with unexpected value rvalue (8)") {
        turner::expected<value_type, error_type> uut{turner::unexpected{sample_error}};
        REQUIRE(uut.has_value() == false);
        REQUIRE(!uut);       // operator bool
        REQUIRE(uut.error() == sample_error);

        // Accessing value() should throw
        REQUIRE_THROWS_AS(uut.value(), turner::bad_expected_access<error_type>);
    }

    SECTION ("Copy construction: source is expected (2)") {
        const turner::expected<value_type, error_type> src{sample_value};
        turner::expected<value_type, error_type> uut{src};
        REQUIRE(uut.has_value());
        REQUIRE(src.has_value());
        REQUIRE(uut.value() == src.value());
    }

    SECTION ("Copy construction: source is unexpected (2)") {
        const turner::expected<value_type, error_type> src{turner::unexpected{sample_error}};
        turner::expected<value_type, error_type> uut{src};
        REQUIRE(!uut.has_value());
        REQUIRE(!src.has_value());
        REQUIRE(uut.error() == src.error());
    }

    SECTION ("Move construction: source is expected (3)") {
        turner::expected<value_type, error_type> src{sample_value};
        turner::expected<value_type, error_type> uut{std::move(src)};
        REQUIRE(uut.has_value());
        REQUIRE(uut.value() == sample_value);
    }

    SECTION ("Move construction: source is unexpected (3)") {
        turner::expected<value_type, error_type> src{turner::unexpected{sample_error}};
        turner::expected<value_type, error_type> uut{std::move(src)};
        REQUIRE(!uut.has_value());
        REQUIRE(uut.error() == sample_error);
    }

    SECTION ("Converting lvalue expected constructor with expected value (4)") {
        const turner::expected<std::string, int> src{"Some string value"};
        turner::expected<std::string_view, int> uut{src};
        REQUIRE(uut.value() == src.value());
    }

    SECTION ("Converting lvalue expected constructor with unexpected value (4)") {
        const turner::expected<int, std::string> src{turner::unexpected{"Some string value"}};
        turner::expected<int, std::string_view> uut{src};
        REQUIRE(uut.error() == src.error());
    }

    SECTION ("Converting rvalue expected constructor with expected value (5)") {
        constexpr auto* src_str = "Some string value";
        turner::expected<std::string, int> src{src_str};
        turner::expected<std::string, double> uut{std::move(src)};
        REQUIRE(uut.value() == src_str);
        REQUIRE(src.value().empty());   // NOLINT(bugprone-use-after-move)
    }

    SECTION ("Converting rvalue expected constructor with unexpected value (5)") {
        constexpr auto* src_str = "Some string value";
        turner::expected<int,    std::string> src{turner::unexpected{src_str}};
        turner::expected<double, std::string> uut{std::move(src)};
        REQUIRE(uut.error() == src_str);
        REQUIRE(src.error().empty());   // NOLINT(bugprone-use-after-move)
    }

    SECTION ("In place construction of expected value (9)") {
        turner::expected<std::string, int> uut{std::in_place, std::size_t{5}, 'a'};
        REQUIRE(uut.has_value());
        REQUIRE(uut.value() == "aaaaa");
    }

    SECTION ("In place construction of expected value with a list (10)") {
        turner::expected<std::string, int> uut{std::in_place, {'C', 'a', 'm', 'i', 'l', 'a'} };
        REQUIRE(uut.has_value());
        REQUIRE(uut.value() == "Camila");
    }

    SECTION ("In place construction of unexpected value (12)") {
        turner::expected<int, std::string> uut{turner::unexpect, std::size_t{5}, 'a'};
        REQUIRE(!uut.has_value());
        REQUIRE(uut.error() == "aaaaa");
    }

    SECTION ("In place construction of unexpected value with a list (13)") {
        turner::expected<int, std::string> uut{turner::unexpect, {'C', 'a', 'm', 'i', 'l', 'a'}};
        REQUIRE(!uut.has_value());
        REQUIRE(uut.error() == "Camila");
    }
}

TEST_CASE ("turner::expected<!void,...> construction (constexpr)") {

    using value_type = int;
    using error_type = std::string_view;
    constexpr value_type sample_value = 42;
    constexpr error_type sample_error = "Miles Davis";

    // Requirements of value_type for this test case
    static_assert(!std::is_void_v<value_type>);

    using my_expected = turner::expected<value_type, error_type>;

    // Used to ensure copy/move construction is deleted when appropriate
    struct NoCopyOrMove {
        NoCopyOrMove(const NoCopyOrMove&) = delete;
        NoCopyOrMove& operator=(const NoCopyOrMove&) = delete;
    };
    static_assert(!std::is_copy_constructible_v<NoCopyOrMove>);
    static_assert(!std::is_move_constructible_v<NoCopyOrMove>);

    // Default (1)
    static_assert(my_expected{}.has_value());

    // Copy constructor (2)
    constexpr my_expected copy_src{sample_value};
    static_assert(my_expected{copy_src}.value() == sample_value);
    static_assert(std::is_trivially_copy_constructible_v<turner::expected<int, int>>);
    static_assert(!std::is_trivially_copy_constructible_v<turner::expected<int, std::string>>);
    static_assert(!std::is_copy_constructible_v<turner::expected<value_type, NoCopyOrMove>>);

    // Move constructor (3)
    static_assert(my_expected{my_expected{sample_value}}.value() == sample_value);
    static_assert(std::is_trivially_move_constructible_v<turner::expected<int, int>>);
    static_assert(!std::is_trivially_move_constructible_v<turner::expected<int, std::string>>);
    static_assert(!std::is_move_constructible_v<turner::expected<value_type, NoCopyOrMove>>);

    // Converting expected lvalue (4)
    constexpr turner::expected<char, error_type> converting_lval{'a'};
    static_assert(turner::expected<int, error_type>{converting_lval}.value() == 'a');

    // Converting expected rvalue (5)
    static_assert(turner::expected<int, error_type>{
        turner::expected<char, error_type>{'b'}}.value() == 'b');

    // Construct from expected value (6)
    static_assert(my_expected{sample_value}.value() == sample_value);

    // Construct from unexpected lvalue (7)
    constexpr turner::unexpected lval_err_src{sample_error};
    static_assert(my_expected{lval_err_src}.error() == sample_error);

    // Construct from unexpected rvalue (8)
    static_assert(my_expected{turner::unexpected{sample_error}}.error() == sample_error);

    class Foo {
        int     _a{};
        std::size_t  _sz{};
    public:
        constexpr Foo(int a) noexcept : _a(a) {}
        constexpr Foo(std::initializer_list<int> il) noexcept : _sz(il.size()) {}
        [[nodiscard]] constexpr int get() const noexcept { return _a; }
        [[nodiscard]] constexpr std::size_t size() const noexcept { return _sz; }
    };

    // In-place construct expected value (9)
    static_assert(turner::expected<Foo, int>{std::in_place, 42}.value().get() == 42);

    // In-place construct expected value with list (10)
    static_assert(turner::expected<Foo, int>{std::in_place, {1,2,3,4,5}}.value().size() == 5);

    // (11) is expected<void,...> is handled in a different test case below

    // In-place construct unexpected value (12)
    static_assert(turner::expected<int, Foo>{turner::unexpect, 42}.error().get() == 42);

    // In-place construct unexpected value with list (13)
    static_assert(turner::expected<int, Foo>{turner::unexpect, {1,2,3,4,5}}.error().size() == 5);

    REQUIRE(true);  // If we compile, we pass!
}

TEST_CASE ("turner::expected<!void,...> observers") {

    using value_type = std::string;
    using error_type = std::string;
    const value_type sample_value = "Pat Hughes";
    const error_type sample_error = "Ron Coomer";

    // Requirements of value_type for this test case
    static_assert(!std::is_void_v<value_type>);

    using my_expected = turner::expected<value_type, error_type>;

    SECTION("operator->") {
        my_expected uut_val{sample_value};
        REQUIRE(const_cast<      my_expected&>(uut_val)->data() == sample_value);   // NOLINT(readability-redundant-string-cstr)
        REQUIRE(const_cast<const my_expected&>(uut_val)->data() == sample_value);   // NOLINT(readability-redundant-string-cstr)
    }

    SECTION("operator*") {
        // T& and const T&
        my_expected uut_val{sample_value};
        REQUIRE((*const_cast<      my_expected&>(uut_val)).data() == sample_value); // NOLINT(readability-redundant-string-cstr)
        REQUIRE((*const_cast<const my_expected&>(uut_val)).data() == sample_value); // NOLINT(readability-redundant-string-cstr)
        // T&&
        const auto moved_value = *std::move(uut_val);
        REQUIRE(moved_value == sample_value);
        REQUIRE(uut_val->empty());  // NOLINT(bugprone-use-after-move)
        // const T&&
        my_expected uut_val2{sample_value};
        const auto not_moved_value = *static_cast<const my_expected&&>(uut_val2);
        REQUIRE(not_moved_value == sample_value);
        REQUIRE(*uut_val2       == sample_value);   // Was a copy, not a move
    }

    SECTION("error()") {
        // E& and const E&
        my_expected uut_err{turner::unexpected{sample_error}};
        REQUIRE(const_cast<      my_expected&>(uut_err).error() == sample_error);
        REQUIRE(const_cast<const my_expected&>(uut_err).error() == sample_error);
        // T&&
        const auto moved_error = std::move(uut_err).error();
        REQUIRE(moved_error == sample_error);
        REQUIRE(uut_err.error().empty());  // NOLINT(bugprone-use-after-move)
        my_expected uut_err2{turner::unexpected{sample_error}};
        const auto not_moved_error = static_cast<const my_expected&&>(uut_err2).error();
        REQUIRE(not_moved_error  == sample_error);
        REQUIRE(uut_err2.error() == sample_error);
    }
}

TEST_CASE ("turner::expected value_or") {

    using value_type = std::string_view;
    using error_type = std::string_view;

    // value_or not defined for void expected types
    static_assert(!std::is_void_v<value_type>);

    constexpr value_type sample_value = "sample_value";
    constexpr value_type other_value  = "other_value";
    constexpr error_type error_value  = "error_value";

    using my_expected = turner::expected<value_type, error_type>;

    SECTION ("value_or(...) const&") {
        static_assert(my_expected{sample_value}.value_or(other_value) == sample_value);
        static_assert(my_expected{turner::unexpected{error_value}}.value_or(other_value) == other_value);
        REQUIRE(true);  // If it compiles, it passes
    }

    SECTION("value_or(...) &&") {
        turner::expected<std::string, int> uut_val{sample_value};
        const auto moved_value{std::move(uut_val).value_or("uh-oh")};
        REQUIRE(moved_value == sample_value);
        constexpr auto* great_success = "Great success!";
        turner::expected<std::string, int> uut_err{turner::unexpected{42}};
        const auto moved_error{std::move(uut_err).value_or(great_success)};
        REQUIRE(moved_error == great_success);
    }
}

TEST_CASE ("turner::expected emplace") {

    class Foo {
        int     _a{};
        std::size_t  _sz{};
    public:
        constexpr Foo() = default;
        constexpr Foo(int a) noexcept : _a(a) {}
        constexpr Foo(std::initializer_list<int> il) noexcept : _sz(il.size()) {}
        [[nodiscard]] constexpr int get() const noexcept { return _a; }
        [[nodiscard]] constexpr std::size_t size() const noexcept { return _sz; }
    };

    using my_expected = turner::expected<Foo, int>;

    // emplace (non-list)
    static_assert(my_expected{}.emplace(42).get() == 42);
    // emplace (list)
    static_assert(my_expected{}.emplace({1,1,2,3,5,8,13}).size() == 7);
    REQUIRE(true);

    SECTION ("expected<void,...>") {
        // Emplace with an expected value: not very exciting
        turner::expected<void, int> uut_a;
        uut_a.emplace();
        REQUIRE(uut_a.has_value());

        // Emplace with an unexpected value: a little more interesting
        turner::expected<void, int> uut_b{turner::unexpected{42}};
        REQUIRE(!uut_b.has_value());
        uut_b.emplace();
        REQUIRE(uut_b.has_value());
    }
}

TEST_CASE ("turner::expected<!void,...> assignment") {

    using value_type = std::string;
    using error_type = std::string;

    const value_type sample_value = "42";
    const error_type sample_error = "Chicago Cubs";
    const error_type other_error  = "San Diego Padres";

    using my_expected = turner::expected<value_type, error_type>;

    SECTION("Copy assignment") {

        const my_expected src_val{sample_value};
        const my_expected src_err{turner::unexpected{sample_error}};

        my_expected uut_val_to_val{};
        REQUIRE(uut_val_to_val.has_value() == true);
        uut_val_to_val = src_val;
        REQUIRE(uut_val_to_val.has_value() == true);
        REQUIRE(uut_val_to_val.value() == src_val.value());

        my_expected uut_val_to_err{};
        REQUIRE(uut_val_to_err.has_value() == true);
        uut_val_to_err = src_err;
        REQUIRE(uut_val_to_err.has_value() == false);
        REQUIRE(uut_val_to_err.error() == src_err.error());

        my_expected uut_err_to_err{turner::unexpected{other_error}};
        REQUIRE(uut_err_to_err.has_value() == false);
        uut_err_to_err = src_err;
        REQUIRE(uut_err_to_err.has_value() == false);
        REQUIRE(uut_err_to_err.error() == src_err.error());

        my_expected uut_err_to_val{turner::unexpected{other_error}};
        REQUIRE(uut_err_to_val.has_value() == false);
        uut_err_to_val = src_val;
        REQUIRE(uut_err_to_val.has_value() == true);
        REQUIRE(uut_err_to_val.value() == src_val.value());
    }

    SECTION("Move assignment") {

        my_expected src_val_to_val{sample_value};
        my_expected uut_val_to_val{};
        REQUIRE(uut_val_to_val.has_value() == true);
        uut_val_to_val = std::move(src_val_to_val);
        REQUIRE(uut_val_to_val.has_value() == true);
        REQUIRE(uut_val_to_val.value() == sample_value);
        REQUIRE(src_val_to_val.value().empty());    // NOLINT(bugprone-use-after-move)

        my_expected src_val_to_err{turner::unexpected{sample_error}};
        my_expected uut_val_to_err{};
        REQUIRE(uut_val_to_err.has_value() == true);
        uut_val_to_err = std::move(src_val_to_err);
        REQUIRE(uut_val_to_err.has_value() == false);
        REQUIRE(uut_val_to_err.error() == sample_error);
        REQUIRE(src_val_to_err.error().empty());    // NOLINT(bugprone-use-after-move)

        my_expected src_err_to_err{turner::unexpected{sample_error}};
        my_expected uut_err_to_err{turner::unexpected{other_error}};
        REQUIRE(uut_err_to_err.has_value() == false);
        uut_err_to_err = std::move(src_err_to_err);
        REQUIRE(uut_err_to_err.has_value() == false);
        REQUIRE(uut_err_to_err.error() == sample_error);
        REQUIRE(src_err_to_err.error().empty());    // NOLINT(bugprone-use-after-move)

        my_expected src_err_to_val{sample_value};
        my_expected uut_err_to_val{turner::unexpected{sample_error}};
        REQUIRE(uut_err_to_val.has_value() == false);
        uut_err_to_val = std::move(src_err_to_val);
        REQUIRE(uut_err_to_val.has_value() == true);
        REQUIRE(uut_err_to_val.value() == sample_value);
    }

    // TODO : Assign from an expected value
    // TODO : Assign from an unexpected value
}

// TODO : Monadic operations
// TODO : swap
// TODO : operator==
// TODO : All of the above TODOs for expected<void,...>

TEST_CASE ("turner::expected<void,...> constructors") {

    using value_type = void;
    using other_value_type = const void;
    using error_type = std::string_view;

    constexpr error_type sample_error = "Chicago Cubs";

    // Requirements of value_type for this test case
    static_assert(std::is_void_v<value_type>);
    static_assert(std::is_void_v<other_value_type>);
    static_assert(!std::is_same_v<value_type, other_value_type>);

    // Requirements of turner::unexpected value type
    static_assert(
        !std::is_array_v<error_type> &&
         std::is_object_v<error_type> &&
        !std::is_const_v<error_type> &&
        !std::is_volatile_v<error_type>
    );

    SECTION ("Default construction (1)") {
        turner::expected<value_type, error_type> uut{};
        REQUIRE(uut.has_value());
        REQUIRE(uut);       // operator bool
    }

    // Construct with expected value (6) not valid for this test case

    SECTION ("Construct with unexpected value lvalue (7)") {
        const turner::unexpected src{sample_error};
        turner::expected<value_type, error_type> uut{src};
        REQUIRE(uut.has_value() == false);
        REQUIRE(!uut);       // operator bool
        REQUIRE(uut.error() == sample_error);

        // Accessing value() should throw
        REQUIRE_THROWS_AS(uut.value(), turner::bad_expected_access<error_type>);
    }

    SECTION ("Construct with unexpected value rvalue (8)") {
        turner::expected<value_type, error_type> uut{turner::unexpected{sample_error}};
        REQUIRE(uut.has_value() == false);
        REQUIRE(!uut);       // operator bool
        REQUIRE(uut.error() == sample_error);

        // Accessing value() should throw
        REQUIRE_THROWS_AS(uut.value(), turner::bad_expected_access<error_type>);
    }

    SECTION ("Copy construction: source is expected (2)") {
        const turner::expected<value_type, error_type> src{};
        const turner::expected<value_type, error_type> uut{src};
        REQUIRE(uut.has_value());
        REQUIRE(src.has_value());
    }

    SECTION ("Copy construction: source is unexpected (2)") {
        const turner::expected<value_type, error_type> src{turner::unexpected{sample_error}};
        turner::expected<value_type, error_type> uut{src};
        REQUIRE(!uut.has_value());
        REQUIRE(!src.has_value());
        REQUIRE(uut.error() == src.error());
    }

    SECTION ("Move construction: source is expected (3)") {
        turner::expected<value_type, error_type> src{};
        const turner::expected<value_type, error_type> uut{std::move(src)};
        REQUIRE(uut.has_value());
    }

    SECTION ("Move construction: source is unexpected (3)") {
        turner::expected<value_type, error_type> src{turner::unexpected{sample_error}};
        turner::expected<value_type, error_type> uut{std::move(src)};
        REQUIRE(!uut.has_value());
        REQUIRE(uut.error() == sample_error);
    }

    SECTION ("Converting lvalue expected constructor with expected value (4)") {
        const turner::expected<value_type, int> src{};
        const turner::expected<other_value_type, int> uut{src};
        REQUIRE(uut.has_value());
    }

    SECTION ("Converting lvalue expected constructor with unexpected value (4)") {
        const turner::expected<value_type, std::string> src{turner::unexpected{"Some string value"}};
        turner::expected<value_type, std::string_view> uut{src};
        REQUIRE(uut.error() == src.error());
    }

    SECTION ("Converting rvalue expected constructor with expected value (5)") {
        turner::expected<value_type, int> src{};
        const turner::expected<value_type, double> uut{std::move(src)};
        REQUIRE(uut.has_value());
    }

    SECTION ("Converting rvalue expected constructor with unexpected value (5)") {
        constexpr auto* src_str = "Some string value";
        turner::expected<void,       std::string> src{turner::unexpected{src_str}};
        turner::expected<const void, std::string> uut{std::move(src)};
        REQUIRE(uut.error() == src_str);
        REQUIRE(src.error().empty());   // NOLINT(bugprone-use-after-move)
    }

    // In place construction of expected value (9) not valid for this test case
    // In place construction of expected value with a list (10) not valid for this test case

    SECTION ("In place construction of expected value (11)") {
        const turner::expected<value_type, error_type> uut{std::in_place};
        REQUIRE(uut.has_value());
    }

    SECTION ("In place construction of unexpected value (12)") {
        turner::expected<value_type, std::string> uut{turner::unexpect, std::size_t{5}, 'a'};
        REQUIRE(!uut.has_value());
        REQUIRE(uut.error() == "aaaaa");
    }

    SECTION ("In place construction of unexpected value with a list (13)") {
        turner::expected<value_type, std::string> uut{turner::unexpect, {'C', 'a', 'm', 'i', 'l', 'a'}};
        REQUIRE(!uut.has_value());
        REQUIRE(uut.error() == "Camila");
    }
}

TEST_CASE ("turner::expected<void, ...> construction (constexpr)") {

    using value_type = void;
    using error_type = std::string_view;
    constexpr error_type sample_error = "Miles Davis";

    // Requirements of value_type for this test case
    static_assert(std::is_void_v<value_type>);

    using my_expected = turner::expected<value_type, error_type>;

    // Used to ensure copy/move construction is deleted when appropriate
    struct NoCopyOrMove {
        NoCopyOrMove(const NoCopyOrMove&) = delete;
        NoCopyOrMove& operator=(const NoCopyOrMove&) = delete;
    };
    static_assert(!std::is_copy_constructible_v<NoCopyOrMove>);
    static_assert(!std::is_move_constructible_v<NoCopyOrMove>);

    // Default (1)
    static_assert(my_expected{}.has_value());

    // Copy constructor (2)
    constexpr my_expected copy_src{};
    static_assert(my_expected{copy_src}.has_value());
    static_assert(std::is_trivially_copy_constructible_v<turner::expected<void, int>>);
    static_assert(!std::is_trivially_copy_constructible_v<turner::expected<void, std::string>>);
    static_assert(!std::is_copy_constructible_v<turner::expected<value_type, NoCopyOrMove>>);

    // Move constructor (3)
    static_assert(my_expected{my_expected{}}.has_value());
    static_assert(std::is_trivially_move_constructible_v<turner::expected<void, int>>);
    static_assert(!std::is_trivially_move_constructible_v<turner::expected<void, std::string>>);
    static_assert(!std::is_move_constructible_v<turner::expected<value_type, NoCopyOrMove>>);

    // Converting expected lvalue (4)
    constexpr turner::expected<void, error_type> converting_lval{};
    static_assert(turner::expected<const void, error_type>{converting_lval}.has_value());

    // Converting expected rvalue (5)
    static_assert(turner::expected<const void, error_type>{
        turner::expected<void, error_type>{}}.has_value());

    // Construct from expected value (6) not valid for this test case

    // Construct from unexpected lvalue (7)
    constexpr turner::unexpected lval_err_src{sample_error};
    static_assert(my_expected{lval_err_src}.error() == sample_error);

    // Construct from unexpected rvalue (8)
    static_assert(my_expected{turner::unexpected{sample_error}}.error() == sample_error);

    class Foo {
        int     _a{};
        std::size_t  _sz{};
    public:
        constexpr Foo(int a) noexcept : _a(a) {}
        constexpr Foo(std::initializer_list<int> il) noexcept : _sz(il.size()) {}
        [[nodiscard]] constexpr int get() const noexcept { return _a; }
        [[nodiscard]] constexpr std::size_t size() const noexcept { return _sz; }
    };

    // In-place construct expected value (9) not valid for this test case
    // In-place construct expected value with list (10) not valid for this test case

    // In place construction of expected value (11)
    static_assert(my_expected{std::in_place}.has_value());

    // In-place construct unexpected value (12)
    static_assert(turner::expected<int, Foo>{turner::unexpect, 42}.error().get() == 42);

    // In-place construct unexpected value with list (13)
    static_assert(turner::expected<int, Foo>{turner::unexpect, {1,2,3,4,5}}.error().size() == 5);

    REQUIRE(true);  // If we compile, we pass!
}

TEST_CASE ("turner::expected<void,...> observers") {

    using value_type = void;
    using error_type = std::string;
    const error_type sample_error = "Ron Coomer";

    // Requirements of value_type for this test case
    static_assert(std::is_void_v<value_type>);

    using my_expected = turner::expected<value_type, error_type>;

    SECTION("operator*") {
        const my_expected uut{};
        *uut;           // Not very exciting
        REQUIRE(true);  // but we'll give it a point
    }

    SECTION("error()") {
        // E& and const E&
        my_expected uut_err{turner::unexpected{sample_error}};
        REQUIRE(const_cast<      my_expected&>(uut_err).error() == sample_error);
        REQUIRE(const_cast<const my_expected&>(uut_err).error() == sample_error);
        // T&&
        const auto moved_error = std::move(uut_err).error();
        REQUIRE(moved_error == sample_error);
        REQUIRE(uut_err.error().empty());  // NOLINT(bugprone-use-after-move)
        my_expected uut_err2{turner::unexpected{sample_error}};
        const auto not_moved_error = static_cast<const my_expected&&>(uut_err2).error();
        REQUIRE(not_moved_error  == sample_error);
        REQUIRE(uut_err2.error() == sample_error);
    }
}

// NOLINTEND(readability-function-cognitive-complexity)
