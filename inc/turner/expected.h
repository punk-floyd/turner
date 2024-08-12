/**
 * @file    expected.h
 * @author  Mike DeKoker (dekoker.mike@gmail.com)
 * @brief   A std::expected clone that we can use with C++20
 * @date    2024-02-17
 *
 * This class is based on: https://en.cppreference.com/w/cpp/utility/expected
 * and the C++23 standard draft (N4950).
 *
 * All entities are defined in the turner namespace. The idea is what when
 * we're in a position to build against C++23 we can just replace
 * turner::expected (et al.) with std::expected (et al.).
 *
 * It should be standard compliant with the following temporary exceptions:
 *  - We don't have the std::expected<void, ...> partial specialization.
 *
 * @copyright Copyright (c) 2024 Mike DeKoker
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
 */
#ifndef turner_expected_header_has_been_included
#define turner_expected_header_has_been_included

#include <initializer_list>
#include <type_traits>
#include <functional>
#include <exception>
#include <utility>
#include <memory>

// clang-tidy-17 gives false positives when noexcept specifications are
// based on std type traits.
//
// NOLINTBEGIN(performance-noexcept-swap)
// NOLINTBEGIN(performance-noexcept-move-constructor)
// NOLINTBEGIN(performance-noexcept-destructor)

namespace turner {

// Represented as an unexpected value
template <class E>
    requires (
        !std::is_array_v<E> && std::is_object_v<E> &&
        !std::is_const_v<E> && !std::is_volatile_v<E>)
class unexpected
{
public:

    using error_type = E;

    // -- Construction

    constexpr unexpected(const unexpected& other) = default;
    constexpr unexpected(unexpected&& other)      = default;

    template <class Err = E>
    constexpr explicit unexpected(Err&& e)
        requires (
            !std::is_same_v<std::remove_cvref_t<Err>, unexpected> &&
            !std::is_same_v<std::remove_cvref_t<Err>, std::in_place_t> &&
             std::is_constructible_v<E, Err>)
        : _error(std::forward<Err>(e))
    {}

    template <class... Args>
    constexpr explicit unexpected([[maybe_unused]] std::in_place_t tag, Args&&... args)
        requires (std::is_constructible_v<E, Args...>)
        : _error(std::forward<Args>(args)...)
    {}

    template <class U, class... Args>
    constexpr explicit unexpected([[maybe_unused]] std::in_place_t tag,
        std::initializer_list<U> il, Args&&... args)
        requires (std::is_constructible_v<E, std::initializer_list<U>&, Args...>)
        : _error(il, std::forward<Args>(args)...)
    {}

    // -- Access the stored value

    [[nodiscard]] constexpr const E&  error() const &  noexcept { return _error; }
    [[nodiscard]] constexpr       E&  error()       &  noexcept { return _error; }
    [[nodiscard]] constexpr const E&& error() const && noexcept { return std::move(_error); }
    [[nodiscard]] constexpr       E&& error()       && noexcept { return std::move(_error); }

    // -- Implementation

    constexpr unexpected& operator=(const unexpected&) = default;
    constexpr unexpected& operator=(unexpected&&) = default;

    constexpr void swap(unexpected& other) noexcept(std::is_nothrow_swappable_v<E>)
        requires (std::is_swappable_v<E>)
    {
        using std::swap;
        swap(error(), other.error());
    }

    friend constexpr void swap(unexpected& lhs, unexpected& rhs) noexcept(noexcept(lhs.swap(rhs)))
        requires (std::is_swappable_v<E>)
    {
        lhs.swap(rhs);
    }

    template <class E2>
    friend constexpr bool operator==(unexpected& lhs, unexpected<E2>& rhs)
    {
        return lhs.error() == rhs.error();
    }

private:

    error_type  _error;  ///< The unexpected value
};

/// The deduction guide is provided for unexpected to allow deduction from the constructor argument.
template <class E>
unexpected(E) -> unexpected<E>;

// A tag type for in-place construction of an unexpected value in an turner::expected object
struct unexpect_t {
    constexpr explicit unexpect_t() = default;
};
inline constexpr unexpect_t unexpect{};

template <class ErrorType>
class bad_expected_access;

template<>
class bad_expected_access<void> : public std::exception
{
protected:

    bad_expected_access() noexcept = default;
    bad_expected_access(const bad_expected_access&) = default;
    bad_expected_access(bad_expected_access&&) = default;

    ~bad_expected_access() override = default;

    bad_expected_access& operator=(const bad_expected_access&) = default;
    bad_expected_access& operator=(bad_expected_access&&) = default;

public:

    [[nodiscard]] const char* what() const noexcept override
    {
        return "Expected object does not have a value";
    }
};

template <class ErrorType>
class bad_expected_access : public bad_expected_access<void>
{
public:

    explicit bad_expected_access(ErrorType e) : _error(std::move(e)) {}

    [[nodiscard]] const ErrorType&  error() const &  noexcept { return _error; }
    [[nodiscard]]       ErrorType&  error()       &  noexcept { return _error; }
    [[nodiscard]] const ErrorType&& error() const && noexcept { return std::move(_error); }
    [[nodiscard]]       ErrorType&& error()       && noexcept { return std::move(_error); }

    [[nodiscard]] const char* what() const noexcept override
    {
        return "Expected object does not have a value";
    }

private:

    ErrorType _error;
};

template <class T, class E>
class expected;

namespace imp::exp {
    template <class T>
    constexpr bool is_expected = false;
    template <class T, class E>
    constexpr bool is_expected<expected<T, E>> = true;

    template <class E>
    constexpr bool is_unexpected = false;
    template <class E>
    constexpr bool is_unexpected<unexpected<E>> = true;

    template <class T>
        requires (std::is_nothrow_move_constructible_v<T>)
    class Guard {
    public:

        constexpr explicit Guard(T& obj)
            : _guarded(std::addressof(obj)), _temp(std::move(obj))
        {
            std::destroy_at(_guarded);
        }

        constexpr T&& release() noexcept
        {
            _guarded = nullptr;
            return std::move(_temp);
        }

        // -- Implementation

        constexpr ~Guard()
        {
            if (_guarded)
                std::construct_at(_guarded, std::move(_temp));
        }

        Guard(const Guard&) = delete;
        Guard& operator=(const Guard&) = delete;

    private:

        T* _guarded;
        T  _temp;
    };

    struct in_place_invoke {};
    struct unexpect_invoke {};

    template <class F, class T>
    using transform_result_t = std::remove_cv_t<std::invoke_result_t<F&&, T&&>>;

    template <class T, class E>
    static constexpr auto trivial_destructor =
        std::is_trivially_destructible_v<T> &&          // NOLINT(misc-redundant-expression)
        std::is_trivially_destructible_v<E>;

    template <class T, class E>
    static constexpr auto trivial_copy_cx_ok =
        std::is_trivially_copy_constructible_v<T> &&    // NOLINT(misc-redundant-expression)
        std::is_trivially_copy_constructible_v<E>;
    template <class T, class E>
    static constexpr auto copy_cx_ok =
        std::is_copy_constructible_v<T> &&              // NOLINT(misc-redundant-expression)
        std::is_copy_constructible_v<E>;

    template <class T, class E>
    static constexpr auto trivial_move_cx_ok =
        std::is_trivially_move_constructible_v<T> &&    // NOLINT(misc-redundant-expression)
        std::is_trivially_move_constructible_v<E>;
    template <class T, class E>
    static constexpr auto move_cx_ok =
        std::is_move_constructible_v<T> &&              // NOLINT(misc-redundant-expression)
        std::is_move_constructible_v<E>;
    template <class T, class E>
    static constexpr auto is_nothrow_move_cx =
        std::is_nothrow_move_constructible_v<T> &&      // NOLINT(misc-redundant-expression)
        std::is_nothrow_move_constructible_v<E>;

    template <class T, class E, class U, class G>
    static constexpr auto cx45_explicit =
        !std::is_convertible_v<std::add_lvalue_reference_t<const U>, T> || // NOLINT(misc-redundant-expression)
        !std::is_convertible_v<const G&, E>;

    template <class T, class E, class U, class G>
    static constexpr auto cx45_conv_check =
       (std::is_same_v<std::remove_cv_t<T>, bool> || (
            !std::is_constructible_v<T, expected<U, G>&> &&
            !std::is_constructible_v<T, expected<U, G> > &&
            !std::is_constructible_v<T, const expected<U, G>&> &&
            !std::is_constructible_v<T, const expected<U, G> > &&
            !std::is_convertible_v<      expected<U, G>&, T> &&
            !std::is_convertible_v<      expected<U, G>, T> &&
            !std::is_convertible_v<const expected<U, G>&, T> &&
            !std::is_convertible_v<const expected<U, G>, T>)) &&
        !std::is_constructible_v<unexpected<E>, expected<U, G>&> &&
        !std::is_constructible_v<unexpected<E>, expected<U, G> > &&
        !std::is_constructible_v<unexpected<E>, const expected<U, G>&> &&
        !std::is_constructible_v<unexpected<E>, const expected<U, G> >;

    template <class U> using cx4_UF = std::add_lvalue_reference_t<const U>;
    template <class G> using cx4_GF = const G&;

    template <class T, class E>
    static constexpr auto dx_noexcept =
        std::is_nothrow_destructible_v<T> && std::is_nothrow_destructible_v<E>; // NOLINT(misc-redundant-expression)

    template <class T, class E>
    static constexpr auto copy_assignable =
        (std::is_void_v<T> || std::is_copy_assignable_v<T>) &&
        (std::is_void_v<T> || std::is_copy_constructible_v<T>) &&
        std::is_copy_assignable_v<E> &&
        std::is_copy_constructible_v<E> && (            // NOLINT(misc-redundant-expression)
            std::is_void_v<T> ||
            std::is_nothrow_move_constructible_v<T> ||  // NOLINT(misc-redundant-expression)
            std::is_nothrow_move_constructible_v<E>);

    template <class T, class E>
    static constexpr auto move_assignable =
        (std::is_void_v<T> || std::is_move_assignable_v<T>) &&
        (std::is_void_v<T> || std::is_move_constructible_v<T>) &&
        std::is_move_assignable_v<E> &&
        std::is_move_constructible_v<E> && (            // NOLINT(misc-redundant-expression)
            std::is_void_v<T> ||
            std::is_nothrow_move_constructible_v<T> ||  // NOLINT(misc-redundant-expression)
            std::is_nothrow_move_constructible_v<E>);

    template <class T, class E>
    static constexpr auto move_assign_noexcept =
        (std::is_void_v<T> || std::is_nothrow_move_assignable_v<T>) &&
        (std::is_void_v<T> || std::is_nothrow_move_constructible_v<T>) &&
        std::is_nothrow_move_assignable_v<E> &&         // NOLINT(misc-redundant-expression)
        std::is_nothrow_move_constructible_v<E>;

    template <class T, class E, class U>
    static constexpr auto assign_from_expected =
        !std::is_same_v<expected<T,E>, std::remove_cvref_t<U>> &&
        !imp::exp::is_unexpected<std::remove_cvref_t<U>> &&
        std::is_constructible_v<T, U> &&
        std::is_assignable_v<T&, U> && (
            std::is_nothrow_constructible_v<T, U> ||
            std::is_nothrow_move_constructible_v<T> ||  // NOLINT(misc-redundant-expression)
            std::is_nothrow_move_constructible_v<E>);

    template <class T, class E, class GF>
    static constexpr auto assign_from_unexpected =
        std::is_constructible_v<E, GF> &&
        std::is_assignable_v<E&, GF> && (
            std::is_void_v<T> ||
            std::is_nothrow_constructible_v<E, GF> ||
            std::is_nothrow_move_constructible_v<T> ||
            std::is_nothrow_move_constructible_v<E>);

    template <class T, class E>
    static constexpr auto is_swappable =
        std::is_swappable_v<T> &&
        std::is_swappable_v<E> &&
        std::is_move_constructible_v<T> &&
        std::is_move_constructible_v<E> && (
            std::is_nothrow_move_constructible_v<T> ||
            std::is_nothrow_move_constructible_v<E>);

    template <class T, class E>
    static constexpr auto swap_noexcept =
        (std::is_void_v<T> || std::is_nothrow_move_constructible_v<T>) &&
        (std::is_void_v<T> || std::is_nothrow_swappable_v<T>) &&
        std::is_nothrow_move_constructible_v<E> &&
        std::is_nothrow_swappable_v<E>;
}

/// A wrapper that contains either an expected or error value
template <class T, class E>
class expected {
public:

    static_assert(!std::is_reference_v<T>);
    static_assert(!std::is_function_v<T>);
    static_assert(!imp::exp::is_unexpected<T>);
    static_assert(!std::is_same_v<T, std::in_place_t>);
    static_assert(!std::is_same_v<T, turner::unexpect_t>);

    using value_type = T;
    using error_type = E;
    using unexpected_type = turner::unexpected<E>;

    template <class U>
    using rebind = expected<U, error_type>;

    // -- Construction
    //    The (#) below refer to the cppreference constructor number

    /// Default construct with value-initialized value type
    constexpr expected()                                                    // (1)
        requires (std::is_default_constructible_v<T>)
    : _value(), _has_value(true)
    {}

    //      -- Copy

    // Trivial copy constructor
    constexpr expected(const expected& other)                               // (2)
        requires (imp::exp::trivial_copy_cx_ok<T,E>)
    = default;

    // Non-trivial copy constructor
    constexpr expected(const expected& other)                               // (2)
        requires (!imp::exp::trivial_copy_cx_ok<T,E> && imp::exp::copy_cx_ok<T,E>)
        : _has_value(other.has_value())
    {
        if (has_value())
            std::construct_at(std::addressof(_value), other.value());
        else
            std::construct_at(std::addressof(_error), other.error());
    }

    constexpr expected(const expected&)
        requires (!imp::exp::trivial_copy_cx_ok<T,E> && !imp::exp::copy_cx_ok<T,E>)
    = delete;

    //      -- Move

    // Trivial move constructor
    constexpr expected(expected&&)
        noexcept(imp::exp::is_nothrow_move_cx<T,E>)                         // (3)
        requires (imp::exp::trivial_move_cx_ok<T,E>)
    = default;

    // Non trivial move constructor
    constexpr expected(expected&& other)
        noexcept(imp::exp::is_nothrow_move_cx<T,E>)                         // (3)
        requires (!imp::exp::trivial_move_cx_ok<T,E> && imp::exp::move_cx_ok<T,E>)
        : _has_value(other.has_value())
    {
        if (has_value())
            std::construct_at(std::addressof(_value), std::move(other)._value);
        else
            std::construct_at(std::addressof(_error), std::move(other)._error);
    }

    expected(expected&&)
        requires (!imp::exp::trivial_move_cx_ok<T,E> && !imp::exp::move_cx_ok<T,E>)
    = delete;

    //      -- Converting (expected)

    // Converting expected constructor (lval)
    template <class U, class G>
        requires (
            std::is_constructible_v<T, imp::exp::cx4_UF<U>> &&
            std::is_constructible_v<E, imp::exp::cx4_GF<G>> &&
            imp::exp::cx45_conv_check<T,E,U,G>)
    constexpr explicit(imp::exp::cx45_explicit<T,E,U,G>)
    expected(const expected<U, G>& other)                                   // (4)
        : _has_value(other.has_value())
    {
        if (has_value())
            std::construct_at(std::addressof(_value), other.value());
        else
            std::construct_at(std::addressof(_error), other.error());
    }

    // Converting expected constructor (rval)
    template <class U, class G>
        requires (
            std::is_constructible_v<T, U>&&
            std::is_constructible_v<E, G>&&
            imp::exp::cx45_conv_check<T, E, U, G>)
    constexpr explicit(imp::exp::cx45_explicit<T,E,U,G>)
    expected(expected<U, G>&& other)                                        // (5)
        : _has_value(other.has_value())
    {
        if (has_value())
            std::construct_at(std::addressof(_value), std::move(other)._value);
        else
            std::construct_at(std::addressof(_error), std::move(other)._error);
    }

    // Construct expected value by direct-initialization of an object
    template <class U = T>
        requires (
            !std::is_same_v<std::remove_cvref_t<U>, std::in_place_t> &&
            !std::is_same_v<std::remove_cvref_t<U>, expected> &&
            std::is_constructible_v<T, U> &&
            !imp::exp::is_unexpected<std::remove_cvref_t<U>> && (
                !std::is_same_v<std::remove_cv_t<T>, bool> ||
                !imp::exp::is_expected<std::remove_cvref_t<U>>)
        )
    constexpr explicit(!std::is_convertible_v<U, T>) expected(U&& v)        // (6)
        : _value(std::forward<U>(v)), _has_value(true)
    {}

    // Construct unexpected value by direct-initialization of an lval reference
    template <class G>
        requires (std::is_constructible_v<E, const G&>)
    constexpr explicit(!std::is_convertible_v<const G&, E>)
    expected(const unexpected<G>& e)                                        // (7)
        : _error(e.error()), _has_value(false)
    {}

    // Construct unexpected value, which is direct-initialized from the argument
    template <class G>
        requires (std::is_constructible_v<E, G>)
    constexpr explicit(!std::is_convertible_v<G, E>)
    expected(unexpected<G>&& e)                                             // (8)
        : _error(std::move(e).error()), _has_value(false)
    {}

    // Construct expected value, which is direct-initialized from the arguments
    template <class... Args>
        requires (std::is_constructible_v<T, Args...>)
    constexpr explicit expected([[maybe_unused]] std::in_place_t tag, Args&&... args) // (9)
        : _value(std::forward<Args>(args)...), _has_value(true)
    {}

    // Construct expected value, which is direct-initialized from the list of arguments
    template <class U, class... Args>
        requires (std::is_constructible_v<T, std::initializer_list<U>&, Args...>)
    constexpr explicit expected([[maybe_unused]] std::in_place_t tag,       // (10)
        std::initializer_list<U> il, Args&&... args)
        : _value(il, std::forward<Args>(args)...), _has_value(true)
    {}

    // (11) is only for std::is_void_v<value_type>

    // Construct unexpected value, which is direct-initialized from the arguments
    template <class... Args>
        requires (std::is_constructible_v<E, Args...>)
    constexpr explicit expected([[maybe_unused]] turner::unexpect_t tag, Args&&... args) // (12)
        : _error(std::forward<Args>(args)...), _has_value(false)
    {}

    // Construct unexpected value, which is direct-initialized from the list of arguments
    template <class U, class... Args>
        requires (std::is_constructible_v<E, std::initializer_list<U>&, Args...>)
    constexpr explicit expected([[maybe_unused]] turner::unexpect_t tag,       // (13)
        std::initializer_list<U> il, Args&&... args)
        : _error(il, std::forward<Args>(args)...), _has_value(false)
    {}

private:

    // These constructors used by transform and transform_error

    template <class TOther, class EOther>
    friend class expected;

    using in_place_invoke = imp::exp::in_place_invoke;
    using unexpect_invoke = imp::exp::unexpect_invoke;

    template <class F, class Arg>
    constexpr expected([[maybe_unused]] in_place_invoke tag, F&& f, Arg&& arg)
        : _value(std::invoke(std::forward<F>(f), std::forward<Arg>(arg)))
        , _has_value(true)
    {}

    template <class F, class Arg>
    explicit constexpr expected([[maybe_unused]] unexpect_invoke tag, F&& f, Arg&& arg)
        : _error(std::invoke(std::forward<F>(f), std::forward<Arg>(arg)))
        , _has_value(false)
    {}

 public:

    // -- Observers

    constexpr const T* operator->() const noexcept { return &_value; }
    constexpr       T* operator->()       noexcept { return &_value; }

    constexpr const T&  operator*() const &  noexcept { return _value; }
    constexpr       T&  operator*()       &  noexcept { return _value; }
    constexpr const T&& operator*() const && noexcept { return std::move(_value); }
    constexpr       T&& operator*()       && noexcept { return std::move(_value); }

    [[nodiscard]] constexpr const E&  error() const &  noexcept { return _error; }
    [[nodiscard]] constexpr       E&  error()       &  noexcept { return _error; }
    [[nodiscard]] constexpr const E&& error() const && noexcept { return std::move(_error); }
    [[nodiscard]] constexpr       E&& error()       && noexcept { return std::move(_error); }

    [[nodiscard]] constexpr T& value()&
    {
        if (!has_value())
            throw bad_expected_access(std::as_const(error()));

        return _value;
    }

    [[nodiscard]] constexpr const T& value() const &
    {
        if (!has_value())
            throw bad_expected_access(std::as_const(error()));

        return _value;
    }

    [[nodiscard]] constexpr T&& value() &&
    {
        if (!has_value())
            throw bad_expected_access(std::move(_error));

        return std::move(_value);
    }

    [[nodiscard]] constexpr const T&& value() const &&
    {
        if (!has_value())
            throw bad_expected_access(std::move(_error));

        return std::move(_value);
    }

    constexpr explicit operator bool() const noexcept
    {
        return has_value();
    }

    [[nodiscard]] constexpr bool has_value() const noexcept
    {
        return _has_value;
    }

    template <class U>
        requires (std::is_convertible_v<U, T> && std::is_copy_constructible_v<T>)
    [[nodiscard]] constexpr T value_or(U&& default_value) const&
    {
        return has_value() ? **this :
            static_cast<T>(std::forward<U>(default_value));
    }

    template <class U>
        requires (std::is_convertible_v<U, T> && std::is_move_constructible_v<T>)
    constexpr T value_or(U&& default_value)&&
    {
        return has_value() ? std::move(**this) :
            static_cast<T>(std::forward<U>(default_value));
    }

    // -- Monadic operations

    //      -- and_then

    // Returns the result of the given function on expected value, or the expected itself otherwise
    template <class F>
        requires (std::is_constructible_v<E, E&>)
    constexpr auto and_then(F&& f) &
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F, T&>>;

        static_assert(imp::exp::is_expected<U>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename U::error_type, E>,
            "Callable must return turner::expected with the same error type");

        if (has_value())
            return std::invoke(std::forward<F>(f), _value);

        return U{ turner::unexpect, error() };
    }

    // Returns the result of the given function on expected value, or the expected itself otherwise
    template <class F>
        requires (std::is_constructible_v<E, const E&>)
    constexpr auto and_then(F&& f) const &
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F, const T&>>;

        static_assert(imp::exp::is_expected<U>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename U::error_type, E>,
            "Callable must return turner::expected with the same error type");

        if (has_value()) {
            return std::invoke(std::forward<F>(f), _value);
        }

        return U{ turner::unexpect, error() };
    }

    template <class F>
        requires (std::is_constructible_v<E, E>)
    constexpr auto and_then(F&& f)&&
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F, T&&>>;

        static_assert(imp::exp::is_expected<U>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename U::error_type, E>,
            "Callable must return turner::expected with the same error type");

        if (has_value()) {
            return std::invoke(std::forward<F>(f), std::move(_value));
        }

        return U{turner::unexpect, std::move(_error)};
    }

    template <class F>
        requires (std::is_constructible_v<E, const E>)
    constexpr auto and_then(F&& f) const &&
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F, const T&&>>;

        static_assert(imp::exp::is_expected<U>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename U::error_type, E>,
            "Callable must return turner::expected with the same error type");

        if (has_value()) {
            return std::invoke(std::forward<F>(f), std::move(_value));
        }

        return U{ turner::unexpect, std::move(_error) };
    }

    //      -- or_else

    template <class F>
        requires (std::is_constructible_v<T, T&>)
    constexpr auto or_else(F&& f) &
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, E&>>;

        static_assert(imp::exp::is_expected<G>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename G::value_type, value_type>,
            "Callable must return turner::expected with the same value type");

        if (has_value())
            return G{std::in_place, _value};

        return std::invoke(std::forward<F>(f), _error);
    }

    template <class F>
        requires (std::is_constructible_v<T, const T&>)
    constexpr auto or_else(F&& f) const &
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, const E&>>;

        static_assert(imp::exp::is_expected<G>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename G::value_type, value_type>,
            "Callable must return turner::expected with the same value type");

        if (has_value()) {
            return G{ std::in_place, _value };
        }

        return std::invoke(std::forward<F>(f), _error);
    }

    template <class F>
        requires (std::is_constructible_v<T, T>)
    constexpr auto or_else(F&& f) &&
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, E&&>>;

        static_assert(imp::exp::is_expected<G>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename G::value_type, value_type>,
            "Callable must return turner::expected with the same value type");

        if (has_value()) {
            return G{ std::in_place, std::move(_value) };
        }

        return std::invoke(std::forward<F>(f), std::move(_error));
    }

    template <class F>
        requires (std::is_constructible_v<T, const T>)
    constexpr auto or_else(F&& f) const &&
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, const E&&>>;

        static_assert(imp::exp::is_expected<G>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename G::value_type, value_type>,
            "Callable must return turner::expected with the same value type");

        if (has_value()) {
            return G{ std::in_place, std::move(_value) };
        }

        return std::invoke(std::forward<F>(f), std::move(_error));
    }

    //      -- transform

    template <class F>
        requires (std::is_constructible_v<E, E&>)
    constexpr auto transform(F&& func) &
    {
        using ResultType = expected<imp::exp::transform_result_t<F, T&>, E>;

        if (has_value()) {
            return ResultType{ in_place_invoke{},
                std::forward<F>(func), _value };
        }

        return ResultType{ unexpect, _error };
    }

    template <class F>
        requires (std::is_constructible_v<E, const E&>)
    constexpr auto transform(F&& func) const&
    {
        using ResultType = expected<imp::exp::transform_result_t<F, const T&>, E>;

        if (has_value()) {
            return ResultType{ in_place_invoke{},
                std::forward<F>(func), _value };
        }

        return ResultType{ unexpect, _error };
    }

    template <class F>
        requires (std::is_constructible_v<E, E>)
    constexpr auto transform(F&& func) &&
    {
        using ResultType = expected<imp::exp::transform_result_t<F, T>, E>;

        if (has_value()) {
            return ResultType{ in_place_invoke{},
                std::forward<F>(func), std::move(_value) };
        }

        return ResultType{unexpect, std::move(_error)};
    }

    template <class F>
        requires (std::is_constructible_v<E, const E>)
    constexpr auto transform(F&& func) const&&
    {
        using ResultType = expected<imp::exp::transform_result_t<F, const T>, E>;

        if (has_value()) {
            return ResultType{ in_place_invoke{},
                std::forward<F>(func), std::move(_value) };
        }

        return ResultType{ unexpect, std::move(_error) };
    }

    //      -- transform_error

    template <class F>
        requires (std::is_constructible_v<T, T&>)
    constexpr auto transform_error(F&& func) &
    {
        using ResultType = expected<T, imp::exp::transform_result_t<F, E&>>;

        if (has_value())
            return ResultType{ std::in_place, _value };

        return ResultType{ unexpect_invoke{}, std::forward<F>(func), _error };
    }

    template <class F>
        requires (std::is_constructible_v<T, const T&>)
    constexpr auto transform_error(F&& func) const &
    {
        using ResultType = expected<T, imp::exp::transform_result_t<F, const E&>>;

        if (has_value())
            return ResultType{ std::in_place, _value };

        return ResultType{ unexpect_invoke{}, std::forward<F>(func), _error };
    }

    template <class F>
        requires (std::is_constructible_v<T, T>)
    constexpr auto transform_error(F&& func) &&
    {
        using ResultType = expected<T, imp::exp::transform_result_t<F, E>>;

        if (has_value())
            return ResultType{ std::in_place, std::move(_value) };

        return ResultType{ unexpect_invoke{},
            std::forward<F>(func), std::move(_error) };
    }

    template <class F>
        requires (std::is_constructible_v<T, const T>)
    constexpr auto transform_error(F&& func) const &&
    {
        using ResultType = expected<T, imp::exp::transform_result_t<F, const E>>;

        if (has_value())
            return ResultType{ std::in_place, std::move(_value) };

        return ResultType{ unexpect_invoke{},
            std::forward<F>(func), std::move(_error) };
    }

    // -- Modifiers

    // Construct an expected value in-place
    template <class... Args>
    constexpr T& emplace(Args&&... args) noexcept
        requires (std::is_nothrow_constructible_v<T, Args...>)
    {
        if (has_value())
            std::destroy_at(std::addressof(_value));
        else {
            std::destroy_at(std::addressof(_error));
            _has_value = true;
        }

        return *std::construct_at(std::addressof(_value), std::forward<Args>(args)...);
    }

    // Construct an expected value in-place (list)
    template <class U, class... Args>
        requires (std::is_nothrow_constructible_v<T, std::initializer_list<U>&, Args...>)
    constexpr T& emplace(std::initializer_list<U> il, Args&&... args) noexcept
    {
        if (has_value())
            std::destroy_at(std::addressof(_value));
        else {
            std::destroy_at(std::addressof(_error));
            _has_value = true;
        }

        return *std::construct_at(std::addressof(_value), il, std::forward<Args>(args)...);
    }

    // -- Implementation

private:

    template <class NewType, class OldType, class... Args>
    constexpr void reinit_expected(NewType& new_val, OldType& old_val, Args&&... args)
    {
        if constexpr (std::is_nothrow_constructible_v<NewType, Args...>) {
            std::destroy_at(std::addressof(old_val));
            std::construct_at(std::addressof(new_val), std::forward<Args>(args)...);
        }
        else if constexpr (std::is_nothrow_move_constructible_v<NewType>)
        {
            NewType temp(std::forward<Args>(args)...);
            std::destroy_at(std::addressof(old_val));
            std::construct_at(std::addressof(new_val), std::move(temp));
        }
        else {
            imp::exp::Guard<OldType> guard(std::addressof(old_val));
            std::construct_at(std::addressof(new_val), std::forward<Args>(args)...); // Might throw
            guard.release();
        }
    }

public:

    constexpr ~expected() noexcept(imp::exp::dx_noexcept<T,E>)
        requires (!imp::exp::trivial_destructor<T,E>)
    {
        if (has_value())
            std::destroy_at(std::addressof(_value));
        else
            std::destroy_at(std::addressof(_error));
    }

    constexpr ~expected() noexcept(imp::exp::dx_noexcept<T,E>)
        requires (imp::exp::trivial_destructor<T,E>)
    = default;

    // Copy assignment
    constexpr expected& operator=(const expected& other)
        requires(imp::exp::copy_assignable<T,E>)
    {
        const auto this_valued = has_value();
        const auto that_valued = other.has_value();

        if (this_valued && that_valued)
            _value = *other;
        else if (!this_valued && !that_valued)
            _error = other.error();
        else if (this_valued) {
            reinit_expected(_error, _value, other.error());  // Copy other error
            _has_value = false;
        }
        else {
            reinit_expected(_value, _error, other.value());  // Copy other value
            _has_value = true;
        }

        return *this;
    }

    constexpr expected& operator=(const expected& other)
        requires(!imp::exp::copy_assignable<T,E>)
    = delete;

    // Move assignment
    constexpr expected& operator=(expected&& other) noexcept(imp::exp::move_assign_noexcept<T,E>)
        requires (imp::exp::move_assignable<T,E>)
    {
        const auto this_valued = has_value();
        const auto that_valued = other.has_value();

        if (this_valued && that_valued)
            _value = std::move(*other);
        else if (!this_valued && !that_valued)
            _error = std::move(other._error);
        else if (this_valued) {
            reinit_expected(_error, _value, std::move(other._error));
            _has_value = false;
        }
        else {
            reinit_expected(_value, _error, std::move(*other));
            _has_value = true;
        }

        return *this;
    }

    // Assign from an expected value
    template <class U = T>
        requires (imp::exp::assign_from_expected<T,E,U>)
    constexpr expected& operator=(U&& v)
    {
        if (has_value())
            _value = std::forward<U>(v);
        else {
            reinit_expected(_value, _error, std::forward<U>(v));
            _has_value = true;
        }

        return *this;
    }

    // Assign from an unexpected value
    template <class G>
    constexpr expected& operator=(const unexpected<G>& other)
        requires (imp::exp::assign_from_unexpected<T, E, const G&>)
    {
        if (has_value()) {
            reinit_expected(_error, _value, std::forward<const G&>(other.error()));
            _has_value = false;
        }
        else
            _error = std::forward<const G&>(other.error());

        return *this;
    }

    // Assign from an unexpected value
    template <class G>
    constexpr expected& operator=(unexpected<G>&& other)
        requires (imp::exp::assign_from_unexpected<T, E, G>)
    {
        if (has_value()) {
            reinit_expected(_error, _value, std::forward<G>(other.error));
            _has_value = false;
        }
        else
            _error = std::forward<G>(other.error());

        return *this;
    }

    /// Swap the contents with those of other
    constexpr void swap(expected& other) noexcept(imp::exp::swap_noexcept<T,E>)
        requires (imp::exp::is_swappable<T,E>)
    {
        const auto this_valued = has_value();
        const auto that_valued = other.has_value();

        if (this_valued && that_valued) {
            using std::swap;
            swap(_value, other._value);
            return;
        }

        if (!this_valued && !that_valued) {
            using std::swap;
            swap(_error, other._error);
            return;
        }

        if (that_valued) {
            other.swap(*this);  // Do all the stuff below...
            return;
        }

        if constexpr (std::is_nothrow_move_constructible_v<E>) {
            imp::exp::Guard<E> guard(other._error);
            std::construct_at(std::addressof(other._value), std::move(_value)); // Might throw
            other._has_value = true;
            std::destroy_at(std::addressof(_value));
            std::construct_at(std::addressof(_error), guard.release());
            _has_value = false;
        }
        else {
            imp::exp::Guard<T> guard(_value);
            std::construct_at(std::addressof(_error), std::move(other._error)); // Might throw
            _has_value = false;
            std::destroy_at(std::addressof(other._error));
            std::construct_at(std::addressof(other._error), guard.release());
            other._has_value = true;
        }
    }

    friend constexpr void swap(expected& x, expected& y) noexcept(noexcept(x.swap(y)))
    {
        x.swap(y);
    }

    /// Compares two expected objects (T2 is non-void)
    template <class T2, class E2>
        requires (!std::is_void_v<T2>)
    friend constexpr bool operator==(const expected& lhs, const expected<T2, E2>& rhs)
    {
        const auto lhs_valued = lhs.has_value();
        const auto rhs_valued = rhs.has_value();

        if (lhs_valued && rhs_valued)
            return *lhs == *rhs;
        if (!lhs_valued && !rhs_valued)
            return lhs.error() == rhs.error();

        return false;
    }

    /// Compares two expected objects (T2 is void)
    template <class T2, class E2>
        requires (std::is_void_v<T2>)
    friend constexpr bool operator==(const expected& lhs, const expected<T2, E2>& rhs)
    {
        const auto lhs_valued = lhs.has_value();
        const auto rhs_valued = rhs.has_value();

        if (lhs_valued == rhs_valued)
            return lhs_valued ? true : (lhs.error() == rhs.error());

        return false;
    }

    /// Compares expected object with a value
    template <class T2>
        requires (!std::is_void_v<T>)
    friend constexpr bool operator==(const expected& x, const T2& val)
    {
        return x.has_value() && static_cast<bool>(*x == val);
    }

    /// Compares expected object with an unexpected value
    template <class E2>
    friend constexpr bool operator==(const expected& x, const unexpected<E2>& e)
    {
        return !x.has_value() && static_cast<bool>(x.error() == e.error());
    }

private:

    // -- Member data

    union {
        value_type  _value;
        error_type  _error;
    };
    bool            _has_value;
};

// Partial specialization for void
template <class VoidType, class E>
    requires (std::is_void_v<VoidType>)
class expected<VoidType, E> {
public:

    using value_type = VoidType;
    using error_type = E;
    using unexpected_type = turner::unexpected<E>;

    template <class U>
    using rebind = expected<U, error_type>;

    // -- Construction
    //    The (#) below refer to the cppreference constructor number

    /// Default construct with value-initialized value type
    constexpr expected()                                                    // (1)
        : _no_value(), _has_value(true)
    {}

    // -- Copy

    // Trivial copy constructor
    constexpr expected(const expected& other)                               // (2)
        requires (std::is_trivially_copy_constructible_v<E>)
    = default;

    // Non-trivial copy constructor
    constexpr expected(const expected& other)                               // (2)
        requires (std::is_copy_constructible_v<E> && !std::is_trivially_copy_constructible_v<E>)
        : _has_value(other.has_value())
    {
        if (has_value())
            std::construct_at(std::addressof(_no_value));
        else
            std::construct_at(std::addressof(_error), other.error());
    }

    expected(const expected& other)
        requires (!std::is_copy_constructible_v<E> && !std::is_trivially_copy_constructible_v<E>)
    = delete;

    //      -- Move

    // Trivial move constructor
    constexpr expected(expected&&)
        noexcept(std::is_nothrow_move_constructible_v<E>)                   // (3)
        requires (std::is_trivially_move_constructible_v<E>)
    = default;

    // Non-trivial move constructor
    constexpr expected(expected&& other)
        noexcept(std::is_nothrow_move_constructible_v<E>)                   // (3)
        requires(std::is_move_constructible_v<E> && !std::is_trivially_move_constructible_v<E>)
        : _has_value(other.has_value())
    {
        if (has_value())
            std::construct_at(std::addressof(_no_value));
        else
            std::construct_at(std::addressof(_error), std::move(other)._error);
    }

    expected(expected&&)
        requires(!std::is_move_constructible_v<E> && !std::is_trivially_move_constructible_v<E>)
    = delete;

    //      -- Converting (expected)

    // Converting expected constructor (lval)
    template <class U, class G>
        requires (
            std::is_void_v<U> &&
            std::is_constructible_v<E, imp::exp::cx4_GF<G>> &&
            !std::is_convertible_v<      expected<U, G>&, value_type> &&
            !std::is_convertible_v<      expected<U, G>,  value_type> &&
            !std::is_convertible_v<const expected<U, G>&, value_type> &&
            !std::is_convertible_v<const expected<U, G>,  value_type> &&
            !std::is_constructible_v<unexpected<E>, expected<U, G>&>  &&
            !std::is_constructible_v<unexpected<E>, expected<U, G>>   &&
            !std::is_constructible_v<unexpected<E>, const expected<U, G>&> &&
            !std::is_constructible_v<unexpected<E>, const expected<U, G>>)
    constexpr explicit(std::is_convertible_v<imp::exp::cx4_GF<G>, E>)
    expected(const expected<U, G>& other)                                   // (4)
        : _has_value(other.has_value())
    {
        if (has_value())
            std::construct_at(std::addressof(_no_value));
        else
            std::construct_at(std::addressof(_error), other.error());
    }

    // Converting expected constructor (rval)
    template <class U, class G>
        requires (
            std::is_void_v<U> &&
            std::is_constructible_v<E, G> &&
            !std::is_convertible_v<      expected<U, G>&, value_type> &&
            !std::is_convertible_v<      expected<U, G>,  value_type> &&
            !std::is_convertible_v<const expected<U, G>&, value_type> &&
            !std::is_convertible_v<const expected<U, G>,  value_type> &&
            !std::is_constructible_v<unexpected<E>, expected<U, G>&>  &&
            !std::is_constructible_v<unexpected<E>, expected<U, G>>   &&
            !std::is_constructible_v<unexpected<E>, const expected<U, G>&> &&
            !std::is_constructible_v<unexpected<E>, const expected<U, G>>)
    constexpr explicit(!std::is_convertible_v<G, E>)
    expected(expected<U, G>&& other)                                        // (5)
        : _has_value(other.has_value())
    {
        if (has_value())
            std::construct_at(std::addressof(_no_value));
        else
            std::construct_at(std::addressof(_error), std::move(other)._error);
    }

    // (6) is not valid for std::is_void_v<value_type>

    // Construct unexpected value by direct-initialization of an lval reference
    template <class G>
        requires (std::is_constructible_v<E, const G&>)
    constexpr explicit(!std::is_convertible_v<const G&, E>)
    expected(const unexpected<G>& e)                                        // (7)
        : _error(e.error()), _has_value(false)
    {}

    // Construct unexpected value, which is direct-initialized from the argument
    template <class G>
        requires (std::is_constructible_v<E, G>)
    constexpr explicit(!std::is_convertible_v<G, E>)
    expected(unexpected<G>&& e)                                             // (8)
        : _error(std::move(e).error()), _has_value(false)
    {}

    // (9) is not valid for std::is_void_v<value_type>
    // (10) is not valid for std::is_void_v<value_type>

    // In place construction of expected void value
    constexpr explicit expected([[maybe_unused]] std::in_place_t tag) noexcept // (11)
        : _no_value(), _has_value(true)
    {}

    // Construct unexpected value, which is direct-initialized from the arguments
    template <class... Args>
        requires (std::is_constructible_v<E, Args...>)
    constexpr explicit expected([[maybe_unused]] turner::unexpect_t tag, Args&&... args) // (12)
        : _error(std::forward<Args>(args)...), _has_value(false)
    {}

    // Construct unexpected value, which is direct-initialized from the list of arguments
    template <class U, class... Args>
        requires (std::is_constructible_v<E, std::initializer_list<U>&, Args...>)
    constexpr explicit expected([[maybe_unused]] turner::unexpect_t tag,       // (13)
        std::initializer_list<U> il, Args&&... args)
        : _error(il, std::forward<Args>(args)...), _has_value(false)
    {}

private:

    // These constructors used by transform and transform_error

    template <class TOther, class EOther>
    friend class expected;

    using in_place_invoke = imp::exp::in_place_invoke;
    using unexpect_invoke = imp::exp::unexpect_invoke;

    template <class F>
    constexpr expected([[maybe_unused]] in_place_invoke tag, F&& f)
        : _no_value(), _has_value(true)
    {
        std::invoke(std::forward<F>(f));
    }

    template <class F, class Arg>
    explicit constexpr expected([[maybe_unused]] unexpect_invoke tag, F&& f, Arg&& arg)
        : _error(std::invoke(std::forward<F>(f), std::forward<Arg>(arg)))
        , _has_value(false)
    {}

 public:

    // -- Observers

    constexpr void operator*() const noexcept {}

    [[nodiscard]] constexpr const E&  error() const &  noexcept { return _error; }
    [[nodiscard]] constexpr       E&  error()       &  noexcept { return _error; }
    [[nodiscard]] constexpr const E&& error() const && noexcept { return std::move(_error); }
    [[nodiscard]] constexpr       E&& error()       && noexcept { return std::move(_error); }

    constexpr void value() const&
    {
        if (!has_value())
            throw bad_expected_access(std::as_const(error()));
    }
    constexpr void value() &&
    {
        if (!has_value())
            throw bad_expected_access(std::move(_error));
    }

    constexpr explicit operator bool() const noexcept
    {
        return has_value();
    }

    [[nodiscard]] constexpr bool has_value() const noexcept
    {
        return _has_value;
    }

    // -- Monadic operations

    //      -- and_then

    // Returns the result of the given function on expected value, or the expected itself otherwise
    template <class F>
        requires (std::is_constructible_v<E, E&>)
    constexpr auto and_then(F&& f) &
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F>>;

        static_assert(imp::exp::is_expected<U>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename U::error_type, E>,
            "Callable must return turner::expected with the same error type");

        if (has_value())
            return std::invoke(std::forward<F>(f));

        return U{ turner::unexpect, error() };
    }

    // Returns the result of the given function on expected value, or the expected itself otherwise
    template <class F>
        requires (std::is_constructible_v<E, const E&>)
    constexpr auto and_then(F&& f) const &
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F>>;

        static_assert(imp::exp::is_expected<U>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename U::error_type, E>,
            "Callable must return turner::expected with the same error type");

        if (has_value())
            return std::invoke(std::forward<F>(f));

        return U{ turner::unexpect, error() };
    }

    template <class F>
        requires (std::is_constructible_v<E, E>)
    constexpr auto and_then(F&& f)&&
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F>>;

        static_assert(imp::exp::is_expected<U>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename U::error_type, E>,
            "Callable must return turner::expected with the same error type");

        if (has_value())
            return std::invoke(std::forward<F>(f));

        return U{turner::unexpect, std::move(_error)};
    }

    template <class F>
        requires (std::is_constructible_v<E, const E>)
    constexpr auto and_then(F&& f) const &&
    {
        using U = std::remove_cvref_t<std::invoke_result_t<F>>;

        static_assert(imp::exp::is_expected<U>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename U::error_type, E>,
            "Callable must return turner::expected with the same error type");

        if (has_value())
            return std::invoke(std::forward<F>(f));

        return U{ turner::unexpect, std::move(_error) };
    }

    //      -- or_else

    template <class F>
    constexpr auto or_else(F&& f) &
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, E&>>;

        static_assert(imp::exp::is_expected<G>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename G::value_type, value_type>,
            "Callable must return turner::expected with the same value type");

        if (has_value())
            return G();

        return std::invoke(std::forward<F>(f), _error);
    }

    template <class F>
    constexpr auto or_else(F&& f) const &
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, const E&>>;

        static_assert(imp::exp::is_expected<G>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename G::value_type, value_type>,
            "Callable must return turner::expected with the same value type");

        if (has_value())
            return G();

        return std::invoke(std::forward<F>(f), _error);
    }

    template <class F>
    constexpr auto or_else(F&& f) &&
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, E&&>>;

        static_assert(imp::exp::is_expected<G>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename G::value_type, value_type>,
            "Callable must return turner::expected with the same value type");

        if (has_value())
            return G();

        return std::invoke(std::forward<F>(f), std::move(_error));
    }

    template <class F>
    constexpr auto or_else(F&& f) const &&
    {
        using G = std::remove_cvref_t<std::invoke_result_t<F, const E&&>>;

        static_assert(imp::exp::is_expected<G>,
            "Callable must return turner::expected");
        static_assert(std::is_same_v<typename G::value_type, value_type>,
            "Callable must return turner::expected with the same value type");

        if (has_value())
            return G();

        return std::invoke(std::forward<F>(f), std::move(_error));
    }

    //      -- transform

    template <class F>
        requires (std::is_constructible_v<E, E&>)
    constexpr auto transform(F&& func) &
    {
        using ResultType = std::remove_cv_t<std::invoke_result_t<F>>;

        if (has_value())
            return ResultType{ in_place_invoke{}, std::forward<F>(func) };

        return ResultType{ unexpect, _error };
    }

    template <class F>
        requires (std::is_constructible_v<E, const E&>)
    constexpr auto transform(F&& func) const&
    {
        using ResultType = std::remove_cv_t<std::invoke_result_t<F>>;

        if (has_value())
            return ResultType{ in_place_invoke{}, std::forward<F>(func) };

        return ResultType{ unexpect, _error };
    }

    template <class F>
        requires (std::is_constructible_v<E, E>)
    constexpr auto transform(F&& func) &&
    {
        using ResultType = std::remove_cv_t<std::invoke_result_t<F>>;

        if (has_value())
            return ResultType{ in_place_invoke{}, std::forward<F>(func) };

        return ResultType{unexpect, std::move(_error)};
    }

    template <class F>
        requires (std::is_constructible_v<E, const E>)
    constexpr auto transform(F&& func) const&&
    {
        using ResultType = std::remove_cv_t<std::invoke_result_t<F>>;

        if (has_value())
            return ResultType{ in_place_invoke{}, std::forward<F>(func) };

        return ResultType{ unexpect, std::move(_error) };
    }

    //      -- transform_error

    template <class F>
    constexpr auto transform_error(F&& func) &
    {
        using ResultType =
            expected<value_type, imp::exp::transform_result_t<F, E&>>;

        if (has_value())
            return ResultType{};

        return ResultType{ unexpect_invoke{}, std::forward<F>(func), _error };
    }

    template <class F>
    constexpr auto transform_error(F&& func) const &
    {
        using ResultType =
            expected<value_type, imp::exp::transform_result_t<F, const E&>>;

        if (has_value())
            return ResultType{};

        return ResultType{ unexpect_invoke{}, std::forward<F>(func), _error };
    }

    template <class F>
    constexpr auto transform_error(F&& func) &&
    {
        using ResultType =
            expected<value_type, imp::exp::transform_result_t<F, E>>;

        if (has_value())
            return ResultType{};

        return ResultType{ unexpect_invoke{},
            std::forward<F>(func), std::move(_error) };
    }

    template <class F>
    constexpr auto transform_error(F&& func) const &&
    {
        using ResultType =
            expected<value_type, imp::exp::transform_result_t<F, const E>>;

        if (has_value())
            return ResultType{};

        return ResultType{ unexpect_invoke{},
            std::forward<F>(func), std::move(_error) };
    }

    // -- Modifiers

    constexpr void emplace() noexcept
    {
        if (!has_value()) {
            std::destroy_at(std::addressof(_error));
            std::construct_at(std::addressof(_no_value));
            _has_value = true;
        }
    }

    // -- Implementation

private:

    template <class NewType, class OldType, class... Args>
    constexpr void reinit_expected(NewType& new_val, OldType& old_val, Args&&... args)
    {
        if constexpr (std::is_nothrow_constructible_v<NewType, Args...>) {
            std::destroy_at(std::addressof(old_val));
            std::construct_at(std::addressof(new_val), std::forward<Args>(args)...);
        }
        else if constexpr (std::is_nothrow_move_constructible_v<NewType>)
        {
            NewType temp(std::forward<Args>(args)...);
            std::destroy_at(std::addressof(old_val));
            std::construct_at(std::addressof(new_val), std::move(temp));
        }
        else {
            imp::exp::Guard<OldType> guard(std::addressof(old_val));
            std::construct_at(std::addressof(new_val), std::forward<Args>(args)...); // Might throw
            guard.release();
        }
    }

public:

    constexpr ~expected() noexcept(std::is_nothrow_destructible_v<E>)
        requires (!std::is_trivially_destructible_v<E>)
    {
        if (!has_value())
            std::destroy_at(std::addressof(_error));
    }

    constexpr ~expected() noexcept(std::is_nothrow_destructible_v<E>)
        requires (std::is_trivially_destructible_v<E>)
    = default;

    // Copy assignment
    constexpr expected& operator=(const expected& other)
        requires (imp::exp::copy_assignable<VoidType,E>)
    {
        const auto this_valued = has_value();
        const auto that_valued = other.has_value();

        if (!this_valued && !that_valued)
            _error = other.error();
        else if (this_valued && !that_valued) {
            reinit_expected(_error, _no_value, other.error());  // Copy other error
            _has_value = false;
        }
        else if (!this_valued && that_valued) {
            std::destroy_at(std::addressof(_error));
            std::construct_at(std::addressof(_no_value));
            _has_value = true;
        }

        return *this;
    }

    constexpr expected& operator=(const expected&)
        requires (!imp::exp::copy_assignable<VoidType,E>)
    = delete;

    // Move assignment
    constexpr expected& operator=(expected&& other)
        noexcept (imp::exp::move_assign_noexcept<VoidType,E>)
        requires (imp::exp::move_assignable<VoidType,E>)
    {
        const auto this_valued = has_value();
        const auto that_valued = other.has_value();

        if (!this_valued && !that_valued)
            _error = std::move(other._error);
        else if (this_valued && !that_valued) {
            reinit_expected(_error, _no_value, std::move(other._error));
            _has_value = false;
        }
        else if (!this_valued && that_valued) {
            std::destroy_at(std::addressof(_error));
            std::construct_at(std::addressof(_no_value));
            _has_value = true;
        }

        return *this;
    }

    // Assign from an unexpected value
    template <class G>
    constexpr expected& operator=(const unexpected<G>& other)
        requires (imp::exp::assign_from_unexpected<VoidType, E, const G&>)
    {
        if (has_value()) {
            reinit_expected(_error, _no_value, std::forward<const G&>(other.error()));
            _has_value = false;
        }
        else
            _error = std::forward<const G&>(other.error());

        return *this;
    }

    // Assign from an unexpected value
    template <class G>
    constexpr expected& operator=(unexpected<G>&& other)
        requires (imp::exp::assign_from_unexpected<VoidType, E, G>)
    {
        if (has_value()) {
            reinit_expected(_error, _no_value, std::forward<G>(other.error));
            _has_value = false;
        }
        else
            _error = std::forward<G>(other.error());

        return *this;
    }

    /// Swap the contents with those of other
    constexpr void swap(expected& other)
        noexcept(imp::exp::swap_noexcept<VoidType,E>)
        requires(imp::exp::is_swappable<VoidType,E>)
    {
        const auto this_valued = has_value();
        const auto that_valued = other.has_value();

        if (this_valued && that_valued)
            return;     // Presto change-o

        if (!this_valued && !that_valued) {
            using std::swap;
            swap(_error, other._error);
            return;
        }

        if (that_valued) {
            other.swap(*this);  // Do all the stuff below...
            return;
        }

        // this_valued && !that_valued
        std::destroy_at(std::addressof(_no_value));
        std::construct_at(std::addressof(_error), std::move(other._error));
        std::destroy_at(std::addressof(other._error));
    }

    friend constexpr void swap(expected& x, expected& y) noexcept(noexcept(x.swap(y)))
    {
        x.swap(y);
    }

    /// Compares two expected objects (T2 is [cv] void)
    template <class T2, class E2>
        requires(std::is_void_v<T2>)
    friend constexpr bool operator==(const expected& lhs,
        const turner::expected<T2, E2>& rhs)
    {
        const auto lhs_valued = lhs.has_value();
        const auto rhs_valued = rhs.has_value();

        if (lhs_valued && rhs_valued)
            return true;
        if (!lhs_valued && !rhs_valued)
            return lhs.error() == rhs.error();

        return false;
    }

    /// Compares expected object with an unexpected value
    template <class E2>
    friend constexpr bool operator==(const expected& x, const unexpected<E2>& e)
    {
        return !x.has_value() && static_cast<bool>(x.error() == e.error());
    }

private:

    // We need something for constexpr; can't have uninitialized memory.
    // Since the class is trivially constructible and destructible, the
    // std::construct_at and std::destroy_at should evaluate to no ops when
    // optimization is enabled.
    struct no_type { constexpr no_type() noexcept = default; };
    static_assert(std::is_trivially_default_constructible_v<no_type>);
    static_assert(std::is_trivially_destructible_v<no_type>);

    // -- Member data

    union {
        no_type     _no_value;
        error_type  _error;
    };
    bool            _has_value;
};

}   // End namespace turner

// NOLINTEND(performance-noexcept-destructor)
// NOLINTEND(performance-noexcept-move-constructor)
// NOLINTEND(performance-noexcept-swap)

#endif // ifndef turner_expected_header_has_been_included
