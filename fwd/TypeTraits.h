#ifndef FWD_TYPE_TRAITS_H
#define FWD_TYPE_TRAITS_H

#include <array>
#include <type_traits>
#include "aether/fwd/typelist/TypeList.h"

template <typename T>
struct fn_traits : fn_traits<decltype(&T::operator())> {
};

template <typename Ret, typename C, typename... Args>
struct fn_traits<Ret(C::*)(Args...)> {
  using type = Ret;
  using args = aether::TypeList<Args...>;
};

template <typename Ret, typename C, typename... Args>
struct fn_traits<Ret(C::*)(Args...) const> {
  using type = Ret;
  using args = aether::TypeList<Args...>;
};

template <typename T>
using fn_return_t = typename fn_traits<T>::type;

template <typename T>
using fn_args_t = typename fn_traits<T>::args;


template <typename T, typename E = std::void_t<>>
struct is_callable : std::false_type {};

template <typename T>
struct is_callable<T, std::void_t<decltype(&T::operator())>> : std::true_type {};

template <typename T>
constexpr bool is_callable_v = is_callable<T>::value;


namespace aether {

template <typename T, typename E = std::void_t<>>
struct is_dereferenceable : std::false_type {};

template <typename T>
struct is_dereferenceable<T, std::void_t<decltype(*std::declval<T>())>> : std::true_type {};

template <typename T>
constexpr bool is_dereferenceable_v = is_dereferenceable<T>::value;

template <typename... Ts>
struct Ty;

template <bool Cond>
using EnableIf = typename std::enable_if<Cond, int>::type;

template <typename T, typename V>
using EnableIfBaseOf = EnableIf<std::is_base_of<T, V>::value>;

template <typename T, typename V>
using DisableIfBaseOf = EnableIf<!std::is_base_of<std::decay_t<T>, std::decay_t<V>>::value>;

template <bool B>
using bool_t = std::integral_constant<bool, B>;

using true_t = std::true_type;
using false_t = std::false_type;

template <typename Tuple>
struct size_of_tuple_t;

template <std::size_t I>
struct _size_t : std::integral_constant<std::size_t, I> {
  constexpr std::size_t operator()() const {
    return I;
  }

  constexpr operator std::size_t() const {
    return I;
  }

  constexpr _size_t<I + 1> Next() const {
    return {};
  }

  constexpr _size_t<I - 1> Prev() const {
    return {};
  }

  constexpr _size_t<I + 1> operator++() const {
    return {};
  }

  template <std::size_t J>
  constexpr _size_t<I + J> operator+(_size_t<J>) const {
    return {};
  }

  template <std::size_t J>
  constexpr _size_t<I - J> operator-(_size_t<J>) const {
    return {};
  }

  constexpr _size_t<I + 1> operator++(int) const {
    return {};
  }
};

template <std::size_t A, std::size_t B>
constexpr bool less_than(_size_t<A>, _size_t<B>) {
  return A < B;
};

template <std::size_t I>
using _index = _size_t<I>;

template <typename...>
using void_t = void;

template <typename T, typename R>
using is_same_t = std::is_same<T, R>;

template <typename T, typename R>
constexpr bool equal(T, R) {
  return false;
} 

template <typename T>
constexpr bool equal(T, T) {
  return true;
} 

template <char c>
constexpr int to_int() {
  return static_cast<int>(c) - 48;
}

template <char c>
constexpr bool is_dot() {
  return c == '.';
}

template <std::size_t N>
constexpr bool contains_dot(const std::array<bool, N>& dot) {
  for (std::size_t i = 0; i < N; ++i) {
    if (dot[i]) {
      return true;
    }
  }
  return false;
}

} // end namespace aether

#endif
