#ifndef INT_H
#define INT_H

#include <ratio>

#include "aether/fwd/TypeTraits.h"

namespace aether {

template <std::intmax_t I>
struct _intmax : std::integral_constant<std::intmax_t, I> {};

template <int I>
struct _int : std::integral_constant<int, I> {
  constexpr int Value() const {
    return I;
  }

  template <int J>
  constexpr bool operator==(_int<J>) const {
    return I == J;
  }
};

template <int A, int B>
constexpr bool less_than(_int<A>, _int<B>) {
  return A < B;
}

template <int A, int B>
constexpr auto operator+(_int<A>, _int<B>) {
  return _int<A + B>{};
}

constexpr _int<0> _0{};
constexpr _int<1> _1{};
constexpr _int<2> _2{};
constexpr _int<3> _3{};
constexpr _int<4> _4{};
constexpr _int<5> _5{};
constexpr _int<6> _6{};
constexpr _int<7> _7{};
constexpr _int<-1> _neg1{};
constexpr _int<-2> _neg2{};

template <char... c>
constexpr auto parse_int() {
  constexpr int list[] = {to_int<c>()...};
  constexpr std::array<bool, sizeof...(c)> dot{is_dot<c>()...};

  static_assert(!contains_dot(dot), "int must be an integer");

  int n = 0;
  for (std::size_t i = 0, N = sizeof...(c); i < N; ++i) {
    n = n * 10 + list[i];
  }

  return n;
}

template <char... c>
constexpr auto operator "" _i() {
  constexpr auto result = parse_int<c...>();
  return _int<result>{};
}

} // end namespace aether

#endif
