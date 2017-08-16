#ifndef LITERAL_H
#define LITERAL_H

#include <array>
#include <ratio>

#include "aether/Expr.h"
#include "aether/Int.h"
#include "aether/typelist/TypeSet.h"

namespace aether {


template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator<(literal<N1, D1>, literal<N2, D2>) {
  return std::ratio_less<std::ratio<N1, D1>, std::ratio<N2, D2>>::value;
} 

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator<=(literal<N1, D1>, literal<N2, D2>) {
  return bool_t<std::ratio_less_equal<std::ratio<N1, D1>, std::ratio<N2, D2>>::value>{};
} 

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator>(literal<N1, D1>, literal<N2, D2>) {
  return bool_t<std::ratio_greater<std::ratio<N1, D1>, std::ratio<N2, D2>>::value>{};
} 

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator>=(literal<N1, D1>, literal<N2, D2>) {
  return bool_t<std::ratio_greater_equal<std::ratio<N1, D1>, std::ratio<N2, D2>>::value>{};
} 

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator==(literal<N1, D1>, literal<N2, D2>) {
  return bool_t<std::ratio_equal<std::ratio<N1, D1>, std::ratio<N2, D2>>::value>{};
} 

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator!=(literal<N1, D1>, literal<N2, D2>) {
  return bool_t<std::ratio_not_equal<std::ratio<N1, D1>, std::ratio<N2, D2>>::value>{};
} 

template <std::intmax_t N, std::intmax_t D>
constexpr auto vars(literal<N, D>) {
  return make_type_set();
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto inverse_vars(literal<N, D>) {
  return make_type_set();
}

template <std::intmax_t A, std::intmax_t B>
constexpr bool multiply_overflows_impl() {
  if (B > 0) {
    return A > (std::numeric_limits<std::intmax_t>::max() / B);
  }

  return A < (std::numeric_limits<std::intmax_t>::max() / B);
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t D2>
constexpr bool multiply_overflows(literal<N1, D1>, literal<0, D2>) {
  return multiply_overflows_impl<D1, D2>();
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr bool multiply_overflows(literal<N1, D1>, literal<N2, D2>) {
  return multiply_overflows_impl<N1, N2>() || multiply_overflows_impl<D1, D2>();
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2, EnableIf<!multiply_overflows(literal<N1, D1>{}, literal<N2, D2>{})> = 0>
constexpr auto operator*(literal<N1, D1>, literal<N2, D2>) {
  using T = std::ratio_multiply<std::ratio<N1, D1>, std::ratio<N2, D2>>;
  return literal<T::num, T::den>{};
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2, EnableIf<multiply_overflows(literal<N1, D1>{}, literal<N2, D2>{})> = 0>
constexpr productexpr<literal<N1, D1>, literal<N2, D2>> operator*(literal<N1, D1>, literal<N2, D2>) {
  return {};
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator/(literal<N1, D1>, literal<N2, D2>) {
  using T = std::ratio_divide<std::ratio<N1, D1>, std::ratio<N2, D2>>;
  return literal<T::num, T::den>{};
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator+(literal<N1, D1>, literal<N2, D2>) {
  using T = std::ratio_add<std::ratio<N1, D1>, std::ratio<N2, D2>>;
  return literal<T::num, T::den>{};
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto add(literal<N1, D1>, literal<N2, D2>) {
  return literal<N1, D1>{} + literal<N2, D2>{};
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto operator-(literal<N1, D1>, literal<N2, D2>) {
  using T = std::ratio_subtract<std::ratio<N1, D1>, std::ratio<N2, D2>>;
  return literal<T::num, T::den>{};
}


template <std::intmax_t X>
constexpr std::intmax_t sqrt() {
  std::intmax_t low = 0;
  std::intmax_t high = X + 1;

  while (high - low > 1) {
    std::intmax_t mid = low + ((high - low) / 2);

    if (mid > (std::numeric_limits<std::intmax_t>::max() - mid) / mid) {
      high = mid;
      continue;
    }

    if (mid * mid == X) {
      return mid;
    }

    if (mid * mid > X) {
      high = mid;
    } else {
      low = mid;
    }
  }

  return -1;
}

template <std::intmax_t X>
constexpr bool is_perfect_square() {
  return sqrt<X>() != -1;
}


using zero_t = literal<0, 1>;
using one_t = literal<1, 1>;
using two_t = literal<2, 1>;
using half_t = literal<1, 2>;
using minus_half_t = literal<-1, 2>;
using minus_one_t = literal<-1, 1>;
using minus_two_t = literal<-2, 1>;

constexpr zero_t zero{};
constexpr one_t one{};
constexpr two_t two{};
constexpr half_t half{};
constexpr minus_half_t minus_half{};
constexpr minus_one_t minus_one{};
constexpr minus_two_t minus_two{};


template <std::intmax_t N, std::intmax_t D>
constexpr bool is_fraction(literal<N, D> x) {
  return (x < one && x > zero) || (x > minus_one && x < zero);
}

constexpr zero_t make_zero(dimensions<1, 1>) {
  return {};
}

constexpr one_t make_one(dimensions<1, 1>) {
  return {};
}


template <std::intmax_t N, std::intmax_t D>
constexpr auto reduce(literal<N, D>) {
  return add(literal<N, D>{}, zero);
}

template <std::intmax_t N, std::intmax_t D, EnableIf<(D > 0)> = 0>
constexpr auto normalize_sign(literal<N, D>) {
  return literal<N, D>{};
}

template <std::intmax_t N, std::intmax_t D, EnableIf<(D < 0)> = 0>
constexpr auto normalize_sign(literal<N, D>) {
  return literal<-N, -D>{};
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto simplify_impl(literal<N, D>) {
  return literal<N, D>{};
}

template <std::intmax_t N, std::intmax_t D>
constexpr bool is_negative(literal<N, D>) {
  return std::ratio_less<std::ratio<N, D>, std::ratio<0, 1>>::value;
}

template <std::intmax_t N, std::intmax_t D, typename B>
constexpr bool less_than(literal<N, D>, baseexpr<B>) {
  return true;
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr bool less_than(literal<N1, D1> a, literal<N2, D2> b) {
  return a < b;
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto canonicalize_impl(literal<N, D>) {
  return normalize_sign(reduce(literal<N, D>{}));
}

template <std::intmax_t N, std::intmax_t D>
constexpr one_t pow(literal<N, D>, zero_t) {
  return {};
}

template <std::intmax_t N, std::intmax_t D>
constexpr literal<N, D> pow(literal<N, D>, one_t) {
  return {};
}

constexpr two_t pow(literal<4, 1>, literal<1, 2>) {
  return {};
}

template <std::intmax_t N, std::intmax_t D, std::intmax_t E, EnableIf<(E > 1)> = 0>
constexpr auto pow(literal<N, D>, literal<E, 1>) {
  return literal<N, D>{} * pow(literal<N, D>{}, literal<E - 1, 1>{});
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto pow(literal<N, D>, minus_one_t) {
  return rcp(literal<N, D>{});
}

template <std::intmax_t N, std::intmax_t D, std::intmax_t E, EnableIf<(E < -1)> = 0>
constexpr auto pow(literal<N, D>, literal<E, 1>) {
  return rcp(pow(literal<N, D>{}, literal<-E, 1>{}));
}

template <std::size_t... Is>
constexpr auto make_literal_sequence_impl(std::index_sequence<Is...>) {
  return List(literal<Is, 1>{}...);
}

template <std::size_t N>
constexpr auto make_literal_sequence() {
  return make_literal_sequence_impl(std::make_index_sequence<N>{});
}

template <std::intmax_t N, std::intmax_t D = 1>
constexpr auto make_literal() {
  return make_expr(literal_tag, _intmax<N>{}, _intmax<D>{});
}

template <std::intmax_t N, std::intmax_t D, EnableIf<is_perfect_square<N>() && !is_perfect_square<D>()> = 0>
constexpr auto sqrt(literal<N, D>) {
  return make_literal<sqrt<N>(), D>();
}

template <std::intmax_t N, std::intmax_t D, EnableIf<!is_perfect_square<N>() && is_perfect_square<D>()> = 0>
constexpr auto sqrt(literal<N, D>) {
  return make_literal<N, sqrt<D>()>();
}

template <std::intmax_t N, std::intmax_t D, EnableIf<is_perfect_square<N>() && is_perfect_square<D>()> = 0>
constexpr auto sqrt(literal<N, D>) {
  return make_literal<sqrt<N>(), sqrt<D>()>();
}

template <std::intmax_t N, std::intmax_t D>
constexpr bool is_guaranteed_non_negative(literal<N, D> l) {
  return l >= zero;
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto operator-(literal<N, D>) {
  return literal<-N, D>{};
}

constexpr int pow10(int N) {
  if (N <= 0) {
    return 1;
  }
  return 10 * pow10(N - 1);
}

struct float_literal {
  int num;
  int exponent;
};

template <char... c>
constexpr auto parse_float() {
  constexpr int list[] = {to_int<c>()...};
  constexpr bool dot[] = {is_dot<c>()...};

  int n = 0;
  int e = 0;

  for (std::size_t i = 0, N = sizeof...(c); i < N; ++i) {
    if (dot[i]) {
      e = N - i - 1;
      continue;
    }
    n = n * 10 + list[i];
  }

  return float_literal{n, e};
}

template <char... c>
constexpr auto operator "" _l() {
  constexpr auto result = parse_float<c...>();
  return literal<result.num>{} * literal<1, pow10(result.exponent)>{};
}

constexpr auto get_pi() {
    return literal<245850922, 78256779>{};
}
  
} // end namespace aether


#endif
