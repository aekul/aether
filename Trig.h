#ifndef TRIG_H
#define TRIG_H

#include "Expr.h"
#include "Literal.h"
#include "Pow.h"
#include "fwd/typelist/TypeList.h"

namespace aether {

template <typename T>
constexpr auto exp(baseexpr<T>) {
  return expexpr<T>{};
}

template <typename T>
constexpr auto log(baseexpr<T>) {
  return logexpr<T>{};
}


template <typename T>
constexpr auto make_sin(T) {
  return sinexpr<T>{};
}

template <typename T>
constexpr auto make_cos(T) {
  return cosexpr<T>{};
}

template <typename T>
constexpr auto make_tan(T) {
  return tanexpr<T>{};
}

template <typename T, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
constexpr auto var_subexpr(sinexpr<T>, Var) {
  return sin(T{});
}

template <typename T, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
constexpr auto var_subexpr(cosexpr<T>, Var) {
  return cos(T{});
}




template <typename T>
constexpr auto make_arcsin(T) {
  return arcsinexpr<T>{};
}

template <typename T>
constexpr auto make_arccos(T) {
  return arccosexpr<T>{};
}

template <typename T>
constexpr auto make_arctan(T) {
  return arctanexpr<T>{};
}


template <typename T>
constexpr auto operator*(sinexpr<T>, powerexpr<cosexpr<T>, minus_one_t>) {
  return make_tan(T{});
}

template <typename T>
constexpr auto operator/(arcsinexpr<T>, arccosexpr<T>) {
  return make_arctan(T{});
}

template <typename T>
constexpr T sin(arcsinexpr<T>) {
  return {};
}

template <typename T>
constexpr auto sin(arccosexpr<T>) {
  return sqrt(one - sq(T{}));
}

template <typename T>
constexpr auto sin(arctanexpr<T>) {
  return T{} / sqrt(one + sq(T{}));
}

template <typename T>
constexpr auto cos(arcsinexpr<T>) {
  return sqrt(one - sq(T{}));
}

template <typename T>
constexpr T cos(arccosexpr<T>) {
  return {};
}

template <typename T>
constexpr auto cos(arctanexpr<T>) {
  return one / sqrt(one + sq(T{}));
}

template <typename T>
constexpr auto sin(baseexpr<T>) {
  return sinexpr<T>{};
}

template <typename T>
constexpr auto arcsin(baseexpr<T>) {
  return arcsinexpr<T>{};
}

template <typename T>
constexpr auto cos(baseexpr<T>) {
  return cosexpr<T>{};
}

template <typename T>
constexpr auto arccos(baseexpr<T>) {
  return arccosexpr<T>{};
}

template <typename A, typename B>
constexpr auto arccos(productexpr<A, powerexpr<addexpr<powerexpr<A, two_t>, powerexpr<B, two_t>>, minus_half_t>>) {
  return arctan2(B{}, A{});
}

constexpr one_t cos(zero_t) {
  return {};
}

template <typename T>
constexpr auto tan(baseexpr<T>) {
  return tanexpr<T>{};
}

template <typename T>
constexpr auto arctan(baseexpr<T>) {
  return arctanexpr<T>{};
}

template <typename A, typename B>
constexpr auto arctan2(baseexpr<A>, baseexpr<B>) {
  return arctan2expr<A, B>{};
}

template <typename T>
constexpr int size(sinexpr<T>) {
  return size(T{}) + 1;
}

template <typename T>
constexpr int size(cosexpr<T>) {
  return size(T{}) + 1;
}

template <typename T>
constexpr auto canonicalize_impl(sinexpr<T>) {
  return sin(T{});
}

template <typename T>
constexpr auto canonicalize_impl(arcsinexpr<T>) {
  return arcsin(T{});
}

template <typename T>
constexpr auto canonicalize_impl(cosexpr<T>) {
  return cos(T{});
}

template <typename T>
constexpr auto canonicalize_impl(arccosexpr<T>) {
  return arccos(T{});
}

template <typename T>
constexpr auto canonicalize_impl(tanexpr<T>) {
  return tan(T{});
}

template <typename T>
constexpr auto canonicalize_impl(arctanexpr<T>) {
  return arctan(T{});
}

template <typename A, typename B>
constexpr auto canonicalize_impl(arctan2expr<A, B>) {
  return arctan2(A{}, B{});
}

} // end namespace aether

#endif
