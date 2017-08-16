#ifndef POW_H
#define POW_H

#include "fwd/Canonical.h"
#include "Expr.h"
#include "Literal.h"
#include "typelist/TypeList.h"

namespace aether {


template <typename T, typename E>
constexpr auto make_power(baseexpr<T>, baseexpr<E>) {
  return powerexpr<T, E>{};
}

template <typename T, typename E>
constexpr auto inverse_vars(powerexpr<T, E>) {
  return MergeAll(inverse_vars(T{}), inverse_vars(E{}));
}

template <typename B, int N, int D, int ID, int SeqIndex, EnableIf<(literal<N, D>{} < zero)> = 0>
constexpr auto degree(powerexpr<B, literal<N, D>>, variable<ID, SeqIndex> v) {
  return degree(B{}, v) * -literal<N, D>{};
}

template <typename B, int N, int D, int ID, int SeqIndex, EnableIf<(literal<N, D>{} > zero)> = 0>
constexpr auto degree(powerexpr<B, literal<N, D>>, variable<ID, SeqIndex> v) {
  return degree(B{}, v) * literal<N, D>{};
}

template <typename T>
constexpr auto make_power(baseexpr<T>, one_t) {
  return T{};
}

template <typename T>
constexpr one_t make_power(one_t, baseexpr<T>) {
  return {};
}

template <typename T>
constexpr auto make_power(baseexpr<T>, zero_t) {
  return one;
}

template <typename T>
constexpr auto make_power(diracexpr<T>, zero_t) {
  return make_one(dimensions_of(T{}));
}

template <typename T>
constexpr auto make_power(constantsubexpr<diracexpr<T>>, zero_t) {
  return make_one(dimensions_of(T{}));
}


template <typename T, typename E1, typename E2>
constexpr auto make_power(powerexpr<T, E1>, baseexpr<E2>) {
  return make_power(T{}, E1{} * E2{});
}

template <typename T, typename E1, typename E2>
constexpr auto make_power(absexpr<powerexpr<T, E1>>, baseexpr<E2>) {
  return make_power(make_expr(abs_tag, T{}), E1{} * E2{});
}

template <typename... Ts>
constexpr productexpr<Ts...> make_power(productexpr<Ts...>, one_t) {
  return {};
}

template <typename... Ts, typename E>
constexpr auto make_power(productexpr<Ts...>, baseexpr<E>) {
  return product_list(List(pow(Ts{}, E{})...));
}

template <typename... Ts, typename E>
constexpr auto make_power(constantsubexpr<branchexpr<Ts...>>, baseexpr<E>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(pow(get_value(Ts{}), E{})...));
}

template <typename... Ts, typename E>
constexpr auto make_power(balanceexpr<branchexpr<Ts...>>, baseexpr<E>) {
  return make_balance_expr(make_branch_expr(List(get_condition(Ts{})...), List(pow(get_value(Ts{}), E{})...)));
}

template <typename... Ts, typename E>
constexpr auto make_power(branchexpr<Ts...>, baseexpr<E>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(pow(get_value(Ts{}), E{})...));
}

template <typename T, typename E>
constexpr auto pow(baseexpr<T>, baseexpr<E>) {
  return make_expr(power_tag, T{}, E{});
}

template <typename T, typename E>
constexpr auto pow(TypeList<T, E>) {
  return pow(T{}, E{});
}

template <typename T>
constexpr auto rcp(baseexpr<T>) {
  return pow(T{}, minus_one);
}

template <typename... Ts>
constexpr auto rcp(branchexpr<Ts...>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(rcp(get_value(Ts{}))...));
}

template <typename... Ts>
constexpr auto rcp(constantsubexpr<branchexpr<Ts...>>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(rcp(get_value(Ts{}))...));
}

template <typename T>
constexpr auto sqrt(baseexpr<T>) {
  return pow(T{}, half);
}

template <typename T>
constexpr auto sq(baseexpr<T>) {
  return pow(T{}, two);
}

template <std::intmax_t N, std::intmax_t D, typename T, EnableIf<(literal<N, D>{} < zero)> = 0>
constexpr auto make_power(productexpr<literal<N, D>, T>, two_t) {
  return sq(literal<-N, D>{} * T{});
}

template <typename T, std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto make_power(powerexpr<T, literal<N1, D1>>, literal<N2, D2>) {
  return pow(T{}, literal<N1, D1>{} * literal<N2, D2>{});
}

template <typename T, std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2>
constexpr auto make_power(constantsubexpr<powerexpr<T, literal<N1, D1>>>, literal<N2, D2>) {
  return pow(T{}, literal<N1, D1>{} * literal<N2, D2>{});
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto rcp(literal<N, D>) {
  return make_literal<D, N>();
}


template <typename B, std::intmax_t N, std::intmax_t D, EnableIf<is_leaf(B{})> = 0>
constexpr int size(powerexpr<B, literal<N, D>>) {
  return 1;
}

template <typename B, typename E>
constexpr int size(powerexpr<B, E>) {
  return size(B{}) + size(E{}) + 1;
}

template <typename B, typename E>
constexpr auto canonicalize_impl(powerexpr<B, E>) {
  return make_power(B{}, E{});
}



} // end namespace aether


#endif
