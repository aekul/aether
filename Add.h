#ifndef ADD_H
#define ADD_H

#include "fwd/Canonical.h"
#include "Expr.h"
#include "Literal.h"
#include "Param.h"
#include "Pow.h"
#include "Product.h"
#include "Trig.h"
#include "typelist/Intersection.h"
#include "typelist/TypeList.h"
#include "typelist/Max.h"

namespace aether {




constexpr auto flatten_add(TypeList<>) {
  return TypeList<>{};
}

template <typename... As, typename... Ts>
constexpr auto flatten_add(TypeList<addexpr<As...>, Ts...>) {
  return flatten_add(TypeList<As..., Ts...>{});
}

template <typename A, typename... Ts>
constexpr auto flatten_add(TypeList<A, Ts...>) {
  return TypeList<A>{}.Concat(flatten_add(TypeList<Ts...>{}));
}

template <typename T, typename... Vs>
constexpr auto add(baseexpr<T>, addexpr<Vs...>) {
  return addexpr<T, Vs...>{};
}


template <typename... Rs, typename... Ts, EnableIf<condition_list(branchexpr<Rs...>{}) == condition_list(branchexpr<Ts...>{})> = 0>
constexpr auto add(branchexpr<Rs...>, branchexpr<Ts...>) {
  return make_branch_expr(List(make_expr(branch_case_tag, get_condition(Rs{}), get_value(Rs{}) + get_value(Ts{}))...));
}

template <typename A, typename B, EnableIf<is_zero(A{}) && is_zero(B{})> = 0>
constexpr A add(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename A, typename B, EnableIf<is_zero(B{})> = 0>
constexpr A add(baseexpr<A>, baseexpr<B>) {
  return {};
}
  
template <typename A, typename B, EnableIf<is_zero(A{})> = 0>
constexpr B add(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename T, typename... Vs>
constexpr auto add(addexpr<Vs...>, baseexpr<T>) {
  return addexpr<Vs..., T>{};
}
  
template <typename... Ts, typename... Vs>
constexpr auto add(addexpr<Ts...>, addexpr<Vs...>) {
  return addexpr<Ts..., Vs...>{};
}

template <typename T, typename... Vs>
constexpr auto add(baseexpr<T> a, balanceexpr<branchexpr<Vs...>>) {
  return make_balance_expr(make_branch_expr(List(get_condition(Vs{})...), List(a + get_value(Vs{})...)));
}

template <typename T, typename... Vs>
constexpr auto add(balanceexpr<branchexpr<Vs...>>, expr<T> b) {
  return make_balance_expr(make_branch_expr(List(get_condition(Vs{})...), List(get_value(Vs{}) + b...)));
}

struct is_simple_fraction_t {
  template <typename T>
  constexpr bool operator()(powerexpr<T, minus_one_t>) const {
    return true;
  }

  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }
};

constexpr is_simple_fraction_t is_simple_fraction{};

template <std::intmax_t N, std::intmax_t D, typename... Ts, typename... Vs, EnableIf<TypeList<Ts...>{} == TypeList<Vs...>{}> = 0>
constexpr auto add(productexpr<literal<N, D>, Ts...>, productexpr<Vs...>) {
  return (literal<N, D>{} + one) * productexpr<Ts...>{};
}

template <std::intmax_t N, std::intmax_t D, typename... Ts, typename... Vs, EnableIf<TypeList<Ts...>{} == TypeList<Vs...>{}> = 0>
constexpr auto add(productexpr<Ts...>, productexpr<literal<N, D>, Vs...>) {
  return (literal<N, D>{} + one) * productexpr<Ts...>{};
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2, typename... Ts, typename... Vs, EnableIf<TypeList<Ts...>{} == TypeList<Vs...>{}> = 0>
constexpr auto add(productexpr<literal<N1, D1>, Ts...>, productexpr<literal<N2, D2>, Vs...>) {
  return (literal<N1, D1>{} + literal<N2, D2>{}) * productexpr<Ts...>{};
}


template <typename... Ts>
constexpr auto inverse_vars(addexpr<Ts...>) {
  return MergeAll(inverse_vars(Ts{})...);
}

template <typename... Ts, typename Var>
constexpr auto var_subexpr(addexpr<Ts...>, Var) {
  constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
  return add_list(at<0>(parts));
}

template <typename... Ts, typename Var>
constexpr auto partition_dependent_subexprs(addexpr<Ts...>, Var) {
  constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
  return List(add_list(at<0>(parts)), add_list(at<1>(parts)));
}

template <typename A>
constexpr auto add(baseexpr<A>, baseexpr<A>) {
  return two * A{};
}

template <typename A, typename B>
constexpr bool sin2cos2(TypeList<baseexpr<A>>, TypeList<baseexpr<B>>) {
  return false;
}

template <typename T>
constexpr bool sin2cos2(TypeList<powerexpr<sinexpr<T>, two_t>>, TypeList<powerexpr<cosexpr<T>, two_t>>) {
  return true;
}

template <typename T>
constexpr bool sin2cos2(TypeList<powerexpr<cosexpr<T>, two_t>>, TypeList<powerexpr<sinexpr<T>, two_t>>) {
  return true;
}

template <typename... As, typename... Bs, typename I = decltype(intersection(List(As{}...), List(Bs{}...))), EnableIf<at<0>(I{}).Size() == 1 && at<1>(I{}).Size() == 1 && sin2cos2(at<0>(I{}), at<1>(I{}))> = 0>
constexpr auto add(productexpr<As...>, productexpr<Bs...>) {
  return product_list(at<2>(I{}));
}

template <typename A>
constexpr one_t add(powerexpr<sinexpr<A>, two_t>, powerexpr<cosexpr<A>, two_t>) {
  return {};
}

template <typename A>
constexpr one_t add(powerexpr<cosexpr<A>, two_t>, powerexpr<sinexpr<A>, two_t>) {
  return {};
}


template <typename A>
constexpr zero_t add(baseexpr<A>, productexpr<minus_one_t, A>) {
  return {};
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2, typename T>
constexpr auto add(productexpr<literal<N1, D1>, T>, productexpr<literal<N2, D2>, T>) {
  return (literal<N1, D1>{} + literal<N2, D2>{}) * T{};
}

template <std::intmax_t N, std::intmax_t D, typename T>
constexpr auto add(productexpr<literal<N, D>, T>, baseexpr<T>) {
  return (literal<N, D>{} + one) * T{};
}

template <int ID, int SeqIndex>
constexpr auto add(variable<ID, SeqIndex> v, variable<ID, SeqIndex>) {
  return two * v;
}


template <typename A, typename B, typename E>
constexpr bool addd(baseexpr<A>, powerexpr<B, E>) {
  return true;
}

template <typename A, typename B, typename E = void_t<>>
struct can_simplify_add_t : std::false_type {
};

template <typename A, typename B>
struct can_simplify_add_t<A, B, void_t<decltype(add(A{}, B{}))>> : std::true_type {
};

template <typename A, typename B>
constexpr bool can_simplify_add(baseexpr<A>, baseexpr<B>) {
  return can_simplify_add_t<A, B>::value;
}

template <typename T>
constexpr auto add(baseexpr<one_t>, productexpr<T, powerexpr<addexpr<one_t, productexpr<minus_one_t, T>>, minus_one_t>>) {
  return one * rcp(one - T{});
}

template <typename... Ts>
constexpr addexpr<Ts...> make_add(TypeList<addexpr<Ts...>>) {
  return {};
}

template <typename... Ts>
constexpr addexpr<Ts...> make_add(TypeList<Ts...>) {
  return {};
}

template <typename T>
constexpr T make_add(TypeList<T>) {
  return {};
}

template <typename A, typename B>
constexpr addexpr<A, B> make_add(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename A>
constexpr TypeList<A> simplify_adjacent_add_impl(TypeList<A>) {
  return {};
}

template <typename A, typename B, typename... Ts, EnableIf<can_simplify_add(A{}, B{})> = 0>
constexpr auto simplify_adjacent_add_impl(TypeList<A, B, Ts...>) {
  return simplify_adjacent_add_impl(List(add(A{}, B{})).Concat(TypeList<Ts...>{}));
}

template <typename A, typename B, typename... Ts, EnableIf<!can_simplify_add(A{}, B{})> = 0>
constexpr auto simplify_adjacent_add_impl(TypeList<A, B, Ts...>) {
  return List(A{}).Concat(simplify_adjacent_add_impl(TypeList<B, Ts...>{}));
}

template <typename Current>
constexpr Current simplify_adjacent_add_recursive(Current, Current) {
  return {};
}

template <typename Current, typename Next>
constexpr auto simplify_adjacent_add_recursive(Current, Next) {
  return simplify_adjacent_add(Next{}); 
}

template <typename... Ts>
constexpr auto simplify_adjacent_add(TypeList<Ts...>) {
  return simplify_adjacent_add_recursive(TypeList<Ts...>{}, simplify_adjacent_add_impl(TypeList<Ts...>{}));
}


template <typename T, typename... Ts>
constexpr int size(addexpr<T, Ts...>) {
  return size(T{}) + size(TypeList<Ts...>{}) + 1;
}


template <typename... Ts, int ID, int SeqIndex>
constexpr auto degree(addexpr<Ts...>, variable<ID, SeqIndex> v) {
  return max(List(degree(Ts{}, v)...));
}

template <typename... Ts>
constexpr auto canonicalize_impl(addexpr<Ts...>) {
  return make_add(simplify_adjacent_add(sort(Ts{}...)));
}

template <typename... Ts>
constexpr TypeList<Ts...> operands(addexpr<Ts...>) {
  return {};
}

template <typename... Ts>
constexpr auto add_list(TypeList<Ts...>) {
  static_assert(sizeof...(Ts) > 0, "add_list() requires a list with at least one element");
  return (Ts{} + ...);
}


template <typename A, typename B>
constexpr auto operator+(baseexpr<A>, baseexpr<B>) {
  return make_expr(add_tag, A{}, B{});
}

template <typename A, typename B>
constexpr auto operator-(baseexpr<A>, baseexpr<B>) {
  return A{} + minus_one * B{};
}

template <typename T>
constexpr auto operator-(baseexpr<T>) {
  return minus_one * T{};
}

constexpr auto operator-(zero_t) {
  return zero;
}

template <typename A, typename B>
constexpr auto minus_impl(bool_t<false>, baseexpr<A>, baseexpr<B>) {
  return A{} - B{};
}

template <typename A, typename B>
constexpr auto minus_impl(bool_t<true>, baseexpr<A>, baseexpr<B>) {
  return A{} - B{};
}


} // end namespace aether

#endif
