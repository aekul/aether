#ifndef PRODUCT_H
#define PRODUCT_H

#include "aether/Add.h"
#include "aether/typelist/Append.h"
#include "aether/Expr.h"
#include "aether/Literal.h"
#include "aether/Pow.h"
#include "aether/Condition.h"
#include "aether/typelist/TypeList.h"
#include "aether/typelist/Sort.h"
#include "aether/Vector.h"

namespace aether {




template <typename A, typename B, EnableIf<is_zero(A{}) || is_zero(B{})> = 0>
constexpr auto product(baseexpr<A>, baseexpr<B>) {
  return make_zero(dimensions_of(productexpr<A, B>{}));
}

template <typename A>
constexpr A product(one_t, baseexpr<A>) {
  return {};
}

template <typename A>
constexpr A product(baseexpr<A>, one_t) {
  return {};
}

template <std::intmax_t N1, std::intmax_t D1, std::intmax_t N2, std::intmax_t D2, EnableIf<!multiply_overflows(literal<N1, D1>{}, literal<N2, D2>{})> = 0>
constexpr auto product(literal<N1, D1>, literal<N2, D2>) {
  return literal<N1, D1>{} * literal<N2, D2>{};
}

template <typename... Ts>
constexpr auto product(one_t, productexpr<Ts...>) {
  return productexpr<Ts...>{};
}

template <int ID, int SeqIndex>
constexpr auto product(variable<ID, SeqIndex> v, variable<ID, SeqIndex>) {
  return sq(v);
}


template <typename... Ts, int ID, int SeqIndex>
constexpr auto degree(productexpr<Ts...>, variable<ID, SeqIndex> v) {
  return canonicalize(add_list(List(degree(Ts{}, v)...)));
}




template <typename A, EnableIf<is_scalarexpr(A{})> = 0>
constexpr auto product(baseexpr<A>, baseexpr<A>) {
  return sq(A{});
}


template <typename T, typename A, typename B>
constexpr auto product(powerexpr<T, A>, powerexpr<T, B>) {
  return pow(T{}, A{} + B{});
}

template <typename T, typename E>
constexpr auto product(powerexpr<T, E>, baseexpr<T>) {
  return pow(T{}, E{} + one);
}

template <typename T, typename E>
constexpr auto product(baseexpr<T>, powerexpr<T, E>) {
  return pow(T{}, E{} + one);
}

template <typename T, typename E>
constexpr auto product(powerexpr<constantsubexpr<T>, E>, baseexpr<T>) {
  return pow(T{}, E{} + one);
}

template <typename T>
constexpr T make_product(TypeList<T>) {
  return {};
}

template <typename... Ts>
constexpr productexpr<Ts...> make_product(TypeList<productexpr<Ts...>>) {
  return {};
}

template <typename... Ts>
constexpr productexpr<Ts...> make_product(TypeList<Ts...>) {
  return {};
}

template <typename A, typename B>
constexpr productexpr<A, B> make_product(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename A, typename B, EnableIf<is_constant(B{}) && !depends_on_inverse_variables(B{})> = 0>
constexpr auto product(constantsubexpr<A>, baseexpr<B>) {
  return A{} * B{};
}

template <typename A, typename B, EnableIf<is_constant(A{}) && !depends_on_inverse_variables(A{})> = 0>
constexpr auto product(baseexpr<A>, constantsubexpr<B>) {
  return A{} * B{};
}

template <typename A, typename B>
constexpr auto product(constantsubexpr<A>, constantsubexpr<B>) {
  return A{} * B{};
}


template <typename T>
constexpr auto product(sinexpr<T>, powerexpr<cosexpr<T>, minus_one_t>) {
  return tan(T{});
}

template <typename T>
constexpr auto product(sinexpr<T>, cosexpr<T>) {
  return half * sin(two * T{});
}

template <typename A, typename B>
constexpr auto product(baseexpr<A>, powerexpr<addexpr<one_t, productexpr<powerexpr<B, two_t>, powerexpr<A, minus_two_t>>>, half_t>) {
  return powerexpr<addexpr<powerexpr<B, two_t>, powerexpr<A, two_t>>, half_t>{};
}

template <typename A, typename B>
constexpr auto product(baseexpr<A>, powerexpr<addexpr<one_t, productexpr<powerexpr<A, minus_two_t>, powerexpr<B, two_t>>>, half_t>) {
  return powerexpr<addexpr<powerexpr<A, two_t>, powerexpr<B, two_t>>, half_t>{};
}

template <typename A, typename C1, typename B>
constexpr auto product(baseexpr<A>, powerexpr<addexpr<one_t, productexpr<constantsubexpr<C1>, powerexpr<A, minus_two_t>, powerexpr<B, two_t>>>, half_t>) {
  return powerexpr<addexpr<powerexpr<A, two_t>, productexpr<constantsubexpr<C1>, powerexpr<B, two_t>>>, half_t>{};
}

template <typename A, typename C1, typename C2, typename B>
constexpr auto product(baseexpr<A>, powerexpr<addexpr<one_t, productexpr<constantsubexpr<C1>, constantsubexpr<C2>, powerexpr<A, minus_two_t>, powerexpr<B, two_t>>>, half_t>) {
  return powerexpr<addexpr<powerexpr<A, two_t>, productexpr<constantsubexpr<C1>, constantsubexpr<C2>, powerexpr<B, two_t>>>, half_t>{};
}





constexpr auto flatten_product(TypeList<>) {
  return TypeList<>{};
}

template <typename... As, typename... Ts>
constexpr auto flatten_product(TypeList<productexpr<As...>, Ts...>) {
  return flatten_product(TypeList<As..., Ts...>{});
}

template <typename A, typename... Ts>
constexpr auto flatten_product(TypeList<A, Ts...>) {
  return TypeList<A>{}.Concat(flatten_product(TypeList<Ts...>{}));
}

template <typename... Ts>
constexpr auto flatten(productexpr<Ts...>) {
  return product_list(flatten_product(TypeList<Ts...>{}));
}


template <typename... Ts>
constexpr TypeList<Ts...> operands(productexpr<Ts...>) {
  return {};
}

template <typename T>
constexpr TypeList<T> operands(baseexpr<T>) {
  return {}; 
}

template <std::intmax_t N, std::intmax_t D, typename... Ts>
constexpr auto product(literal<N, D>, addexpr<Ts...>) {
  return add_list(List(literal<N, D>{} * Ts{} ...));
}

template <typename... Ts, std::intmax_t N, std::intmax_t D>
constexpr auto product(addexpr<Ts...>, literal<N, D>) {
  return add_list(List(literal<N, D>{} * Ts{} ...));
}

template <int ID, typename T, typename... Ts, EnableIf<!is_constant(addexpr<Ts...>{})> = 0>
constexpr auto product(subexpr<ID, T>, addexpr<Ts...>) {
  return add_list(List(subexpr<ID, T>{} * Ts{} ...));
}


template <typename... Ts, typename... Vs, EnableIf<same_conditions(condition_list(branchexpr<Ts...>{}), condition_list(branchexpr<Vs...>{}))> = 0>
constexpr auto product(balanceexpr<branchexpr<Ts...>>, balanceexpr<branchexpr<Vs...>>) {
  return make_balance_expr(make_branch_expr(List(get_condition(Vs{})...), List(get_value(Ts{}) * get_value(Vs{})...)));
}

template <typename T, typename... Vs>
constexpr auto product(baseexpr<T> a, balanceexpr<branchexpr<Vs...>>) {
  return make_balance_expr(make_branch_expr(List(get_condition(Vs{})...), List(a * get_value(Vs{})...)));
}

template <typename T, typename... Vs>
constexpr auto product(balanceexpr<branchexpr<Vs...>>, expr<T> b) {
  return make_balance_expr(make_branch_expr(List(get_condition(Vs{})...), List(get_value(Vs{}) * b...)));
}

template <typename... Ts, typename... Vs>
constexpr auto product(constantsubexpr<productexpr<Ts...>>, constantsubexpr<productexpr<Vs...>>) {
  return constantsubexpr<productexpr<Ts..., Vs...>>{};
}


template <typename... Ts, typename... Vs>
constexpr auto product(productexpr<Ts...>, productexpr<Vs...>) {
  return productexpr<Ts..., Vs...>{};
}


template <typename T, typename... Vs>
constexpr auto product(baseexpr<T>, productexpr<Vs...>) {
  return productexpr<T, Vs...>{};
}

template <typename... Ts, typename V>
constexpr auto product(productexpr<Ts...>, baseexpr<V>) {
  return productexpr<Ts..., V>{};
}

template <typename T, typename V, EnableIf<is_matrix(T{}) && is_scalarexpr(V{})> = 0>
constexpr auto product(baseexpr<T>, baseexpr<V>) {
  return V{} * T{};
}


template <typename A, typename B, typename T>
constexpr auto product(addexpr<A, productexpr<decltype(-A{}), T>>, powerexpr<addexpr<B, productexpr<decltype(-B{}), T>>, minus_one_t>) {
  return A{} / B{};
}


template <typename A, typename B, typename E = void_t<>>
struct can_simplify_product_t : std::false_type {
};

template <typename A, typename B>
struct can_simplify_product_t<A, B, void_t<decltype(product(A{}, B{}))>> : std::true_type {
};

template <typename A, typename B>
constexpr bool can_simplify_product(baseexpr<A>, baseexpr<B>) {
  return can_simplify_product_t<A, B>::value;
}

template <typename A, typename B>
constexpr bool can_simplify_product(A, B) {
  return can_simplify_product_t<A, B>::value;
}

template <typename A>
constexpr TypeList<A> simplify_adjacent_product_impl(TypeList<A>) {
  return {};
}

template <typename A, typename B, typename... Ts, EnableIf<can_simplify_product(A{}, B{})> = 0>
constexpr auto simplify_adjacent_product_impl(TypeList<A, B, Ts...>) {
  return simplify_adjacent_product_impl(List(product(A{}, B{})).Concat(TypeList<Ts...>{}));
}

template <typename A, typename B, typename... Ts, EnableIf<!can_simplify_product(A{}, B{})> = 0>
constexpr auto simplify_adjacent_product_impl(TypeList<A, B, Ts...>) {
  return List(A{}).Concat(simplify_adjacent_product_impl(TypeList<B, Ts...>{}));
}

template <typename Current>
constexpr Current simplify_adjacent_product_recursive(Current, Current) {
  return {};
}

template <typename Current, typename Next>
constexpr auto simplify_adjacent_product_recursive(Current, Next) {
  return simplify_adjacent_product(Next{}); 
}

template <typename... Cs>
constexpr bool has_branch_with_same_conditions(TypeList<Cs...>, TypeList<>) {
  return false;
}

template <typename... Cs, typename T, typename... Ps>
constexpr bool has_branch_with_same_conditions(TypeList<Cs...> conds, TypeList<T, Ps...>) {
  return has_branch_with_same_conditions(conds, TypeList<Ps...>{});
}

template <typename... Cs, typename... Ts, typename... Ps, EnableIf<!same_conditions(TypeList<Cs...>{}, condition_list(branchexpr<Ts...>{}))> = 0>
constexpr bool has_branch_with_same_conditions(TypeList<Cs...> conds, TypeList<branchexpr<Ts...>, Ps...>) {
  return has_branch_with_same_conditions(conds, TypeList<Ps...>{});
}

template <typename... Cs, typename... Ts, typename... Ps, EnableIf<same_conditions(TypeList<Cs...>{}, condition_list(branchexpr<Ts...>{}))> = 0>
constexpr bool has_branch_with_same_conditions(TypeList<Cs...>, TypeList<branchexpr<Ts...>, Ps...>) {
  return true;
}

template <typename... Cs, typename... Ts, typename... Ps, EnableIf<same_conditions(TypeList<Cs...>{}, condition_list(branchexpr<Ts...>{}))> = 0>
constexpr bool has_branch_with_same_conditions(TypeList<Cs...>, TypeList<constantsubexpr<branchexpr<Ts...>>, Ps...>) {
  return true;
}

template <typename... Cs, typename T, typename... Ps>
constexpr auto find_branch_with_same_conditions(TypeList<Cs...> conds, TypeList<T, Ps...>) {
  constexpr auto result = find_branch_with_same_conditions(conds, TypeList<Ps...>{});
  return List(at<0>(result), List(T{}).Concat(at<1>(result)));
}

template <typename... Cs, typename... Ts, typename... Ps, EnableIf<!same_conditions(TypeList<Cs...>{}, condition_list(branchexpr<Ts...>{}))> = 0>
constexpr auto find_branch_with_same_conditions(TypeList<Cs...> conds, TypeList<branchexpr<Ts...>, Ps...>) {
  constexpr auto result = find_branch_with_same_conditions(conds, TypeList<Ps...>{});
  return List(at<0>(result), List(branchexpr<Ts...>{}).Concat(at<1>(result)));
}

template <typename... Cs, typename... Ts, typename... Ps, EnableIf<same_conditions(TypeList<Cs...>{}, condition_list(branchexpr<Ts...>{}))> = 0>
constexpr auto find_branch_with_same_conditions(TypeList<Cs...>, TypeList<branchexpr<Ts...>, Ps...>) {
  return List(branchexpr<Ts...>{}, TypeList<Ps...>{});
}

template <typename... Cs, typename... Ts, typename... Ps, EnableIf<same_conditions(TypeList<Cs...>{}, condition_list(branchexpr<Ts...>{}))> = 0>
constexpr auto find_branch_with_same_conditions(TypeList<Cs...>, TypeList<constantsubexpr<branchexpr<Ts...>>, Ps...>) {
  return List(branchexpr<Ts...>{}, TypeList<Ps...>{});
}

constexpr auto combine_branches_with_same_conditions(TypeList<>) {
  return List();
}

template <typename... Ts, typename... Ps, EnableIf<!has_branch_with_same_conditions(condition_list(branchexpr<Ts...>{}), TypeList<Ps...>{})> = 0>
constexpr auto combine_branches_with_same_conditions(TypeList<branchexpr<Ts...>, Ps...>) {
  return List(branchexpr<Ts...>{}).Concat(combine_branches_with_same_conditions(TypeList<Ps...>{}));
}

template <typename... Ts, typename... Ps, EnableIf<has_branch_with_same_conditions(condition_list(branchexpr<Ts...>{}), TypeList<Ps...>{})> = 0>
constexpr auto combine_branches_with_same_conditions(TypeList<branchexpr<Ts...>, Ps...>) {
  constexpr auto result = find_branch_with_same_conditions(condition_list(branchexpr<Ts...>{}), TypeList<Ps...>{});
  
  constexpr auto combined = product_of_cases(List(Ts{}...), case_list(at<0>(result)));
  return combine_branches_with_same_conditions(List(combined).Concat(at<1>(result)));
}

template <typename... Ts, typename... Rs>
constexpr auto product_of_cases(TypeList<Ts...>, TypeList<Rs...>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(get_value(Ts{}) * get_value(Rs{})...));
}

template <typename... Ts, typename... Ps, EnableIf<has_branch_with_same_conditions(condition_list(branchexpr<Ts...>{}), TypeList<Ps...>{})> = 0>
constexpr auto combine_branches_with_same_conditions(TypeList<constantsubexpr<branchexpr<Ts...>>, Ps...>) {
  return combine_branches_with_same_conditions(TypeList<branchexpr<Ts...>, Ps...>{});
}

template <typename T, typename... Ps>
constexpr auto combine_branches_with_same_conditions(TypeList<T, Ps...>) {
  return List(T{}).Concat(combine_branches_with_same_conditions(TypeList<Ps...>{}));
}

template <typename... Ts>
constexpr auto simplify_adjacent_product(TypeList<Ts...> list) {
  constexpr auto updated = combine_branches_with_same_conditions(list);
  return simplify_adjacent_product_recursive(
    updated
    , simplify_adjacent_product_impl(updated)
  );
}

constexpr auto collect_scalarexprs(TypeList<>) {
  return List(List(), List());
}

template <typename T, typename... Ts, EnableIf<!is_scalarexpr(T{})> = 0>
constexpr auto collect_scalarexprs(TypeList<T, Ts...>) {
  return List(List(), TypeList<T, Ts...>{});
}

template <typename T, typename... Ts, EnableIf<is_scalarexpr(T{})> = 0>
constexpr auto collect_scalarexprs(TypeList<T, Ts...>) {
  constexpr auto rest = collect_scalarexprs(TypeList<Ts...>{});
  return List(List(T{}).Concat(at<0>(rest)), at<1>(rest));
}

template <typename... Ts, EnableIf<!is_scalarexpr(productexpr<Ts...>{})> = 0>
constexpr auto canonicalize_impl(productexpr<Ts...>) {
  constexpr auto result = partition(is_scalarexpr, TypeList<Ts...>{});
  constexpr auto scalars = at<0>(result);
  constexpr auto nonscalars = at<1>(result);
  return make_product(simplify_adjacent_product(sort(scalars).Concat(nonscalars)));
}

template <typename... Ts, EnableIf<is_scalarexpr(productexpr<Ts...>{})> = 0>
constexpr auto canonicalize_impl(productexpr<Ts...>) {
  return make_product(simplify_adjacent_product(sort(Ts{}...)));
}

template <typename A, typename B, EnableIf<is_scalarexpr(B{} && is_vector(A{}))> = 0>
constexpr auto product(baseexpr<A>, baseexpr<B>) {
  return make_expr(product_tag, B{}, A{});
}

template <typename A, typename B>
constexpr auto operator*(baseexpr<A>, baseexpr<B>) {
  return make_expr(product_tag, A{}, B{});
}

template <typename... Ts>
constexpr auto product_list(TypeList<Ts...>) {
  static_assert(sizeof...(Ts) > 0, "product_list() requires a list with at least one element");
  return (Ts{} * ...);
}


template <typename... Rs, typename... Ts, EnableIf<can_simplify_condition_lists(List(get_condition(Rs{})...), List(get_condition(Ts{})...))> = 0>
constexpr auto product(constantsubexpr<branchexpr<Rs...>>, constantsubexpr<branchexpr<Ts...>>) {
  return cartesian_product(TypeList<Rs...>{}, TypeList<Ts...>{});
}

template <typename... Rs, typename... Ts, EnableIf<can_simplify_condition_lists(List(get_condition(Rs{})...), List(get_condition(Ts{})...))> = 0>
constexpr auto product(branchexpr<Rs...>, constantsubexpr<branchexpr<Ts...>>) {
  return cartesian_product(TypeList<Rs...>{}, TypeList<Ts...>{});
}

template <typename... Rs, typename... Ts, EnableIf<can_simplify_condition_lists(List(get_condition(Rs{})...), List(get_condition(Ts{})...))> = 0>
constexpr auto product(constantsubexpr<branchexpr<Rs...>>, branchexpr<Ts...>) {
  return cartesian_product(TypeList<Rs...>{}, TypeList<Ts...>{});
}

template <typename... Rs, typename... Ts, EnableIf<can_simplify_condition_lists(List(get_condition(Rs{})...), List(get_condition(Ts{})...))> = 0>
constexpr auto product(branchexpr<Rs...>, branchexpr<Ts...>) {
  return cartesian_product(TypeList<Rs...>{}, TypeList<Ts...>{});
}

template <typename R, typename... Ts, EnableIf<is_scalarexpr(R{}) && !is_branchexpr(R{})> = 0>
constexpr auto product(baseexpr<R>, branchexpr<Ts...>) {
  return make_branch_expr(List(make_expr(branch_case_tag, get_condition(Ts{}), R{} * get_value(Ts{}))...));
}


template <typename A, typename B>
constexpr auto operator/(baseexpr<A>, baseexpr<B>) {
  return A{} * rcp(B{});
}

template <std::intmax_t N, std::intmax_t D, typename... Ts>
constexpr int size(productexpr<literal<N, D>, Ts...>) {
  return size(TypeList<Ts...>{}) + 1;
}

template <std::intmax_t N, std::intmax_t D, typename T>
constexpr int size(productexpr<literal<N, D>, T>) {
  return size(T{});
}

template <typename T, typename... Ts>
constexpr int size(productexpr<T, Ts...>) {
  return size(T{}) + size(TypeList<Ts...>{}) + 1;
}

template <typename... Ts, typename Var>
constexpr auto partition_dependent_subexprs(productexpr<Ts...>, Var) {
  constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
  return List(product_list(append(at<0>(parts), one)), product_list(append(at<1>(parts), one)));
}

template <typename T, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
constexpr auto partition_dependent_subexprs(baseexpr<T>, Var) {
  return List(T{}, one);
}

  
template <typename... Ts, typename Var>
constexpr auto var_subexpr(productexpr<Ts...>, Var) {
  constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
  return product_list(at<0>(parts));
}

template <typename T, EnableIf<is_leaf(T{}) || is_constant(T{})> = 0>
constexpr auto replace_constants_recursive(productexpr<minus_one_t, T>) {
  return unpack<expr>(product_tag, minus_one, replace_constants_recursive(T{}));
}

template <typename T, EnableIf<is_leaf(T{}) || is_constant(T{})> = 0>
constexpr auto replace_if_constant(productexpr<minus_one_t, T> p) {
  return p;
}

} // end namespace aether

#endif
