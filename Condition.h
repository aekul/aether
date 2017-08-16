#ifndef CONDITION_H
#define CONDITION_H

#include "Expr.h"
#include "aether/typelist/TypeSet.h"
#include "aether/typelist/Prepend.h"
#include "aether/typelist/Front.h"
#include "aether/typelist/Fill.h"
#include "aether/typelist/Filter.h"
#include "aether/typelist/Any.h"
#include "aether/typelist/Pick.h"
#include "aether/typelist/Contains.h"
#include "Literal.h"
#include "Variable.h"

namespace aether {

struct contains_branch_t {
  template <typename T, EnableIf<is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }

  template <typename T>
  constexpr bool operator()(coordinate_system<T>) const {
    return false;
  }

  template <std::size_t I>
  constexpr bool operator()(_size_t<I>) const {
    return false;
  }

  template <typename... Ts>
  constexpr bool operator()(branchexpr<Ts...>) const {
    return true;
  }

  template <typename... Ts>
  constexpr bool operator()(TypeList<Ts...>) const {
    return (this->operator()(Ts{}) || ... || false);
  }

  template <typename T, EnableIf<!is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return this->operator()(operands(T{}));
  }
};

constexpr contains_branch_t contains_branch{};

struct contains_branch_case_t {
  template <typename T, EnableIf<is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }

  template <typename T>
  constexpr bool operator()(coordinate_system<T>) const {
    return false;
  }

  template <std::size_t I>
  constexpr bool operator()(_size_t<I>) const {
    return false;
  }

  template <typename... Ts>
  constexpr bool operator()(branchexpr<Ts...>) const {
    return false;
  }

  template <typename C, typename T>
  constexpr bool operator()(branchcaseexpr<C, T>) const {
    return true;
  }

  template <typename... Ts>
  constexpr bool operator()(TypeList<Ts...>) const {
    return (this->operator()(Ts{}) || ... || false);
  }

  template <typename T, EnableIf<!is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return this->operator()(operands(T{}));
  }
};

constexpr contains_branch_case_t contains_branch_case{};


struct contains_inconsistent_branch_t {
  template <typename C, typename T, EnableIf<is_leaf(T{})> = 0>
  constexpr bool impl(baseexpr<C>, baseexpr<T>) const {
    return false;
  }

  template <typename C, typename T>
  constexpr bool impl(baseexpr<C>, coordinate_system<T>) const {
    return false;
  }

  template <typename C, std::size_t I>
  constexpr bool impl(baseexpr<C>, _size_t<I>) const {
    return false;
  }

  template <typename C, typename... Ts>
  constexpr bool impl(baseexpr<C> cond, TypeList<Ts...>) const {
    return (dispatch(cond, Ts{}) || ... || false);
  }

  template <typename C, typename T, EnableIf<!is_leaf(T{})> = 0>
  constexpr bool impl(baseexpr<C> cond, baseexpr<T>) const {
    return impl(cond, operands(T{}));
  }

  template <typename C, typename... Ts, EnableIf<!any_inconsistent(List(C{} && get_condition(Ts{})...))> = 0>
  constexpr bool impl(baseexpr<C>, branchexpr<Ts...>) const {
    return (dispatch(C{} && !get_condition(Ts{}), get_value(Ts{})) || ... || false);
  }

  template <typename C, typename... Ts, EnableIf<any_inconsistent(List(C{} && get_condition(Ts{})...))> = 0>
  constexpr bool impl(baseexpr<C>, branchexpr<Ts...>) const {
    return true;
  }

  template <typename C, typename T, EnableIf<!contains_branch(T{})> = 0>
  constexpr bool dispatch(baseexpr<C>, T) const {
    return false;
  }

  template <typename C, typename T, EnableIf<contains_branch(T{})> = 0>
  constexpr bool dispatch(baseexpr<C> cond, T) const {
    return impl(cond, T{});
  }

  template <typename T>
  constexpr bool operator()(T) const {
    return dispatch(trueexpr{}, T{});
  }

  template <typename... Ts>
  constexpr bool operator()(branchexpr<Ts...>) const {
    return (dispatch(get_condition(Ts{}), get_value(Ts{})) || ... || false);
  }
};

constexpr contains_inconsistent_branch_t contains_inconsistent_branch{};

template <typename C, typename V>
constexpr C get_condition(branchcaseexpr<C, V>) {
  return {};
}

template <typename... Ts>
constexpr auto condition_list(branchexpr<Ts...>) {
  return List(get_condition(Ts{})...);
}

template <typename... Ts>
constexpr auto condition_list(constantsubexpr<branchexpr<Ts...>>) {
  return List(get_condition(Ts{})...);
}

template <typename C, typename V>
constexpr V get_value(branchcaseexpr<C, V>) {
  return {};
}

template <typename... Ts>
constexpr auto value_list(branchexpr<Ts...>) {
  return List(get_value(Ts{})...);
}

template <typename... Ts>
constexpr auto case_list(branchexpr<Ts...>) {
  return List(Ts{}...);
}

template <typename... Ts>
constexpr auto case_list(constantsubexpr<branchexpr<Ts...>>) {
  return List(Ts{}...);
}

template <typename... Cases>
constexpr auto pattern(Cases... cases) {
  return make_branch_expr(List(cases...));
}

template <typename C, typename V>
constexpr auto when(C cond, V value) {
  return make_expr(branch_case_tag, cond, value);
}

template <typename V>
constexpr auto when(bool_t<true>, V value) {
  return make_expr(branch_case_tag, trueexpr{}, value);
}

template <typename V>
constexpr auto when(trueexpr, V value) {
  return make_expr(branch_case_tag, trueexpr{}, value);
}

template <typename V>
constexpr auto when(bool_t<false>, V value) {
  return make_expr(branch_case_tag, falseexpr{}, value);
}

template <typename V>
constexpr auto when(falseexpr, V value) {
  return make_expr(branch_case_tag, falseexpr{}, value);
}

template <typename V>
constexpr auto otherwise(V value) {
  return make_expr(branch_case_tag, trueexpr{}, value);
}

template <typename... Ts>
constexpr auto make_balance_expr(branchexpr<Ts...> value) {
  return make_expr(balance_tag, value);
}

template <typename A, typename B>
constexpr lessthanequalexpr<A, B> operator<=(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename T>
constexpr T make_balance_expr(baseexpr<T>) {
  return {};
}

template <typename A, typename B>
constexpr bool same_conditions(baseexpr<A>, baseexpr<B>) {
  return false;
}

template <typename A>
constexpr bool same_conditions(baseexpr<A>, baseexpr<A>) {
  return true;
}

template <typename A>
constexpr bool same_conditions(constantsubexpr<A>, baseexpr<A>) {
  return true;
}

template <typename A>
constexpr bool same_conditions(baseexpr<A>, constantsubexpr<A>) {
  return true;
}

template <typename A>
constexpr bool same_conditions(constantsubexpr<A>, constantsubexpr<A>) {
  return true;
}

template <typename... As, typename... Bs, EnableIf<sizeof...(As) == sizeof...(Bs)> = 0>
constexpr bool same_conditions(TypeList<As...>, TypeList<Bs...>) {
  return (same_conditions(As{}, Bs{}) && ... && true);
}

template <typename... As, typename... Bs, EnableIf<sizeof...(As) != sizeof...(Bs)> = 0>
constexpr bool same_conditions(TypeList<As...>, TypeList<Bs...>) {
  return false;
}

template <typename... Ts, EnableIf<all_same(List(get_value(Ts{})...))> = 0>
constexpr auto make_branch_expr(TypeList<Ts...>) {
  return front(List(get_value(Ts{})...));
}

struct condition_is_false_t {
  template <typename C, typename V>
  constexpr bool operator()(branchcaseexpr<C, V>) const {
    return is_falseexpr(C{});
  }
};

constexpr condition_is_false_t condition_is_false{};

template <typename... Ts>
constexpr auto make_branch_expr_impl(TypeList<Ts...>) {
  return make_expr(branch_tag, Ts{}...);
}

template <typename T, EnableIf<is_trueexpr(get_condition(T{}))> = 0>
constexpr auto make_branch_expr_impl(TypeList<T>) {
  return get_value(T{});
}

constexpr auto pick_until_true(TypeList<>) {
  return List();
}

template <typename T, typename... Ts, EnableIf<is_trueexpr(get_condition(T{}))> = 0>
constexpr auto pick_until_true(TypeList<T, Ts...>) {
  return List(T{});
}

template <typename T, typename... Ts, EnableIf<!is_trueexpr(get_condition(T{}))> = 0>
constexpr auto pick_until_true(TypeList<T, Ts...>) {
  return List(T{}).Concat(pick_until_true(TypeList<Ts...>{}));
}

template <typename... Ts, EnableIf<!all_same(List(get_value(Ts{})...))> = 0>
constexpr auto make_branch_expr(TypeList<Ts...>) {
  constexpr auto cases = pick_until_true(remove_if(condition_is_false, List(Ts{}...)));
  return make_branch_expr_impl(cases);
}

template <typename... Cs, typename... Vs>
constexpr auto make_branch_expr(TypeList<Cs...>, TypeList<Vs...>) {
  return make_branch_expr(List(make_expr(branch_case_tag, Cs{}, Vs{})...));
}

template <typename C, typename V>
constexpr auto unpack_for_branch_case(baseexpr<C>, baseexpr<V>) {
  return unpack<expr>(branch_case_tag, C{}, V{});
}

template <typename... Cs, typename... Vs>
constexpr auto unpack_for_branch(TypeList<Cs...>, TypeList<Vs...>) {
  return unpack<expr>(branch_tag, unpack_for_branch_case(Cs{}, Vs{})...);
}

template <typename... Cases>
constexpr auto unpack_for_branch(TypeList<Cases...>) {
  return unpack<expr>(branch_tag, Cases{}...);
}


template <std::size_t I, typename C, typename V, EnableIf<is_vector(branchcaseexpr<C, V>{})> = 0>
constexpr auto at(branchcaseexpr<C, V>) {
  return make_expr(branch_case_tag, C{}, at<I>(V{}));
}

template <std::size_t I, typename... Ts, EnableIf<is_vector(branchexpr<Ts...>{})> = 0>
constexpr auto at(branchexpr<Ts...>) {
  return make_branch_expr(
    List(get_condition(Ts{})...)
    , List(at<I>(get_value(Ts{}))...)
  );
}

template <typename Cond>
constexpr auto remove_inconsistent_paths_single_impl(baseexpr<Cond>, TypeList<>) {
  return List(List(), List());
}

template <typename Cond, typename C, typename V, typename... Cases, EnableIf<!consistent(Cond{} && C{})> = 0>
constexpr auto remove_inconsistent_paths_single_impl(baseexpr<Cond>, TypeList<branchcaseexpr<C, V>, Cases...>) { 
  return remove_inconsistent_paths_single_impl(Cond{} && !C{}, TypeList<Cases...>{});
}

template <typename Cond, typename C, typename V, typename... Cases, EnableIf<consistent(Cond{} && C{})> = 0>
constexpr auto remove_inconsistent_paths_single_impl(baseexpr<Cond>, TypeList<branchcaseexpr<C, V>, Cases...>) {
  constexpr auto rest = remove_inconsistent_paths_single_impl(Cond{} && !C{}, TypeList<Cases...>{});
  constexpr auto result = remove_inconsistent_paths_single(Cond{} && C{}, V{});
  return List(List(remove_inconsistent_paths_single(Cond{}, C{})).Concat(at<0>(rest)), List(result).Concat(at<1>(rest)));
}

template <typename Cond, typename Value>
constexpr auto make_branch_from_consistent_paths(TypeList<Cond>, TypeList<Value>) {
  return Value{};
}

template <typename... Conds, typename... Values>
constexpr auto make_branch_from_consistent_paths(TypeList<Conds...> conds, TypeList<Values...> values) {
  return unpack_for_branch(conds, values);
}

template <typename Cond, typename... Ts>
constexpr auto remove_inconsistent_paths_single(baseexpr<Cond>, branchexpr<Ts...>) {
  constexpr auto result = remove_inconsistent_paths_single_impl(Cond{}, TypeList<Ts...>{});
  return make_branch_from_consistent_paths(at<0>(result), at<1>(result));
}

template <typename Cond, typename... Ts>
constexpr auto remove_inconsistent_paths_single(baseexpr<Cond>, TypeList<Ts...>) {
  return List(remove_inconsistent_paths_single(Cond{}, Ts{})...);
}

template <typename Cond, typename Value, EnableIf<!is_leaf(Value{})> = 0>
constexpr auto remove_inconsistent_paths_single(baseexpr<Cond>, baseexpr<Value>) {
  return unpack_list<expr>(List(tag_of(Value{})).Concat(remove_inconsistent_paths_single(Cond{}, operands(Value{}))));
}

template <typename Cond, typename T>
constexpr T remove_inconsistent_paths_single(baseexpr<Cond>, coordinate_system<T>) {
  return {};
}

template <typename Cond, std::size_t I>
constexpr _size_t<I> remove_inconsistent_paths_single(baseexpr<Cond>, _size_t<I>) {
  return {};
}

template <typename Cond, typename Value, EnableIf<is_leaf(Value{})> = 0>
constexpr Value remove_inconsistent_paths_single(baseexpr<Cond>, baseexpr<Value>) {
  return {};
}

template <typename Cond>
constexpr auto remove_inconsistent_paths(baseexpr<Cond>, TypeList<>, TypeList<>) {
  return List(List(), List());
}

template <typename Cond, typename C, typename... Conds, typename V, typename... Values>
constexpr auto remove_inconsistent_paths(baseexpr<Cond>, TypeList<C, Conds...>, TypeList<V, Values...>) {
  constexpr auto result = List(List(C{}), List(remove_inconsistent_paths_single(Cond{} && C{}, V{})));
  constexpr auto rest = remove_inconsistent_paths(Cond{} && !C{}, TypeList<Conds...>{}, TypeList<Values...>{});
  return List(at<0>(result).Concat(at<0>(rest)), at<1>(result).Concat(at<1>(rest)));
}

template <typename C, typename T>
constexpr auto get_value_for_condition(baseexpr<C>, baseexpr<T>) {
  return remove_inconsistent_paths_single(C{}, T{});
}


template <typename... Ts, EnableIf<!contains_inconsistent_branch(branchexpr<Ts...>{})> = 0>
constexpr auto canonicalize_impl(branchexpr<Ts...> br) {
  return br;
}

template <typename... Ts, EnableIf<contains_inconsistent_branch(branchexpr<Ts...>{})> = 0>
constexpr auto canonicalize_impl(branchexpr<Ts...>) {
  constexpr auto result = remove_inconsistent_paths(trueexpr{}, List(get_condition(Ts{})...), List(get_value(Ts{})...));
  return unpack_for_branch(at<0>(result), at<1>(result));
}

template <typename... Ts>
constexpr auto and_all(TypeList<Ts...>) {
  return (Ts{} && ... && trueexpr{});
}

template <typename... Ts>
constexpr auto and_all(TypeSet<Ts...>) {
  return (Ts{} && ... && trueexpr{});
}

template <typename C, typename... Cs>
constexpr auto and_all(baseexpr<C>, TypeList<Cs...>) {
  return List(C{} && Cs{}...);
}

template <typename... Cs, typename... Vs>
constexpr auto zip_conditions_and_values(TypeList<Cs...>, TypeList<Vs...>) {
  return List(make_expr(branch_case_tag, Cs{}, Vs{})...);
}

template <typename... Cs, typename... Vs>
constexpr auto and_conditions_and_values(TypeList<Cs...>, TypeList<Vs...>) {
  return List(Cs{} && Vs{}...);
}

template <typename C, typename V, EnableIf<is_branchexpr(V{})> = 0>
constexpr auto flatten_cases(branchcaseexpr<C, V>) {
  constexpr auto conds = and_all(C{}, condition_list(V{}));
  constexpr auto values = value_list(V{});
  return zip_conditions_and_values(conds, values);
}

template <typename C, typename V, EnableIf<!is_branchexpr(V{})> = 0>
constexpr auto flatten_cases(branchcaseexpr<C, V> bc) {
  return List(bc);
}

template <typename C, typename V, EnableIf<is_branchexpr(C{})> = 0>
constexpr auto flatten_conditions(branchcaseexpr<C, V>) {
  constexpr auto conds = condition_list(C{});
  constexpr auto values = value_list(C{});
  constexpr auto new_conds = and_conditions_and_values(conds, values);
  return zip_conditions_and_values(new_conds, fill(_size_t<new_conds.Size()>{}, V{}));
}

template <typename C, typename V, EnableIf<!is_branchexpr(C{})> = 0>
constexpr auto flatten_conditions(branchcaseexpr<C, V> bc) {
  return List(bc);
}

template <typename CondSetA, typename CondSetB, typename... Bs>
constexpr auto cartesian_product(CondSetA, TypeList<>, CondSetB, TypeList<Bs...>) {
  return List(List(), List());
}

template <typename CondSetA, typename A, typename CondSetB>
constexpr auto cartesian_product_single(CondSetA, baseexpr<A>, CondSetB, TypeList<>) {
  return List(List(), List());
}

template <typename CondSetA, typename A, typename CondSetB, typename B, typename... Bs, EnableIf<!consistent(CondSetA{} && get_condition(A{}) && CondSetB{} && get_condition(B{}))> = 0>
constexpr auto cartesian_product_single(CondSetA csa, baseexpr<A>, CondSetB csb, TypeList<B, Bs...>) {
  return cartesian_product_single(csa, A{}, csb && !get_condition(B{}), TypeList<Bs...>{});
}

template <typename CondSetA, typename A, typename CondSetB, typename B, typename... Bs, EnableIf<consistent(CondSetA{} && get_condition(A{}) && CondSetB{} && get_condition(B{}))> = 0>
constexpr auto cartesian_product_single(CondSetA csa, baseexpr<A>, CondSetB csb, TypeList<B, Bs...>) {
  constexpr auto cond = get_condition(B{}) && get_condition(A{});
  constexpr auto value = get_value(A{}) * get_value(B{});
  constexpr auto result = List(List(cond), List(value));
  constexpr auto rest = cartesian_product_single(csa, A{}, csb && !get_condition(B{}), TypeList<Bs...>{});
  return List(at<0>(result).Concat(at<0>(rest)), at<1>(result).Concat(at<1>(rest)));
}
template <typename CondSetA, typename A, typename... As, typename CondSetB, typename... Bs>
constexpr auto cartesian_product(CondSetA csa, TypeList<A, As...>, CondSetB csb, TypeList<Bs...> list_b) {
  constexpr auto result = cartesian_product_single(csa, A{}, csb, list_b);
  constexpr auto rest = cartesian_product(csa && !get_condition(A{}), TypeList<As...>{}, csb, list_b);
  return List(at<0>(result).Concat(at<0>(rest)), at<1>(result).Concat(at<1>(rest)));
}

template <typename... As, typename... Bs>
constexpr auto cartesian_product(TypeList<As...> as, TypeList<Bs...> bs) {
  constexpr auto result = cartesian_product(trueexpr{}, as, trueexpr{}, bs);
  return make_branch_expr(at<0>(result), at<1>(result));
}

template <typename C1, typename V1, typename C2, typename V2>
constexpr bool are_compatible(branchcaseexpr<C1, V1>, branchcaseexpr<C2, V2>) {
  return !C1{} == C2{};
}

template <typename C, typename V>
constexpr int size(branchcaseexpr<C, V>) {
  return size(C{}) + size(V{});
}

template <typename... Ts>
constexpr int size(branchexpr<Ts...>) {
  return (size(Ts{}) + ... + 0);
}

template <typename T>
constexpr bool inconsistent(baseexpr<T>) {
  return !consistent(T{});
}

template <typename... Ts>
constexpr bool any_inconsistent(TypeList<Ts...>) {
  return (inconsistent(Ts{}) || ... || false);
}

template <typename T>
constexpr bool consistent(baseexpr<T>) {
  return true;
}

constexpr bool consistent(falseexpr) {
  return false;
}

template <typename... Ts, typename B>
constexpr bool contains_negation(TypeSet<Ts...> cond_set, constantsubexpr<B>) {
  return cond_set.Has(!B{}) || cond_set.Has(!make_expr(constant_subexpr_tag, B{})) || cond_set.Has(make_expr(constant_subexpr_tag, !B{}));
}

template <typename... Ts, typename B>
constexpr bool contains_negation(TypeSet<Ts...> cond_set, baseexpr<B>) {
  return cond_set.Has(!B{}) || cond_set.Has(!make_expr(constant_subexpr_tag, B{})) || cond_set.Has(make_expr(constant_subexpr_tag, !B{}));
}

template <typename... Ts, typename B>
constexpr bool contains_negation(TypeList<Ts...> cond_list, constantsubexpr<B>) {
  return contains(!B{}, cond_list) || contains(!make_expr(constant_subexpr_tag, B{}), cond_list) || contains(make_expr(constant_subexpr_tag, !B{}), cond_list);
}

template <typename... Ts, typename B>
constexpr bool contains_negation(TypeList<Ts...> cond_list, baseexpr<B>) {
  return contains(!B{}, cond_list) || contains(!make_expr(constant_subexpr_tag, B{}), cond_list) || contains(make_expr(constant_subexpr_tag, !B{}), cond_list);
}

template <typename A, typename B>
constexpr bool is_negation_of(baseexpr<A>, baseexpr<B>) {
  return contains_negation(make_type_set(A{}), B{});
}

template <typename... As, typename... Bs>
constexpr bool have_conflicts(TypeSet<As...> a, TypeSet<Bs...>) {
  return (contains_negation(a, Bs{}) || ... || false); 
}



template <typename A, typename B>
constexpr auto operator&&(baseexpr<A>, baseexpr<B>) {
  return make_expr(logical_and_tag, A{}, B{});
}

template <typename A, typename B>
constexpr auto operator&&(constantsubexpr<A>, baseexpr<B>) {
  return A{} && B{};
}

template <typename A, typename B>
constexpr auto operator&&(baseexpr<A>, constantsubexpr<B>) {
  return A{} && B{};
}

template <typename A, typename B>
constexpr auto operator&&(constantsubexpr<A>, constantsubexpr<B>) {
  return A{} && B{};
}

template <typename A, typename B>
constexpr auto operator||(baseexpr<A>, baseexpr<B>) {
  return make_expr(logical_or_tag, A{}, B{});
}

template <typename A, typename B>
constexpr lessthanexpr<A, B> operator<(baseexpr<A>, baseexpr<B>) {
  return {};
}


template <typename A, typename B>
constexpr greaterthanexpr<A, B> operator>(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename A, typename B>
constexpr greaterthanequalexpr<A, B> operator>=(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename A, typename B>
constexpr equalexpr<A, B> operator==(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename A, typename B>
constexpr notequalexpr<A, B> operator!=(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename A, typename B>
constexpr greaterthanequalexpr<A, B> operator!(lessthanexpr<A, B>) {
  return {};
}

template <typename A, typename B>
constexpr lessthanequalexpr<A, B> operator!(greaterthanexpr<A, B>) {
  return {};
}

template <typename A, typename B>
constexpr greaterthanexpr<A, B> operator!(lessthanequalexpr<A, B>) {
  return {};
}

template <typename A, typename B>
constexpr lessthanexpr<A, B> operator!(greaterthanequalexpr<A, B>) {
  return {};
}

template <typename A, typename B>
constexpr notequalexpr<A, B> operator!(equalexpr<A, B>) {
  return {};
}

template <typename A, typename B>
constexpr equalexpr<A, B> operator!(notequalexpr<A, B>) {
  return {};
}

template <typename A>
constexpr logicalnotexpr<A> operator!(baseexpr<A>) {
  return {};
}



template <typename... Ts>
constexpr auto expand_conditions(TypeList<Ts...>, TypeList<>) {
  return List();
}

template <typename... Ts, typename C, typename... Cs>
constexpr auto expand_conditions(TypeList<Ts...> done, TypeList<C, Cs...>) {
  constexpr auto cond = C{} && (!Ts{} && ... && trueexpr{});
  constexpr auto rest = expand_conditions(append(done, C{}), TypeList<Cs...>{});
  return List(cond).Concat(rest);
}

template <typename... Cs>
constexpr auto expand_conditions(TypeList<Cs...> conds) {
  return expand_conditions(List(), conds);
}


template <typename A, typename B>
constexpr auto merge_operands_pair(A a, B b) {
  return List(a, b);
}

template <typename... As, typename B>
constexpr auto merge_operands_pair(TypeList<As...>, B b) {
  return List(As{}..., b);
}

template <typename A, typename... Bs>
constexpr auto merge_operands_pair(A a, TypeList<Bs...>) {
  return List(a, Bs{}...);
}

template <typename... As, typename... Bs>
constexpr auto merge_operands_pair(TypeList<As...>, TypeList<Bs...>) {
  return List(As{}..., Bs{}...);
}

template <typename CondsA, typename A, typename CondsB, typename B, EnableIf<!have_conflicts(CondsA{}, CondsB{})> = 0>
constexpr auto merge_conditions_and_operands_pair(TypeList<CondsA, A>, TypeList<CondsB, B>) {
  return List(List(CondsA{}.Merge(CondsB{}), merge_operands_pair(A{}, B{})));
}

template <typename CondsA, typename A, typename CondsB, typename B, EnableIf<have_conflicts(CondsA{}, CondsB{})> = 0>
constexpr auto merge_conditions_and_operands_pair(TypeList<CondsA, A>, TypeList<CondsB, B>) {
  return List();
}

template <typename Conds, typename E, typename... Rs>
constexpr auto merge_conditions_and_operands_pairwise_impl(TypeList<Conds, E> a, TypeList<Rs...>) {
  return concat(merge_conditions_and_operands_pair(a, Rs{})...);
}

template <typename Conds, typename E>
constexpr TypeList<Conds, TypeList<E>> convert_expr_to_list(TypeList<Conds, E>) {
  return {};
}

template <typename Conds, typename... Operands>
constexpr auto convert_expr_to_list(TypeList<Conds, TypeList<Operands...>> a) {
  return a;
}

template <typename... As>
constexpr auto merge_conditions_and_operands_pairwise(TypeList<As...>) {
  return List(convert_expr_to_list(As{})...);
}

template <typename... As, typename... Bs, typename... Others>
constexpr auto merge_conditions_and_operands_pairwise(TypeList<As...>, TypeList<Bs...> b, Others... others) {
  return merge_conditions_and_operands_pairwise(concat(merge_conditions_and_operands_pairwise_impl(As{}, b)...), others...);
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr auto move_conditions_outward_impl(baseexpr<T>) {
  return List(List(make_type_set(), T{}));
}

template <typename T>
constexpr auto move_conditions_outward_impl(coordinate_system<T>) {
  return List(List(make_type_set(), T{}));
}

template <std::size_t I>
constexpr auto move_conditions_outward_impl(_size_t<I> i) {
  return List(List(make_type_set(), i));
}

template <typename... Ts>
constexpr auto move_conditions_outward_impl(TypeList<Ts...>) {
  return merge_conditions_and_operands_pairwise(move_conditions_outward(Ts{})...);
}

template <typename Cond>
constexpr auto add_condition(Cond, TypeList<>) {
  return List();
}

template <typename Cond, typename T, typename... Ts, EnableIf<contains_negation(at<0>(T{}), Cond{})> = 0>
constexpr auto add_condition(Cond cond, TypeList<T, Ts...>) {
  return add_condition(cond, TypeList<Ts...>{});
}

template <typename Cond, typename T, typename... Ts, EnableIf<!contains_negation(at<0>(T{}), Cond{})> = 0>
constexpr auto add_condition(Cond cond, TypeList<T, Ts...>) {
  constexpr auto first = List(at<0>(T{}).Put(Cond{}), at<1>(T{}));
  constexpr auto rest = add_condition(cond, TypeList<Ts...>{});
  return prepend(first, rest);
}

template <typename... Conds, typename... Values, typename... Cases>
constexpr auto move_conditions_outward_helper(TypeList<Conds...>, TypeList<Values...>, TypeList<Cases...>) {
  return concat(add_condition(Conds{}, move_conditions_outward(Values{}))...);
}


template <typename... Ts, EnableIf<!is_constant(branchexpr<Ts...>{})> = 0>
constexpr auto move_conditions_outward_impl(branchexpr<Ts...>) {
  constexpr auto conds = expand_conditions(List(get_condition(Ts{})...));
  constexpr auto values = List(get_value(Ts{})...);

  return move_conditions_outward_helper(conds, values, List(Ts{}...));
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto move_conditions_outward_impl(baseexpr<T>) {
  return unpack_operands(tag_of(T{}), move_conditions_outward(operands(T{})));
}

template <typename T>
constexpr auto move_conditions_outward_impl(constantsubexpr<T> cs) {
  return List(List(make_type_set(), cs));
}

template <typename T, EnableIf<!contains_branch(T{})> = 0>
constexpr auto move_conditions_outward(T) {
  return List(List(make_type_set(), T{}));
}

template <typename T, EnableIf<contains_branch(T{})> = 0>
constexpr auto move_conditions_outward(T) {
  return move_conditions_outward_impl(T{});
}





template <typename T>
constexpr auto extract_conditions_impl(coordinate_system<T>) {
  return List(List(), T{});
}

template <std::size_t N>
constexpr auto extract_conditions_impl(_size_t<N> s) {
  return List(List(), s);
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr auto extract_conditions_impl(baseexpr<T>) {
  return List(List(), T{});
}

template <typename... Ts>
constexpr auto extract_conditions_impl(TypeList<Ts...>) {
  return List(extract_conditions_impl(Ts{})...);
}

template <typename... Ts>
constexpr auto extract_conditions_impl(branchexpr<Ts...> br) {
  return List(List(), br);
}

template <typename T>
constexpr T flatten_conditions_impl(baseexpr<T>) {
  return {};
}

template <typename C, typename T, typename V>
constexpr auto flatten_conditions_impl(greaterthanexpr<branchcaseexpr<C, T>, V>) {
  return T{} > V{} && C{};
}

template <typename C, typename T>
constexpr auto extract_conditions_impl(branchcaseexpr<C, T>) {
  constexpr auto result = extract_conditions_impl(T{});
  return List(append(at<0>(result), flatten_conditions_impl(C{})), at<1>(result));
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto extract_conditions_impl(baseexpr<T>) {
  constexpr auto result = extract_conditions_impl(operands(T{}));
  return List(
    flatten(pick<0>(result))
    , unpack_list<expr>(List(tag_of(T{})).Concat(pick<1>(result)))
  );
}

template <typename T>
constexpr auto extract_conditions_impl(constantsubexpr<T>) {
  return extract_conditions_impl(T{});
}

template <typename T>
constexpr auto extract_conditions(baseexpr<T>) {
  constexpr auto result = extract_conditions_impl(T{});
  return List(and_all(at<0>(result)), at<1>(result));
}




template <typename A, typename... Ts>
constexpr logicalandexpr<Ts...> logicaland(baseexpr<A>, logicalnotexpr<logicalandexpr<A, Ts...>>) {
  return {};
}

template <typename A>
constexpr A logicaland(baseexpr<A>, trueexpr) {
  return {};
}

template <typename A>
constexpr A logicaland(trueexpr, baseexpr<A>) {
  return {};
}
  
constexpr trueexpr logicaland(trueexpr, trueexpr) {
  return {};
}

template <typename A>
constexpr A logicaland(baseexpr<A>, constantsubexpr<A>) {
  return {};
}

template <typename A>
constexpr A logicaland(constantsubexpr<A>, constantsubexpr<A>) {
  return {};
}

template <typename A>
constexpr A logicaland(constantsubexpr<A>, baseexpr<A>) {
  return {};
}

template <typename A, typename B, EnableIf<is_negation_of(A{}, B{})> = 0>
constexpr falseexpr logicaland(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename A>
constexpr falseexpr logicaland(baseexpr<A>, falseexpr) {
  return {};
}

template <typename A>
constexpr falseexpr logicaland(falseexpr, baseexpr<A>) {
  return {};
}

constexpr falseexpr logicaland(falseexpr, trueexpr) {
  return {};
}

constexpr falseexpr logicaland(trueexpr, falseexpr) {
  return {};
}

template <typename A>
constexpr falseexpr logicaland(baseexpr<A>, decltype(!A{})) {
  return {};
}

template <typename A>
constexpr A logicaland(baseexpr<A>, baseexpr<A>) {
  return {};
}

template <typename A>
constexpr A logicaland(logicalandexpr<A>) {
  return {};
}

template <typename A, typename B1, typename B2>
constexpr falseexpr logicaland(equalexpr<A, B1>, equalexpr<A, B2>) {
  return {};
}

template <typename A, typename B1, typename B2, EnableIf<!std::is_same<B1, B2>::value> = 0>
constexpr auto logicaland(equalexpr<A, B1>, notequalexpr<A, B2>) {
  return equalexpr<A, B1>{};
}

template <typename A, typename B1, typename B2, EnableIf<!std::is_same<B1, B2>::value> = 0>
constexpr auto logicaland(notequalexpr<A, B1>, equalexpr<A, B2>) {
  return equalexpr<A, B2>{};
}

template <typename A, typename B1, typename B2>
constexpr falseexpr logicaland(constantsubexpr<equalexpr<A, B1>>, equalexpr<A, B2>) {
  return {};
}

template <typename A, typename B1, typename B2>
constexpr falseexpr logicaland(equalexpr<A, B1>, constantsubexpr<equalexpr<A, B2>>) {
  return {};
}

template <typename A, typename B1, typename B2>
constexpr falseexpr logicaland(constantsubexpr<equalexpr<A, B1>>, constantsubexpr<equalexpr<A, B2>>) {
  return {};
}

template <typename... Ts>
constexpr logicalandexpr<Ts...> logicaland(trueexpr, logicalandexpr<Ts...>) {
  return {};
}

template <typename... Ts>
constexpr falseexpr logicaland(falseexpr, logicalandexpr<Ts...>) {
  return {};
}

template <typename A, typename... Ts, EnableIf<!contains_negation(TypeList<Ts...>{}, A{})> = 0>
constexpr logicalandexpr<A, Ts...> logicaland(baseexpr<A>, logicalandexpr<Ts...>) {
  return {};
}

template <typename A, typename... Ts, EnableIf<contains_negation(TypeList<Ts...>{}, A{})> = 0>
constexpr falseexpr logicaland(baseexpr<A>, logicalandexpr<Ts...>) {
  return {};
}

template <typename... Ts, typename A, EnableIf<!contains_negation(TypeList<Ts...>{}, A{})> = 0>
constexpr logicalandexpr<Ts..., A> logicaland(logicalandexpr<Ts...>, baseexpr<A>) {
  return {};
}

template <typename... Ts, typename A, EnableIf<contains_negation(TypeList<Ts...>{}, A{})> = 0>
constexpr falseexpr logicaland(logicalandexpr<Ts...>, baseexpr<A>) {
  return {};
}

template <typename... Ts>
constexpr falseexpr logicaland(logicalandexpr<Ts...>, falseexpr) {
  return {};
}

template <typename CondList, typename... Ts>
constexpr bool contains_negation_of_any(CondList, TypeList<Ts...>) {
  return (contains_negation(CondList{}, Ts{}) || ... || false);
}

template <typename... As, typename... Bs, EnableIf<!contains_negation_of_any(TypeList<As...>{}, TypeList<Bs...>{})> = 0>
constexpr logicalandexpr<As..., Bs...> logicaland(logicalandexpr<As...>, logicalandexpr<Bs...>) {
  return {};
}

template <typename... As, typename... Bs, EnableIf<contains_negation_of_any(TypeList<As...>{}, TypeList<Bs...>{})> = 0>
constexpr falseexpr logicaland(logicalandexpr<As...>, logicalandexpr<Bs...>) {
  return {};
}

template <typename A, typename B, typename E = void_t<>>
struct can_simplify_logical_and_t : std::false_type {
};

template <typename A, typename B>
struct can_simplify_logical_and_t<A, B, void_t<decltype(logicaland(A{}, B{}))>> : std::true_type {
};

template <typename A, typename B>
constexpr bool can_simplify_logical_and(baseexpr<A>, baseexpr<B>) {
  return can_simplify_logical_and_t<A, B>::value;
}

template <typename A, typename B>
constexpr bool can_simplify_logical_and(A, B) {
  return can_simplify_logical_and_t<A, B>::value;
}

template <typename Cond>
constexpr bool can_simplify_condition_lists_single(baseexpr<Cond>, TypeList<>) {
  return false;
}

template <typename Cond>
constexpr bool can_simplify_condition_lists_single(baseexpr<Cond>, TypeList<trueexpr>) {
  return false;
}

template <typename Cond, typename B1, typename B2, typename... Bs, EnableIf<!can_simplify_logical_and(Cond{}, B1{})> = 0>
constexpr bool can_simplify_condition_lists_single(baseexpr<Cond>, TypeList<B1, B2, Bs...>) {
  return can_simplify_condition_lists_single(Cond{} && !B1{}, TypeList<B2, Bs...>{});
}

template <typename Cond, typename B1, typename B2, typename... Bs, EnableIf<can_simplify_logical_and(Cond{}, B1{})> = 0>
constexpr bool can_simplify_condition_lists_single(baseexpr<Cond>, TypeList<B1, B2, Bs...>) {
  return true;
}


template <typename Cond, typename... Bs>
constexpr bool can_simplify_condition_lists_impl(baseexpr<Cond>, TypeList<>, TypeList<Bs...>) {
  return false;
}

template <typename Cond, typename... Bs>
constexpr bool can_simplify_condition_lists_impl(baseexpr<Cond>, TypeList<trueexpr>, TypeList<Bs...>) {
  return false;
}

template <typename Cond, typename A1, typename A2, typename... As, typename... Bs, EnableIf<can_simplify_condition_lists_single(Cond{} && A1{}, TypeList<Bs...>{})> = 0>
constexpr bool can_simplify_condition_lists_impl(baseexpr<Cond>, TypeList<A1, A2, As...>, TypeList<Bs...>) {
  return true;
}

template <typename Cond, typename A1, typename A2, typename... As, typename... Bs, EnableIf<!can_simplify_condition_lists_single(Cond{} && A1{}, TypeList<Bs...>{})> = 0>
constexpr bool can_simplify_condition_lists_impl(baseexpr<Cond>, TypeList<A1, A2, As...>, TypeList<Bs...> list_b) {
  return can_simplify_condition_lists_impl(Cond{} && !A1{}, TypeList<A2, As...>{}, list_b);
}

template <typename... As, typename... Bs>
constexpr bool can_simplify_condition_lists(TypeList<As...> list_a, TypeList<Bs...> list_b) {
  return can_simplify_condition_lists_impl(trueexpr{}, list_a, list_b);
}

template <typename A>
constexpr TypeList<A> simplify_adjacent_logical_and_impl(TypeList<A>) {
  return {};
}

template <typename A, typename B, typename... Ts, EnableIf<can_simplify_logical_and(A{}, B{})> = 0>
constexpr auto simplify_adjacent_logical_and_impl(TypeList<A, B, Ts...>) {
  return simplify_adjacent_logical_and_impl(List(logicaland(A{}, B{})).Concat(TypeList<Ts...>{}));
}

template <typename A, typename B, typename... Ts, EnableIf<!can_simplify_logical_and(A{}, B{})> = 0>
constexpr auto simplify_adjacent_logical_and_impl(TypeList<A, B, Ts...>) {
  return List(A{}).Concat(simplify_adjacent_logical_and_impl(TypeList<B, Ts...>{}));
}

template <typename Current>
constexpr Current simplify_adjacent_logical_and_recursive(Current, Current) {
  return {};
}

template <typename Current, typename Next>
constexpr auto simplify_adjacent_logical_and_recursive(Current, Next) {
  return simplify_adjacent_logical_and(Next{}); 
}

template <typename... Ts>
constexpr auto simplify_adjacent_logical_and(TypeList<Ts...>) {
  return simplify_adjacent_logical_and_recursive(
    TypeList<Ts...>{}
    , simplify_adjacent_logical_and_impl(TypeList<Ts...>{})
  );
}

template <typename A>
constexpr A make_and(TypeList<A>) {
  return {};
}

template <typename... Ts>
constexpr logicalandexpr<Ts...> make_and(TypeList<logicalandexpr<Ts...>>) {
  return {};
}

template <typename... Ts>
constexpr logicalandexpr<Ts...> make_and(TypeList<Ts...>) {
  return {};
}

template <typename A, typename B>
constexpr logicalandexpr<A, B> make_and(baseexpr<A>, baseexpr<B>) {
  return {};
}

template <typename T>
constexpr T extract_from_constant_subexpr(baseexpr<T>) {
  return {};
}

template <typename... Ts>
constexpr auto canonicalize_impl(logicalandexpr<Ts...>) {
  return make_and(simplify_adjacent_logical_and(sort(extract_from_constant_subexpr(Ts{})...)));
}

template <typename C>
constexpr TypeList<> and_conditions_impl(baseexpr<C>, TypeList<>) {
  return {};
}

template <typename C, typename T, typename... Ts>
constexpr auto and_conditions_impl(baseexpr<C>, TypeList<T, Ts...>) {
  return List(C{} && T{}).Concat(and_conditions_impl(C{} && !T{}, TypeList<Ts...>{}));
}

template <typename... Ts>
constexpr auto and_conditions(TypeList<Ts...> conditions) {
  return and_conditions_impl(trueexpr{}, conditions);
}

} // end namespace aether

#endif
