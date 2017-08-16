#ifndef INVERT_H
#define INVERT_H

#include "Add.h"
#include "typelist/Any.h"
#include "typelist/All.h"
#include "typelist/Append.h"
#include "typelist/Prepend.h"
#include "typelist/Chain.h"
#include "typelist/Count.h"
#include "typelist/Back.h"
#include "typelist/Find.h"
#include "Expr.h"
#include "Condition.h"
#include "Equation.h"
#include "typelist/Filter.h"
#include "typelist/GroupByKey.h"
#include "typelist/Max.h"
#include "Vector.h"
#include "typelist/Partition.h"
#include "typelist/Unique.h"
#include "Literal.h"
#include "typelist/Pick.h"
#include "typelist/RemoveBack.h"
#include "typelist/Intersection.h"
#include "Product.h"
#include "Replace.h"
#include "Trig.h"
#include "typelist/TypeList.h"
#include "typelist/TypeMap.h"
#include "typelist/Zip.h"
#include "Variable.h"

namespace aether {

template <typename T>
constexpr auto dependents(varsubexpr<T> vse) {
  return make_type_set(vse);
}

template <typename T>
constexpr auto dependents(constantsubexpr<T>) {
  return make_type_set();
}

template <typename Lhs, typename Rhs>
constexpr auto dependents(equation<Lhs, Rhs>) {
  return dependents(Lhs{});
}

template <typename T>
constexpr auto dependents(coordinate_system<T>) {
  return make_type_set();
}

template <std::size_t I>
constexpr auto dependents(_size_t<I>) {
  return make_type_set();
}

template <int ID, int SeqIndex>
constexpr auto dependents(variable<ID, SeqIndex> v) {
  return make_type_set(v);
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr auto dependents(baseexpr<T>) {
  return make_type_set();
}

template <typename... Ts>
constexpr auto dependents(TypeList<Ts...>) {
  return MergeAll(dependents(Ts{})...);
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto dependents(baseexpr<T>) {
  return dependents(operands(T{}));
}

template <typename... Ts>
constexpr auto dependents_all(TypeList<Ts...>) {
  return List(dependents(Ts{})...);
}

struct preprocess_t {
  template <typename Lhs, typename Rhs>
  constexpr auto operator()(equation<Lhs, Rhs> eq) const {
    return eq;
  }

  template <typename T, typename Rhs>
  constexpr auto operator()(equation<productexpr<varsubexpr<powerexpr<varsubexpr<vectorlength<T>>, minus_one_t>>, T>, Rhs>) const {
    return make_equation(T{}, normalize(Rhs{}));
  }
};

constexpr preprocess_t preprocess{};

struct is_var_constant_vector_product_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false; 
  }

  template <typename... Ts>
  constexpr bool is_product_of_vars(TypeList<Ts...>) const {
    return ((is_scalarexpr(Ts{}) && !is_constant(Ts{})) && ... && true);
  }

  template <typename... Ts>
  constexpr bool operator()(productexpr<Ts...>) const {
    constexpr auto operands = List(Ts{}...);
    return is_product_of_vars(remove_back(operands))
      && is_vector(back(operands))
      && is_constant(back(operands));
  }
};

constexpr is_var_constant_vector_product_t is_var_constant_vector_product{};

struct get_var_in_var_constant_vector_product_t {
  template <typename... Ts>
  constexpr auto operator()(productexpr<Ts...>) const {
    constexpr auto operands = List(Ts{}...);
    return product_list(remove_back(operands));
  }
};

constexpr get_var_in_var_constant_vector_product_t get_var_in_var_constant_vector_product{};

struct get_constant_in_var_constant_vector_product_t {
  template <typename... Ts>
  constexpr auto operator()(productexpr<Ts...>) const {
    constexpr auto operands = List(Ts{}...);
    return back(operands);
  }
};

constexpr get_constant_in_var_constant_vector_product_t get_constant_in_var_constant_vector_product{};

struct is_basic_barycentric_weight_pair_t {
  template <typename... Ts>
  constexpr bool operator()(TypeList<Ts...>) const {
    return false;
  }

  template <typename A, typename B, EnableIf<is_variable(A{}) && is_constant(B{}) && is_vector(B{})> = 0>
  constexpr bool operator()(TypeList<A, B>) const {
    return true;
  }
};

constexpr is_basic_barycentric_weight_pair_t is_basic_barycentric_weight_pair{};

template <typename... Ts>
constexpr auto get_barycentric_pairs(addexpr<Ts...>) {
  constexpr auto pairs = zip(
    List(get_var_in_var_constant_vector_product(Ts{})...)
    , List(get_constant_in_var_constant_vector_product(Ts{})...)
  );

  constexpr auto parts = partition(is_basic_barycentric_weight_pair, pairs);
  static_assert(at<1>(parts).Size() == 1);
  return at<0>(parts).Concat(at<1>(parts));
}

template <typename... Ts>
constexpr auto get_barycentric_weights(addexpr<Ts...>) {
  return List(get_var_in_var_constant_vector_product(Ts{})...);
}

template <typename... Ts>
constexpr auto get_barycentric_constants(addexpr<Ts...>) {
  return List(get_constant_in_var_constant_vector_product(Ts{})...);
}

template <typename A, typename B, typename C, EnableIf<!is_var_constant_vector_product(A{}) || !is_var_constant_vector_product(B{}) || !is_var_constant_vector_product(C{})> = 0>
constexpr bool is_barycentric(addexpr<A, B, C>) {
  return false;
}

template <typename A, typename B, typename C, EnableIf<is_var_constant_vector_product(A{}) && is_var_constant_vector_product(B{}) && is_var_constant_vector_product(C{})> = 0>
constexpr bool is_barycentric(addexpr<A, B, C> a) {
  constexpr auto weights = get_barycentric_weights(a);
  return are_equal(add_list(weights), one);
}

template <typename T>
constexpr bool is_barycentric(baseexpr<T>) {
  return false;
}

template <typename Lhs, typename Rhs>
constexpr bool is_barycentric(equation<Lhs, Rhs>) {
  return is_barycentric(Lhs{});
}



template <typename Var>
struct has_fraction_degree {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }

  template <typename T, std::intmax_t N, std::intmax_t D, EnableIf<depends_on(T{}, Var{})> = 0>
  constexpr bool operator()(powerexpr<T, literal<N, D>>) const {
    return true;
  }

  template <typename T, std::intmax_t N, EnableIf<depends_on(T{}, Var{})> = 0>
  constexpr bool operator()(powerexpr<T, literal<N, 1>>) const {
    return false;
  }

  constexpr bool impl(TypeList<>) const {
    return false;
  }

  template <typename... Ts>
  constexpr bool impl(TypeList<Ts...>) const {
    bool result[] = {this->operator()(Ts{})...};
    for (bool r : result) {
      if (r) {
        return true;
      }
    }
    return false;
  }

  template <typename... Ts>
  constexpr bool operator()(productexpr<Ts...>) const {
    return impl(TypeList<Ts...>{});
  }
};


// Contains exactly one occurrence of the given variable:
// e.g. y = 2 + 10 * x
// has exactly one occurrence of x
// Invert this by simply reversing every operation
struct has_single_occurrence_t {
  template <typename Lhs, typename Rhs, typename Var>
  constexpr bool operator()(equation<Lhs, Rhs>, Var) const {
    constexpr auto num_lhs = num_occurrences(Lhs{}, Var{});
    constexpr auto num_rhs = num_occurrences(Rhs{}, Var{});
    return (num_lhs == one && num_rhs == zero) || (num_lhs == zero && num_rhs == one);
  }

  template <typename Lhs, typename Rhs, EnableIf<vars(equation<Lhs, Rhs>{}).Size() != 1> = 0>
  constexpr bool operator()(equation<Lhs, Rhs>) const {
    return false;
  }

  template <typename Lhs, typename Rhs, EnableIf<vars(equation<Lhs, Rhs>{}).Size() == 1> = 0>
  constexpr bool operator()(equation<Lhs, Rhs>) const {
    return this->operator()(equation<Lhs, Rhs>{}, at<0>(vars(equation<Lhs, Rhs>{})));
  }
};

constexpr has_single_occurrence_t has_single_occurrence{};

struct invert_dependent_t {
  template <typename... Ts, typename Var, EnableIf<any_depend_on(TypeList<Ts...>{}, Var{}) && !all_depend_on(TypeList<Ts...>{}, Var{})> = 0>
  constexpr auto operator()(equation<addexpr<Ts...>, zero_t>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
    return make_equation(product_list(at<0>(parts)), zero);
  }

  template <typename... Ts, typename Rhs, typename Var, EnableIf<any_depend_on(TypeList<Ts...>{}, Var{}) && !all_depend_on(TypeList<Ts...>{}, Var{})> = 0>
  constexpr auto operator()(equation<productexpr<Ts...>, Rhs>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
    return make_equation(product_list(at<0>(parts)), Rhs{} / product_list(at<1>(parts)));
  }

  template <typename... Ts, typename Rhs, typename Var, EnableIf<any_depend_on(TypeList<Ts...>{}, Var{})> = 0>
  constexpr auto operator()(equation<addexpr<Ts...>, Rhs>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
    return make_equation(add_list(at<0>(parts).Concat(zero)), Rhs{} - add_list(at<1>(parts).Concat(zero)));
  }

  template <typename B, typename E, typename Rhs, typename Var, EnableIf<depends_on(B{}, Var{})> = 0>
  constexpr auto operator()(equation<powerexpr<B, E>, Rhs>, Var) const {
    return make_equation(B{}, pow(Rhs{}, rcp(E{})));
  }

  template <typename T, typename Rhs, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
  constexpr auto operator()(equation<logexpr<T>, Rhs>, Var) const {
    return make_equation(T{}, exp(Rhs{}));
  }

  template <typename T, typename Rhs, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
  constexpr auto operator()(equation<expexpr<T>, Rhs>, Var) const {
    return make_equation(T{}, log(Rhs{}));
  }

  template <typename T, typename Rhs, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
  constexpr auto operator()(equation<sinexpr<T>, Rhs>, Var) const {
    return make_equation(T{}, make_arcsin(Rhs{}));
  }

  template <typename T, typename Rhs, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
  constexpr auto operator()(equation<cosexpr<T>, Rhs>, Var) const {
    return make_equation(T{}, make_arccos(Rhs{}));
  }

  template <typename T, typename Rhs, typename Var, EnableIf<depends_on(T{}, Var{}) && !depends_on(Rhs{}, Var{})> = 0>
  constexpr auto operator()(equation<tanexpr<T>, Rhs>, Var) const {
    return make_equation(T{}, make_arctan(Rhs{}));
  }

  template <typename Var, typename Rhs>
  constexpr auto operator()(equation<Var, Rhs>, Var var) const {
    return make_equation(var, Rhs{});
  }
};

constexpr invert_dependent_t invert_dependent{};

struct solve_for_single_occurrence_t {
  template <typename Var, typename Rhs>
  constexpr auto impl(equation<Var, Rhs>, Var) const {
    return equation<Var, Rhs>{};
  }

  template <typename Lhs, typename Rhs, typename Var>
  constexpr auto impl(equation<Lhs, Rhs>, Var) const {
    return impl(invert_dependent(equation<Lhs, Rhs>{}, Var{}), Var{});
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<!has_single_occurrence(equation<Lhs, Rhs>{}, Var{})> = 0>
  constexpr equation<Lhs, Rhs> operator()(equation<Lhs, Rhs>, Var) const {
    return {};
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<has_single_occurrence(equation<Lhs, Rhs>{}, Var{}) && num_occurrences(Rhs{}, Var{}) == one> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return impl(equation<Rhs, Lhs>{}, Var{});
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<has_single_occurrence(equation<Lhs, Rhs>{}, Var{}) && num_occurrences(Lhs{}, Var{}) == one> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return impl(equation<Lhs, Rhs>{}, Var{});
  }

  template <typename Lhs, typename Rhs>
  constexpr auto operator()(equation<Lhs, Rhs>) const {
    static_assert(vars(equation<Lhs, Rhs>{}).Size() == 1, "Can only call solve_for_single_occurrence(equation) if equation has a single variable.");
    return impl(equation<Lhs, Rhs>{}, at<0>(vars(equation<Lhs, Rhs>{})));
  }
};

constexpr solve_for_single_occurrence_t solve_for_single_occurrence{};


struct expand_t {
  template <typename... Ts, typename Var, EnableIf<depends_on(productexpr<Ts...>{}, Var{})> = 0>
  constexpr auto impl(powerexpr<productexpr<Ts...>, two_t>, Var v) const {
    return impl((sq(Ts{}) * ... * one), v);
  }

  template <typename A, typename... Ts, typename Var, EnableIf<depends_on(A{}, Var{}) && depends_on(addexpr<Ts...>{}, Var{})> = 0>
  constexpr auto impl(productexpr<A, addexpr<Ts...>>, Var v) const {
    return impl(add_list(List(A{} * Ts{}...)), v);
  }

  template <typename T, typename Var>
  constexpr T impl(coordinate_system<T>, Var) const {
    return {};
  }
  
  template <std::size_t I, typename Var>
  constexpr _size_t<I> impl(_size_t<I>, Var) const {
    return {};
  }

  template <typename T, typename Var, EnableIf<is_leaf(T{})> = 0>
  constexpr T impl(baseexpr<T>, Var) const {
    return {};
  }

  template <typename T, typename Var, EnableIf<!is_leaf(T{})> = 0>
  constexpr auto impl(baseexpr<T>, Var v) const {
    return make_expr(tag_of(T{}), impl(operands(T{}), v));
  }

  template <typename T, typename Var>
  constexpr constantsubexpr<T> impl(constantsubexpr<T>, Var) const {
    return {};
  }

  template <typename... Ts, typename Var>
  constexpr auto impl(TypeList<Ts...>, Var v) const {
    return List(impl(Ts{}, v)...);
  }

  template <typename... Ts, typename Var, EnableIf<depends_on(addexpr<Ts...>{}, Var{})> = 0>
  constexpr auto impl(addexpr<Ts...>, Var) const {
    return add_list(List(impl(Ts{}, Var{})...));
  }

  template <typename T, typename Var, EnableIf<depends_on(sinexpr<T>{}, Var{})> = 0>
  constexpr auto impl(sinexpr<T>, Var) const {
    return sin(impl(T{}, Var{}));
  }

  template <typename T, typename Var, EnableIf<depends_on(cosexpr<T>{}, Var{})> = 0>
  constexpr auto impl(cosexpr<T>, Var) const {
    return cos(impl(T{}, Var{}));
  }

  template <typename T, typename Var, EnableIf<depends_on(tanexpr<T>{}, Var{})> = 0>
  constexpr auto impl(tanexpr<T>, Var) const {
    return tan(impl(T{}, Var{}));
  }

  template <typename... Ts, typename Var, EnableIf<depends_on(addexpr<Ts...>{}, Var{})> = 0>
  constexpr auto impl(powerexpr<addexpr<Ts...>, two_t>, Var) const {
    return add_list(List(impl(Ts{} * add_list(List(Ts{}...)), Var{})...));
  }

  template <typename Lhs, typename Rhs, typename Var>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return make_equation(impl(Lhs{}, Var{}), impl(Rhs{}, Var{}));
  }
};

constexpr expand_t expand{};





template <typename T, typename Denom, typename Var, EnableIf<!depends_on(denominator(T{}), Var{})> = 0>
constexpr auto reduce_fraction(expr<T>, expr<Denom>, Var) {
  return T{} * Denom{};
}

template <typename T, typename Denom, typename Var, EnableIf<depends_on(denominator(T{}), Var{})> = 0>
constexpr auto reduce_fraction(expr<T>, expr<Denom>, Var) {
  return T{} * Denom{};
}

template <typename T, typename... Ds, typename Var, EnableIf<!contains(denominator(T{}), TypeList<Ds...>{})> = 0>
constexpr auto reduce_fraction(T, TypeList<Ds...>, Var) {
  return T{} * product_list(TypeList<Ds...>{});
}

template <typename T, typename... Ds, typename Var, EnableIf<contains(denominator(T{}), TypeList<Ds...>{})> = 0>
constexpr auto reduce_fraction(T, TypeList<Ds...>, Var) {
  return numerator(T{}) * product_list(remove_first(denominator(T{}), TypeList<Ds...>{}).Concat(one));
}

template <typename R, typename EQ, typename Var, typename E = void_t<>>
struct can_apply_rule_t : std::false_type {
};

template <typename R, typename A, typename B, typename Var>
struct can_apply_rule_t<R, equation<A, B>, Var, void_t<decltype(R{}(equation<A, B>{}, Var{}))>> : std::true_type {
};

template <typename R, typename Eq, typename E = void_t<>>
struct can_apply_rule_no_var_t : std::false_type {
};

template <typename R, typename... Eqs>
struct can_apply_rule_no_var_t<R, TypeList<Eqs...>, void_t<decltype(R{}(TypeList<Eqs...>{}))>> : std::true_type {
};

template <typename R, typename A, typename B, typename Var>
constexpr bool can_apply_rule(R, equation<A, B>, Var) {
  return can_apply_rule_t<R, equation<A, B>, Var>::value;
}

template <typename R, typename... Eqs>
constexpr bool can_apply_rule(R, TypeList<Eqs...>) {
  return can_apply_rule_no_var_t<R, TypeList<Eqs...>>::value;
}

struct multiply_both_sides_by_denominator_t {
  template <typename... Ts, typename Rhs, typename Var, EnableIf<all_depend_on(TypeList<Ts...>{}, Var{}) && depends_on(denominator(productexpr<Ts...>{}), Var{})> = 0>
  constexpr auto operator()(equation<productexpr<Ts...>, Rhs>, Var) const {
    return make_equation(numerator(productexpr<Ts...>{}), denominator(productexpr<Ts...>{}) * Rhs{});
  }

  template <typename Lhs, typename... Ts, typename Var, EnableIf<all_depend_on(TypeList<Ts...>{}, Var{}) && depends_on(denominator(productexpr<Ts...>{}), Var{})> = 0>
  constexpr auto operator()(equation<Lhs, productexpr<Ts...>>, Var) const {
    return make_equation(numerator(productexpr<Ts...>{}), denominator(productexpr<Ts...>{}) * Lhs{});
  }

  template <typename... Ts, typename Rhs, typename Var, EnableIf<any_depend_on(List(denominator(Ts{})...), Var{})> = 0>
  constexpr auto operator()(equation<addexpr<Ts...>, Rhs>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(denominator(Ts{})...));
    constexpr auto denom_list = unique(at<0>(parts));
    return make_equation(add_list(List(reduce_fraction(Ts{}, denom_list, Var{})...).Concat(zero)), Rhs{} * product_list(denom_list.Concat(one)));
  }

  template <typename Lhs, typename... Ts, typename Var, EnableIf<any_depend_on(List(denominator(Ts{})...), Var{})> = 0>
  constexpr auto operator()(equation<Lhs, addexpr<Ts...>>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(denominator(Ts{})...));
    constexpr auto denom_list = unique(at<0>(parts));
    return make_equation(add_list(List(reduce_fraction(Ts{}, denom_list, Var{})...).Concat(zero)), Lhs{} * product_list(denom_list.Concat(one)));
  }
};

constexpr multiply_both_sides_by_denominator_t multiply_both_sides_by_denominator{};

struct move_whole_powers_to_lhs_t {
  template <typename Lhs, typename Rhs, typename Var, EnableIf<(!is_constant(Rhs{}) && has_fraction_degree<Var>{}(Rhs{}))> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return make_equation(Lhs{}, Rhs{});
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<(is_constant(Rhs{}) || !has_fraction_degree<Var>{}(Rhs{}))> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return make_equation(Lhs{} - Rhs{}, zero);
  }

  template <typename Lhs, typename... Ts, typename Var>
  constexpr auto operator()(equation<Lhs, addexpr<Ts...>>, Var) const {
    constexpr auto parts = partition(has_fraction_degree<Var>{}, TypeList<Ts...>{});
    constexpr auto constants = partition(is_constant, at<1>(parts));
    return make_equation(Lhs{} - add_list(at<1>(constants).Concat(zero)), add_list(at<0>(parts).Concat(at<0>(constants)).Concat(zero)));
  }
};

constexpr move_whole_powers_to_lhs_t move_whole_powers_to_lhs{};




struct move_fraction_powers_to_rhs_t {
  template <typename Lhs, typename Rhs, typename Var, EnableIf<has_fraction_degree<Var>{}(Lhs{}) && depends_on(Rhs{}, Var{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return make_equation(zero, Rhs{} - Lhs{});
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<!has_fraction_degree<Var>{}(Lhs{}) || !depends_on(Rhs{}, Var{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return make_equation(Lhs{}, Rhs{});
  }

  template <typename... Ts, typename Rhs, typename Var>
  constexpr auto operator()(equation<addexpr<Ts...>, Rhs>, Var) const {
    constexpr auto parts = partition(has_fraction_degree<Var>{}, TypeList<Ts...>{});
    return make_equation(add_list(at<1>(parts).Concat(zero)), Rhs{} - add_list(at<0>(parts).Concat(zero)));
  }
};

constexpr move_fraction_powers_to_rhs_t move_fraction_powers_to_rhs{};

struct has_var_under_square_root_t {
  template <typename T, typename Var, EnableIf<is_leaf(T{})> = 0>
  constexpr bool impl(baseexpr<T>, Var) const {
    return false;
  }

  template <typename... Ts, typename Var>
  constexpr bool impl(TypeList<Ts...>, Var v) const {
    return (impl(Ts{}, v) || ... || false); 
  }

  template <typename T, typename Var>
  constexpr bool impl(constantsubexpr<T>, Var) const {
    return false;
  }

  template <std::size_t I, typename Var>
  constexpr bool impl(_size_t<I>, Var) const {
    return false;
  }

  template <typename T, typename Var>
  constexpr bool impl(coordinate_system<T>, Var) const {
    return false;
  }

  template <typename B, typename Var>
  constexpr bool impl(powerexpr<B, half_t>, Var v) const {
    return depends_on(B{}, v);
  }

  template <typename T, typename Var, EnableIf<!is_leaf(T{})> = 0>
  constexpr bool impl(baseexpr<T>, Var v) const {
    return impl(operands(T{}), v);
  }

  template <typename Lhs, typename Rhs, typename Var>
  constexpr bool operator()(equation<Lhs, Rhs>, Var v) const {
    return impl(Lhs{}, v) || impl(Rhs{}, v);
  }
};

constexpr has_var_under_square_root_t has_var_under_square_root{};


struct square_both_sides_t {
  template <typename Lhs, typename Rhs, typename Var, EnableIf<has_var_under_square_root(equation<Lhs, Rhs>{}, Var{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return make_equation(sq(Lhs{}), sq(Rhs{}));
  }
};

constexpr square_both_sides_t square_both_sides{};

struct move_constants_to_lhs_t {
  template <typename Lhs, typename... Ts, typename Var>
  constexpr auto operator()(equation<Lhs, addexpr<Ts...>>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
    return make_equation(Lhs{} - add_list(at<1>(parts).Concat(zero)), add_list(at<0>(parts).Concat(zero)));
  }

  template <typename Lhs, typename... Ts>
  constexpr auto operator()(equation<Lhs, addexpr<Ts...>>) const {
    constexpr auto parts = partition(is_constant, List(Ts{}...));
    return make_equation(Lhs{} - add_list(at<0>(parts).Concat(zero)), add_list(at<1>(parts).Concat(zero)));
  }
};

constexpr move_constants_to_lhs_t move_constants_to_lhs{};

struct move_constants_to_rhs_t {
  template <typename... Ts, typename Rhs, typename Var>
  constexpr auto operator()(equation<addexpr<Ts...>, Rhs>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
    return make_equation(add_list(at<0>(parts).Concat(zero)), Rhs{} - add_list(at<1>(parts).Concat(zero)));
  }

  template <typename... Ts, typename Rhs>
  constexpr auto operator()(equation<addexpr<Ts...>, Rhs>) const {
    constexpr auto parts = partition(is_constant, List(Ts{}...));
    return make_equation(add_list(at<1>(parts).Concat(zero)), Rhs{} - add_list(at<0>(parts).Concat(zero)));
  }

  template <typename Lhs, typename Rhs>
  constexpr auto operator()(equation<Lhs, Rhs>) const {
    return make_equation(Lhs{}, Rhs{});
  }
};

constexpr move_constants_to_rhs_t move_constants_to_rhs{};

template <typename Rule>
struct apply_rule_t {
  template <typename Lhs, typename Rhs, typename Var, EnableIf<!can_apply_rule(Rule{}, equation<Lhs, Rhs>{}, Var{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return equation<Lhs, Rhs>{};
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<can_apply_rule(Rule{}, equation<Lhs, Rhs>{}, Var{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return Rule{}(equation<Lhs, Rhs>{}, Var{});
  }

  template <typename... Eqs, EnableIf<!can_apply_rule(Rule{}, TypeList<Eqs...>{})> = 0>
  constexpr auto operator()(equation<Eqs...>) const {
    return TypeList<Eqs...>{};
  }

  template <typename... Eqs, EnableIf<can_apply_rule(Rule{}, TypeList<Eqs...>{})> = 0>
  constexpr auto operator()(equation<Eqs...>) const {
    return Rule{}(TypeList<Eqs...>{});
  }
};

template <typename... Ts, typename Var>
constexpr bool var_subexprs_are_equal(TypeList<Ts...>, Var) {
  return all_same(var_subexpr(Ts{}, Var{})...);
}

struct factor_var_on_rhs_t {
  template <typename Lhs, typename... Ts, typename Var, EnableIf<!depends_on(Lhs{}, Var{}) && !all_depend_on(List(Ts{}...), Var{})> = 0>
  constexpr auto operator()(equation<Lhs, productexpr<Ts...>>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
    return make_equation(Lhs{} / product_list(at<1>(parts)), product_list(at<0>(parts)));
  }

  template <typename Lhs, typename... Ts, typename Var>
  constexpr auto impl(equation<Lhs, productexpr<Ts...>>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
    return make_equation(Lhs{} / product_list(at<1>(parts)), product_list(at<0>(parts)));
  }

  template <typename Lhs, typename Rhs, typename Var>
  constexpr auto impl(equation<Lhs, Rhs>, Var) const {
    return make_equation(Lhs{}, Rhs{});
  }

  template <typename Lhs, typename... Ts, typename Var>
  constexpr auto impl(equation<Lhs, addexpr<Ts...>>, Var) const {
    constexpr auto var_subexprs = at<0>(List(at<0>(partition_dependent_subexprs(Ts{}, Var{}))...));
    constexpr auto coefficients = add_list(List(at<1>(partition_dependent_subexprs(Ts{}, Var{}))...));  
    return make_equation(Lhs{} / coefficients, var_subexprs);
  }

  template <typename Lhs, typename... Ts, typename Var, typename P = decltype(partition(has_dependent_variable<Var>{}, List(Ts{}...))), EnableIf<var_subexprs_are_equal(at<0>(P{}), Var{})> = 0>
  constexpr auto operator()(equation<Lhs, addexpr<Ts...>>, Var) const {
    return impl(apply_rule_t<move_constants_to_lhs_t>{}(equation<Lhs, addexpr<Ts...>>{}, Var{}), Var{});
  }
};

constexpr factor_var_on_rhs_t factor_var_on_rhs{};

struct factor_var_on_lhs_t {
  template <typename... Ts, typename Rhs, typename Var, EnableIf<!depends_on(Rhs{}, Var{}) && !all_depend_on(List(Ts{}...), Var{})> = 0>
  constexpr auto operator()(equation<productexpr<Ts...>, Rhs>, Var) const {
    constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
    return make_equation(product_list(at<0>(parts)), Rhs{} / product_list(at<1>(parts)));
  }

  template <typename... Ts, typename Rhs, typename Var, EnableIf<all_same(var_subexpr(Ts{}, Var{})...)> = 0>
  constexpr auto operator()(equation<addexpr<Ts...>, Rhs>, Var) const {
    constexpr auto var_subexprs = at<0>(List(at<0>(partition_dependent_subexprs(Ts{}, Var{}))...));
    constexpr auto coefficients = add_list(List(at<1>(partition_dependent_subexprs(Ts{}, Var{}))...).Concat(zero));
    return make_equation(var_subexprs, Rhs{} / coefficients);
  }
};

constexpr factor_var_on_lhs_t factor_var_on_lhs{};


template <typename... Ts, typename Var>
constexpr auto coefficient(productexpr<Ts...>, Var) {
  constexpr auto parts = partition(has_dependent_variable<Var>{}, List(Ts{}...));
  return List(product(at<0>(parts)), product(at<1>(parts)));
}


struct reduce_exponent_t {
  template <typename Rhs, typename Var>
  constexpr auto operator()(equation<powerexpr<Var, two_t>, Rhs>, Var) const {
    return make_equation(Var{}, sqrt(Rhs{}));
  }
};

constexpr reduce_exponent_t reduce_exponent{};





template <typename... Ds>
constexpr bool is_quadratic_impl(TypeList<Ds...>) {
  return false;
}

constexpr bool is_quadratic_impl(TypeList<zero_t, one_t, two_t>) {
  return true;
}

template <typename... Ts, typename Var>
constexpr bool is_quadratic(addexpr<Ts...>, Var) {
  return is_quadratic_impl(List(degree(Ts{}, Var{})...));
}

template <typename T, typename Var>
constexpr bool is_quadratic(expr<T>, Var) {
  return false;
}

template <typename Lhs, typename Rhs, typename Var>
constexpr bool is_quadratic(equation<Lhs, Rhs>, Var) {
  return is_quadratic(Lhs{} - Rhs{}, Var{});
}

template <typename... Ts, typename Var>
constexpr auto coefficients(TypeList<Ts...>, Var) {
  return List(at<1>(partition_dependent_subexprs(Ts{}, Var{}))...);  
}

struct quadratic_formula_t {
  template <typename... Ts, typename Var>
  constexpr auto impl(addexpr<Ts...>, Var) const {
    constexpr auto coeffs = coefficients(TypeList<Ts...>{}, Var{});
    constexpr auto a = at<2>(coeffs);
    constexpr auto b = at<1>(coeffs);
    constexpr auto c = at<0>(coeffs);
    return make_equation(Var{}, (-b + sqrt(b * b - literal<4, 1>{} * a * c)) / (two * a));
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<is_quadratic(equation<Lhs, Rhs>{}, Var{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return impl(Lhs{} - Rhs{}, Var{});
  }
};

constexpr quadratic_formula_t quadratic_formula{};

struct is_var_isolated_t {
  template <typename Lhs, typename Rhs, typename Var>
  constexpr bool operator()(equation<Lhs, Rhs>, Var) const {
    return num_occurrences(Lhs{}, Var{}) == one && num_occurrences(Rhs{}, Var{}) == zero;
  }
};

constexpr is_var_isolated_t is_var_isolated{};

struct invert_isolated_t {
  template <typename Lhs, typename Rhs, typename Var>
  constexpr auto impl(equation<Lhs, Rhs>, Var) const {
    return impl(invert_dependent(equation<Lhs, Rhs>{}, Var{}), Var{});
  }

  template <typename Rhs, typename Var>
  constexpr auto impl(equation<Var, Rhs>, Var) const {
    return equation<Var, Rhs>{};
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<is_var_isolated(equation<Rhs, Lhs>{}, Var{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return impl(equation<Rhs, Lhs>{}, Var{});
  }

  template <typename Lhs, typename Rhs, typename Var, EnableIf<is_var_isolated(equation<Lhs, Rhs>{}, Var{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs>, Var) const {
    return impl(equation<Lhs, Rhs>{}, Var{});
  }
};


constexpr invert_isolated_t invert_isolated{};

struct reconcile_var_degrees_t {
  template <typename... Rs, typename Lhs, typename Rhs, typename Var>
  constexpr auto impl(TypeList<Rs...>, equation<Lhs, Rhs> eq, Var v) const {
    return chain(List(apply_rule_t<Rs>{}...), eq, v);
  }

  template <typename Lhs, typename Rhs, typename Var>
  constexpr auto operator()(equation<Lhs, Rhs> eq, Var v) const {
    constexpr auto rules = List(
      multiply_both_sides_by_denominator
      , move_constants_to_lhs
      , move_whole_powers_to_lhs
      , move_fraction_powers_to_rhs
      , factor_var_on_rhs
      , square_both_sides
      , move_whole_powers_to_lhs
      , expand
      , move_constants_to_rhs
      , factor_var_on_lhs
    );

    return impl(rules, eq, v);
  }
};

constexpr reconcile_var_degrees_t reconcile_var_degrees{};

struct apply_rules_t {
  template <typename... Rs, typename Lhs, typename Rhs, typename Var>
  constexpr auto single_iteration(TypeList<Rs...>, equation<Lhs, Rhs>, Var) const {
    return chain(List(apply_rule_t<Rs>{}...), equation<Lhs, Rhs>{}, Var{});
  }

  template <typename... Rs, typename Lhs, typename Rhs, typename Var>
  constexpr auto impl(TypeList<Rs...> rules, equation<Lhs, Rhs> eq, Var v) const {
    return impl_recursive(eq, single_iteration(rules, eq, v), v);
  }

  template <typename CurrentEq, typename NextEq, typename Var>
  constexpr auto impl_recursive(CurrentEq, NextEq next, Var v) const {
    return this->operator()(next, v);
  }

  template <typename Rhs, typename Var>
  constexpr auto impl_recursive(equation<Rhs, Var>, equation<Rhs, Var>, Var) const {
    return equation<Var, Rhs>{};
  }

  template <typename Rhs, typename Var>
  constexpr auto impl_recursive(equation<Var, Rhs>, equation<Var, Rhs>, Var) const {
    return equation<Var, Rhs>{};
  }

  template <typename Lhs, typename Rhs, typename Var>
  constexpr auto operator()(equation<Lhs, Rhs> eq, Var v) const {
    constexpr auto rules = List(
      quadratic_formula
      , reconcile_var_degrees
      , solve_for_single_occurrence
    );

    return impl(rules, eq, v);
  }
};

constexpr apply_rules_t apply_rules{};


template <typename T, int ID, int SeqIndex>
constexpr auto invert(expr<T>, variable<ID, SeqIndex> v) {
  return invert_impl(T{}, inverse_variable<ID>{}, v);  
}


template <typename... Ts>
constexpr auto make_product_all(TypeList<Ts...>) {
  return List(make_product(Ts{})...);
}

struct is_inverted_t {
  constexpr bool operator()() const {
    return false;
  }
};

constexpr is_inverted_t is_inverted{};

template <typename... Ps>
struct or_t {
  template <typename Arg, typename T>
  constexpr bool impl(Arg, TypeList<T>) const {
    return T{}(Arg{}); 
  }

  template <typename Arg, typename A, typename B, typename... Ts, EnableIf<!A{}(Arg{})> = 0>
  constexpr bool impl(Arg, TypeList<A, B, Ts...>) const {
    return impl(Arg{}, List(B{}, Ts{}...)); 
  }

  template <typename Arg, typename A, typename B, typename... Ts, EnableIf<A{}(Arg{})> = 0>
  constexpr bool impl(Arg, TypeList<A, B, Ts...>) const {
    return true; 
  }

  template <typename Arg>
  constexpr bool operator()(Arg) const {
    return impl(Arg{}, List(Ps{}...));
  }
};

template <typename... Ps>
constexpr auto or_(Ps...) {
  return or_t<Ps...>{};
}


template <typename Lhs, typename Rhs, typename Var>
constexpr auto solve_for(equation<Lhs, Rhs> eq, Var v) {
  constexpr auto result = apply_rules(eq, v);
  return result;
}

// VA * {a0, a1, a2} + VB * {b0, b1, b2} + VC * {c0, c1, c2} = {D0, D1, D2}
template <typename VA, typename A, typename VB, typename B, typename VC, typename C, typename D>
constexpr auto cramers_rule_3x3(equation<addexpr<productexpr<VA, constantsubexpr<A>>, productexpr<VB, constantsubexpr<B>>, productexpr<VC, constantsubexpr<C>>>, D>) {
  constexpr auto a = A{};
  constexpr auto b = B{};
  constexpr auto c = C{};
  constexpr auto d = D{};

  constexpr auto dbc = determinant(make_matrix_expr(d, b, c));
  constexpr auto adc = determinant(make_matrix_expr(a, d, c));
  constexpr auto abd = determinant(make_matrix_expr(a, b, d));
  constexpr auto rabc = rcp(determinant(make_matrix_expr(a, b, c)));

  return make_vector_equation(List(VA{}, VB{}, VC{}), List(dbc * rabc, adc * rabc, abd * rabc));
}

// VA * {a0, a1, a2} + VB * {b0, b1, b2} + {c0, c1, c2} = {D0, D1, D2}
template <typename C, typename VA, typename A, typename VB, typename B, typename D>
constexpr auto cramers_rule_3x3(equation<addexpr<constantsubexpr<C>, productexpr<VA, constantsubexpr<A>>, productexpr<VB, constantsubexpr<B>>>, D>) {
  constexpr auto inv = inverse(make_matrix_expr(A{}, B{}, C{}));

  constexpr auto w = inv * D{};
  return make_vector_equation(List(VA{}, VB{}), List(at<0>(w) / at<2>(w), at<1>(w) / at<2>(w)));
}

template <typename Lhs, typename Rhs>
constexpr auto cramers_rule_3x3(equation<Lhs, Rhs> eq) {
  return cramers_rule_3x3(move_constants_to_rhs(eq));
}

struct is_matrix_vector_product_t {
  template <typename A, typename B, EnableIf<is_matrix(A{}) && is_vector(B{})> = 0>
  constexpr bool impl(productexpr<A, B>) const {
    return true;
  }

  template <typename C, typename V>
  constexpr bool impl(branchcaseexpr<C, V>) const {
    return impl(V{});
  }

  template <typename T>
  constexpr bool impl(baseexpr<T>) const {
    return false;
  }

  template <typename Lhs, typename Rhs>
  constexpr bool operator()(equation<Lhs, Rhs> eq) const {
    return impl(eq.Lhs()); 
  }
};

constexpr is_matrix_vector_product_t is_matrix_vector_product{};

template <typename C, typename A, typename B, typename Rhs>
constexpr auto solve_matrix_vector_product(equation<branchcaseexpr<C, productexpr<A, B>>, Rhs>) {
  return make_equation(B{}, make_expr(branch_case_tag, C{}, inverse(A{}) * Rhs{}));
}

template <typename A, typename B, typename Rhs>
constexpr auto solve_matrix_vector_product(equation<productexpr<A, B>, Rhs>) {
  return make_equation(B{}, inverse(A{}) * Rhs{});
}

struct is_linear_system_t {
  template <typename T>
  constexpr bool impl(baseexpr<T>) const {
    return false;
  }

  template <typename T>
  constexpr bool is_variable_constant_vector_product(baseexpr<T>) const {
    return false;
  }

  template <typename B, EnableIf<is_vector(B{})> = 0>
  constexpr bool is_variable_constant_vector_product(constantsubexpr<B>) const {
    return true;
  }

  template <typename A, typename B, EnableIf<is_vector(B{})> = 0>
  constexpr bool is_variable_constant_vector_product(productexpr<varsubexpr<A>, constantsubexpr<B>>) const {
    return true;
  }

  template <int ID, int SeqIndex, typename T, EnableIf<is_vector(T{})> = 0>
  constexpr bool is_variable_constant_vector_product(productexpr<variable<ID, SeqIndex>, constantsubexpr<T>>) const {
    return true;
  }

  template <typename... Ts>
  constexpr bool impl(addexpr<Ts...>) const {
    return (is_variable_constant_vector_product(Ts{}) && ... && true); 
  }

  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false; 
  }

  template <typename... Ts>
  constexpr bool operator()(addexpr<Ts...> a) const {
    return impl(a);
  }

  template <typename Lhs, typename Rhs>
  constexpr bool operator()(equation<Lhs, Rhs> eq) const {
    return impl(move_constants_to_rhs(eq).Lhs()); 
  }
};

constexpr is_linear_system_t is_linear_system{};

template <typename T>
constexpr bool is_var_or_varsubexpr(baseexpr<T> e) {
  return is_variable(e) || is_varsubexpr(e);
}

template <typename T>
constexpr bool is_var_or_varsubexpr_or_constant(baseexpr<T> e) {
  return is_variable(e) || is_varsubexpr(e) || is_constant(e);
}

struct is_projection_vector_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false; 
  }

  template <typename... Ts>
  constexpr bool operator()(vectorexpr<cartesian_t, Ts...>) const {
    return count(is_constant, List(Ts{}...)) == 1 && (is_var_or_varsubexpr(Ts{}) || ... || false);
  }
};

constexpr is_projection_vector_t is_projection_vector{};

struct is_vector_of_vars_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false; 
  }

  template <typename... Ts>
  constexpr bool operator()(vectorexpr<cartesian_t, Ts...>) const {
    return (is_var_or_varsubexpr(Ts{}) && ... && true);
  }
};

constexpr is_vector_of_vars_t is_vector_of_vars{};

struct is_vector_of_vars_or_constants_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false; 
  }

  template <typename... Ts>
  constexpr bool operator()(vectorexpr<cartesian_t, Ts...>) const {
    constexpr bool at_least_one_var = (is_var_or_varsubexpr(Ts{}) || ... || false);
    return at_least_one_var && (is_var_or_varsubexpr_or_constant(Ts{}) && ... && true);
  }
};

constexpr is_vector_of_vars_or_constants_t is_vector_of_vars_or_constants{};

struct contains_vector_of_vars_t {
  template <typename T, EnableIf<is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return false; 
  }

  template <typename... Ts>
  constexpr bool operator()(TypeList<Ts...>) const {
    return (this->operator()(Ts{}) || ... || false);
  }

  template <typename T, EnableIf<!is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return this->operator()(operands(T{})); 
  }

  template <typename... Ts>
  constexpr bool operator()(vectorexpr<cartesian_t, Ts...>) const {
    return (is_var_or_varsubexpr(Ts{}) && ... && true);
  }
};

constexpr contains_vector_of_vars_t contains_vector_of_vars{};

struct contains_projection_vector_t {
  template <typename T, EnableIf<is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return false; 
  }

  template <typename... Ts>
  constexpr bool operator()(TypeList<Ts...>) const {
    return (this->operator()(Ts{}) || ... || false);
  }

  template <typename T, EnableIf<!is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return this->operator()(operands(T{})); 
  }

  template <typename... Ts>
  constexpr bool operator()(vectorexpr<cartesian_t, Ts...> v) const {
    return is_projection_vector(v);
  }
};

constexpr contains_projection_vector_t contains_projection_vector{};

struct contains_linear_system_t {
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
  constexpr bool operator()(TypeList<Ts...>) const {
    return (this->operator()(Ts{}) || ... || false);
  }

  template <typename T, EnableIf<!is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return this->operator()(operands(T{})); 
  }

  template <typename... Ts, EnableIf<(count(is_constant, List(Ts{}...)) == 0 && !is_constant(addexpr<Ts...>{}))> = 0>
  constexpr bool operator()(addexpr<Ts...> a) const {
    return is_linear_system(a);
  }

  template <typename... Ts, EnableIf<(count(is_constant, List(Ts{}...)) > 0) && !is_constant(addexpr<Ts...>{})> = 0>
  constexpr bool operator()(addexpr<Ts...>) const {
    return this->operator()(add_list(remove_if(is_constant, List(Ts{}...))));
  }
};

constexpr contains_linear_system_t contains_linear_system{};

struct is_scalar_varsubexpr_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false; 
  }

  template <typename T>
  constexpr bool operator()(varsubexpr<T>) const {
    return is_scalarexpr(T{}); 
  }
};

constexpr is_scalar_varsubexpr_t is_scalar_varsubexpr{};

struct is_scaled_vector_t {
  template <typename T>
  constexpr bool impl(baseexpr<T>) const {
    return false; 
  }

  template <typename... Ts>
  constexpr bool impl(productexpr<Ts...> p) const {
    return ((contains_vector_of_vars(p) || contains_linear_system(p)) || contains_projection_vector(p))
      && count(is_scalarexpr, List(Ts{}...)) >= 1;
  }

  template <typename... Ts>
  constexpr bool impl(addexpr<Ts...> a) const {
    return contains_linear_system(a) && !is_linear_system(a);
  }

  template <std::intmax_t N, std::intmax_t D, typename... Ts>
  constexpr bool impl(productexpr<literal<N, D>, Ts...>) const {
    constexpr auto p = productexpr<Ts...>{};
    return (contains_vector_of_vars(p) || contains_linear_system(p));
  }

  template <typename C, typename  V>
  constexpr bool impl(branchcaseexpr<C, V>) const {
    return impl(V{});
  }

  template <typename C, typename T, EnableIf<is_constant(C{})> = 0>
  constexpr bool impl(addexpr<C, T>) const {
    return impl(T{});
  }

  template <typename Lhs, typename Rhs>
  constexpr bool operator()(equation<Lhs, Rhs> eq) const {
    return impl(eq.Lhs()); 
  }
};

constexpr is_scaled_vector_t is_scaled_vector{};

template <typename... Ts, typename Rhs, EnableIf<contains_linear_system(productexpr<Ts...>{})> = 0>
constexpr auto solve_scaled_vector_impl(equation<productexpr<Ts...>, Rhs> eq) {
  constexpr auto lhs = filter(is_linear_system, List(Ts{}...));
  constexpr auto rhs = eq.Rhs();
  return make_equation(product_list(lhs), (rhs / length(rhs)));
}

template <typename... Ts, typename Rhs, EnableIf<!contains_linear_system(productexpr<Ts...>{})> = 0>
constexpr auto solve_scaled_vector_impl(equation<productexpr<Ts...>, Rhs> eq) {
  constexpr auto lhs = remove_if(is_scalarexpr, List(Ts{}...));
  constexpr auto rhs = eq.Rhs();
  return make_equation(product_list(lhs), (rhs / length(rhs)));
}

template <typename C, typename T, typename Rhs, EnableIf<is_scalarexpr(C{}) && is_constant(C{}) && is_vector_of_vars(T{})> = 0>
constexpr auto solve_scaled_vector_impl(equation<productexpr<C, T>, Rhs> eq) {
  constexpr auto rhs = eq.Rhs();
  return make_equation(T{}, rcp(C{}) * rhs);
}

template <typename S, typename M, typename V, typename Rhs, EnableIf<is_scalarexpr(S{}) && is_projection_vector(V{})> = 0>
constexpr auto solve_scaled_vector_impl(equation<productexpr<S, M, V>, Rhs> eq) {
  constexpr auto scale_factor = find(is_constant, remove_front(operands(V{})));
  constexpr auto scale_factor_index = index_of(scale_factor, remove_front(operands(V{})));

  constexpr auto rhs = (scale_factor / at<scale_factor_index>(Rhs{})) * Rhs{};
  return make_equation(M{} * V{}, rhs);
}

template <typename C, typename S, typename V, typename Rhs, EnableIf<is_constant(C{}) && is_vectorexpr(C{}) && is_scalarexpr(S{}) && is_vector_of_vars(V{})> = 0>
constexpr auto solve_scaled_vector(equation<addexpr<productexpr<minus_one_t, C>, productexpr<literal<2, 1>, S, V>>, Rhs> eq) {
  constexpr auto h = normalize(Rhs{} + C{});
  return make_equation(V{}, when(at<2>(h) > zero, h));
}

template <typename P, typename C, typename S, typename V, typename Rhs, EnableIf<is_scalarexpr(P{}) && is_vectorexpr(C{}) && is_scalarexpr(S{}) && is_vector_of_vars(V{})> = 0>
constexpr auto solve_scaled_vector(equation<addexpr<constantsubexpr<productexpr<minus_one_t, P, C>>, productexpr<S, V>>, Rhs> eq) {
  constexpr auto h = normalize(Rhs{} + P{} * C{});
  constexpr auto rhs = pattern(
    when(at<2>(h) > zero, h)
    , otherwise(-h)
  );

  return make_equation(V{}, when(at<2>(C{}) * at<2>(Rhs{}) < zero, rhs));
}

template <typename Lhs, typename Rhs>
constexpr auto solve_scaled_vector(equation<Lhs, Rhs> eq) {
  return solve_scaled_vector_impl(move_constants_to_rhs(eq));
}

template <typename C, typename V, typename Rhs>
constexpr auto solve_scaled_vector(equation<branchcaseexpr<C, V>, Rhs>) {
  constexpr auto result = solve_scaled_vector(make_equation(V{}, Rhs{}));
  return make_equation(result.Lhs(), make_expr(branch_case_tag, C{}, result.Rhs()));
}

constexpr auto devectorize_impl(TypeList<>, TypeList<>) {
  return List();
}

template <typename T, typename... Ts, typename V, typename... Vs, EnableIf<!is_variable(T{}) && !is_varsubexpr(T{})> = 0>
constexpr auto devectorize_impl(TypeList<T, Ts...>, TypeList<V, Vs...>) {
  return devectorize_impl(TypeList<Ts...>{}, TypeList<Vs...>{});
}

template <typename T, typename... Ts, typename V, typename... Vs, EnableIf<is_variable(T{}) || is_varsubexpr(T{})> = 0>
constexpr auto devectorize_impl(TypeList<T, Ts...>, TypeList<V, Vs...>) {
  return List(make_equation(T{}, V{})).Concat(devectorize_impl(TypeList<Ts...>{}, TypeList<Vs...>{}));
}

template <typename Lhs, typename Rhs, std::size_t... Is>
constexpr auto devectorize(equation<Lhs, Rhs>, std::index_sequence<Is...>) {
  return devectorize_impl(List(at<Is>(Lhs{})...), List(at<Is>(Rhs{})...));
}

template <typename Lhs, typename Rhs, EnableIf<is_vector_of_vars_or_constants(Lhs{})> = 0>
constexpr auto devectorize(equation<Lhs, Rhs> eq) {
  return devectorize(eq, std::make_index_sequence<dimensions_of(Lhs{}).Rows()>{});
}

template <typename Lhs, typename Rhs, EnableIf<!is_vector_of_vars_or_constants(Lhs{})> = 0>
constexpr auto devectorize(equation<Lhs, Rhs> eq) {
  return eq;
}

template <typename K, typename V>
constexpr auto devectorize(TypeMap<K, V> kv) {
  return devectorize(make_equation(kv));
}

template <typename... Ts, typename... Rs>
constexpr auto invert_branch(TypeList<Ts...> conds, TypeList<Rs...> solved) {
  return make_equation_list(
    List(Rs{}.Lhs()...)
    , List(make_expr(branch_case_tag, Ts{}, Rs{}.Rhs())...)
  );
}

struct solve_barycentric_t {
  template <typename Lhs, typename Rhs>
  constexpr auto operator()(equation<Lhs, Rhs>) const {
    constexpr auto pairs = get_barycentric_pairs(Lhs{});
    constexpr auto weights = pick<0>(pairs);
    constexpr auto constants = pick<1>(pairs);

    constexpr auto X = Rhs{};
    constexpr auto a = at<0>(constants);
    constexpr auto b = at<1>(constants);
    constexpr auto c = at<2>(constants);

    constexpr auto v0 = b - a;
    constexpr auto v1 = c - a;
    constexpr auto v2 = X - a;

    constexpr auto d00 = dot(v0, v0);
    constexpr auto d01 = dot(v0, v1);
    constexpr auto d11 = dot(v1, v1);
    constexpr auto d20 = dot(v2, v0);
    constexpr auto d21 = dot(v2, v1);
    constexpr auto denom = d00 * d11 - d01 * d01;
    constexpr auto A = d11 * d20 - d01 * d21;
    constexpr auto B = d00 * d21 - d01 * d20;
    constexpr auto v = A / denom;
    constexpr auto w = B / denom;
    return make_vector_equation(List(at<0>(weights), at<1>(weights)), List(one - v - w, v));
  }
};

constexpr solve_barycentric_t solve_barycentric{};





struct are_scaled_vectors_with_origin_t {
  template <typename C, typename T, EnableIf<is_constant(C{})> = 0>
  constexpr bool impl(addexpr<C, T>) const {
    return impl(T{});
  }

  template <typename C, typename A, typename B, typename... Ts, typename Rhs>
  constexpr bool operator()(vectorexpr<C, addexpr<A, Ts...>, addexpr<B, Ts...>>) const {
    return true;
  }

  template <typename C, typename A, typename B, typename... Ts, typename Rhs>
  constexpr auto operator()(equation<vectorexpr<C, addexpr<A, Ts...>, addexpr<B, Ts...>>, Rhs> eq) const {
    return true;
  }
};

constexpr are_scaled_vectors_with_origin_t are_scaled_vectors_with_origin{};




struct solve_t {
  template <typename C, typename... Ts, typename... Vs, EnableIf<!is_vector_of_vars_or_constants(vectorexpr<C, Ts...>{})> = 0>
  constexpr auto impl(equation<vectorexpr<C, Ts...>, vectorexpr<C, Vs...>> eq) const {
    return impl(make_equation_list(eq.Lhs(), eq.Rhs()));
  }

  template <typename Lhs, typename Rhs, EnableIf<!is_vector_of_vars_or_constants(Lhs{}) && is_vector_of_vars(move_constants_to_rhs(equation<Lhs, Rhs>{}).Lhs())> = 0>
  constexpr auto impl(equation<Lhs, Rhs> eq) const {
    return move_constants_to_rhs(eq);
  }

  template <typename Lhs, typename Rhs, EnableIf<!is_vector_of_vars_or_constants(Lhs{}) && !is_vector_of_vars(move_constants_to_rhs(equation<Lhs, Rhs>{}).Lhs())> = 0>
  constexpr auto impl(equation<Lhs, Rhs> eq) const {
    return solve_for(eq, at<0>(vars(eq)));
  }

  template <typename C, typename V, typename Rhs>
  constexpr auto impl(equation<branchcaseexpr<C, V>, Rhs>) const {
    auto result = impl(make_equation(V{}, Rhs{}));
    auto eq = make_equation(result.Lhs(), make_expr(branch_case_tag, C{}, result.Rhs()));
    return eq;
  }

  template <typename Lhs, typename Rhs, EnableIf<is_vector_of_vars_or_constants(Lhs{})> = 0>
  constexpr auto impl(equation<Lhs, Rhs> eq) const {
    return eq;
  }

  template <typename Lhs, typename Rhs, EnableIf<!is_barycentric(equation<Lhs, Rhs>{}) && !is_linear_system(equation<Lhs, Rhs>{}) && !is_matrix_vector_product(equation<Lhs, Rhs>{}) && !is_scaled_vector(equation<Lhs, Rhs>{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs> eq) const {
    return impl(eq);
  }

  template <typename Lhs, typename Rhs, EnableIf<is_linear_system(equation<Lhs, Rhs>{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs> eq) const {
    return cramers_rule_3x3(eq);
  }

  template <typename Lhs, typename Rhs, EnableIf<is_barycentric(equation<Lhs, Rhs>{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs> eq) const {
    return solve_barycentric(eq);
  }

  template <typename Lhs, typename Rhs, EnableIf<is_matrix_vector_product(equation<Lhs, Rhs>{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs> eq) const {
    return this->operator()(solve_matrix_vector_product(eq));
  }

  template <typename Lhs, typename Rhs, EnableIf<is_scaled_vector(equation<Lhs, Rhs>{})> = 0>
  constexpr auto operator()(equation<Lhs, Rhs> eq) const {
    return this->operator()(solve_scaled_vector(eq));
  }

  template <typename... Ts, typename Rhs>
  constexpr auto operator()(equation<branchexpr<Ts...>, Rhs>) const {
    auto inverted_values = List(this->operator()(make_equation(get_value(Ts{}), Rhs{}))...);
    return invert_branch(expand_conditions(List(get_condition(Ts{})...)), inverted_values);
  }
};

constexpr solve_t solve{};




template <typename T>
constexpr auto insert_by_num_dependencies(T, TypeList<>) {
  return TypeList<T>{};
}

template <typename T, typename TRhs, typename S, typename SRhs, typename... Sorted, EnableIf<(vars(T{}).Size() >= vars(S{}).Size())> = 0>
constexpr auto insert_by_num_dependencies(TypeList<T, TRhs>, TypeList<TypeList<S, SRhs>, Sorted...>) {
  return TypeList<TypeList<S, SRhs>>{}.Concat(insert_by_num_dependencies(TypeList<T, TRhs>{}, TypeList<Sorted...>{}));
}

template <typename T, typename TRhs, typename S, typename SRhs, typename... Sorted, EnableIf<(vars(T{}).Size() < vars(S{}).Size())> = 0>
constexpr auto insert_by_num_dependencies(TypeList<T, TRhs>, TypeList<TypeList<S, SRhs>, Sorted...>) {
  return TypeList<TypeList<T, TRhs>, TypeList<S, SRhs>, Sorted...>{};
}

template <typename... Sorted>
constexpr auto sort_by_num_dependencies_impl(TypeList<Sorted...>, TypeList<>) {
  return TypeList<Sorted...>{};
}

template <typename... Sorted, typename T, typename... Ts>
constexpr auto sort_by_num_dependencies_impl(TypeList<Sorted...>, TypeList<T, Ts...>) {
  return sort_by_num_dependencies_impl(insert_by_num_dependencies(T{}, TypeList<Sorted...>{}), TypeList<Ts...>{});
}

template <typename... Ts>
constexpr auto sort_by_num_dependencies(TypeList<Ts...>) {
  return sort_by_num_dependencies_impl(List(), TypeList<Ts...>{});
}


template <typename V>
struct has_independent_variable_t {
  template <typename Lhs, typename Rhs>
  constexpr bool operator()(TypeList<Lhs, Rhs>) {
    return false;
  }

  template <typename Rhs>
  constexpr bool operator()(TypeList<V, Rhs>) {
    return true;
  }
};



template <typename... Vs, typename Map>
constexpr auto invert_var_subexprs_impl(TypeSet<Vs...>, Map m, TypeList<>) {
  return make_equation_list(m);
}

template <int A, int B>
constexpr auto reconcile_inverted_values(inverse_variable<A> a, inverse_variable<B>) {
  return a;
}

template <typename A, typename B>
constexpr auto reconcile_inverted_values(baseexpr<A>, baseexpr<B>) {
  return List(A{}, B{});
}

template <typename A, typename B>
constexpr auto reconcile_inverted_values(arcsinexpr<A>, arccosexpr<B>) {
  return make_expr(arctan2_tag, A{}, B{});
}

template <typename A, typename B>
constexpr auto reconcile_inverted_values(arccosexpr<A>, arcsinexpr<B>) {
  return make_expr(arctan2_tag, B{}, A{});
}

template <typename... Vs, typename Map, typename Lhs, typename Rhs, EnableIf<!Map{}.HasKey(Lhs{})> = 0>
constexpr auto add_inverted_subexpr_to_map_impl(TypeSet<Vs...>, Map, equation<Lhs, Rhs>) {
  return Map{}.Put(Lhs{}, Rhs{});
}

template <typename... Vs, typename Map, typename Lhs, typename Rhs, EnableIf<Map{}.HasKey(Lhs{}) && TypeSet<Vs...>{}.Has(Lhs{})> = 0>
constexpr auto add_inverted_subexpr_to_map_impl(TypeSet<Vs...>, Map, equation<Lhs, Rhs>) {
  return Map{};
}

template <typename... Vs, typename Map, typename Lhs, typename Rhs, EnableIf<Map{}.HasKey(Lhs{}) && !TypeSet<Vs...>{}.Has(Lhs{})> = 0>
constexpr auto add_inverted_subexpr_to_map_impl(TypeSet<Vs...>, Map, equation<Lhs, Rhs>) {
  constexpr auto existing = Map{}.Get(Lhs{});
  return Map{}.Put(Lhs{}, reconcile_inverted_values(existing, Rhs{}));
}

template <typename... Vs, typename Map>
constexpr auto add_inverted_subexpr_to_map(TypeSet<Vs...>, Map current, TypeList<>, TypeList<>) {
  return current;
}

template <typename... Vs, typename Map, typename K, typename... Keys, typename V, typename... Values>
constexpr auto add_inverted_subexpr_to_map(TypeSet<Vs...> vars, Map current, TypeList<K, Keys...>, TypeList<V, Values...>) {
  constexpr auto updated = add_inverted_subexpr_to_map_impl(vars, current, make_equation(K{}, V{}));
  return add_inverted_subexpr_to_map(vars, updated, TypeList<Keys...>{}, TypeList<Values...>{});
}

template <typename... Vs, typename Map, typename ResultMap>
constexpr auto add_inverted_subexpr_to_map(TypeSet<Vs...> vars, Map current, ResultMap results) {
  constexpr auto updated_map = add_inverted_subexpr_to_map(vars, current, results.Keys(), results.Values());
  constexpr auto updated_values = replace_from_map(updated_map.Values(), results);
  return make_type_map(updated_map.Keys(), updated_values);
}

template <typename... Vs, typename Map, typename... Lhs, typename... Rhs, EnableIf<all(is_variable, List(Lhs{}...))> = 0>
constexpr auto add_inverted_subexpr_to_map_dispatch(TypeSet<Vs...> vars, Map current, TypeMap<TypeList<Lhs...>, TypeList<Rhs...>> inverted) {
  return add_inverted_subexpr_to_map(vars, current, inverted);
}

template <typename... Vs, typename Map, typename... Lhs, typename... Rhs, EnableIf<!all(is_variable, List(Lhs{}...))> = 0>
constexpr auto add_inverted_subexpr_to_map_dispatch(TypeSet<Vs...>, Map current, TypeMap<TypeList<Lhs...>, TypeList<Rhs...>>) {
  return current;
}

template <typename... Vs, typename Map, typename Eq, typename... Eqs, EnableIf<(vars_excluding_conditionals(replace_from_map(Eq{}, Map{})).Size() == 0) && !can_be_reconciled(Eq{}, Map{})> = 0>
constexpr auto invert_var_subexprs_impl(TypeSet<Vs...> vars, Map, TypeList<Eq, Eqs...>) {
  return invert_var_subexprs_impl(vars, Map{}, TypeList<Eqs...>{});
}



template <typename LhsA, typename RhsA, typename LhsB, typename RhsB>
constexpr bool can_be_solved_together(equation<LhsA, RhsA>, equation<LhsB, RhsB>) {
  return false;
}

template <int ID, int SeqIndex, typename RhsA, typename RhsB>
constexpr bool can_be_solved_together(equation<powerexpr<variable<ID, SeqIndex>, minus_one_t>, RhsA>, equation<powerexpr<variable<ID, SeqIndex>, literal<-1, 2>>, RhsB>) {
  return true;
}

template <int ID, int SeqIndex, typename RhsA, typename RhsB>
constexpr bool can_be_solved_together(equation<sinexpr<variable<ID, SeqIndex>>, RhsA>, equation<cosexpr<variable<ID, SeqIndex>>, RhsB>) {
  return true;
}

template <int ID, int SeqIndex, typename RhsA, typename RhsB>
constexpr bool can_be_solved_together(equation<cosexpr<variable<ID, SeqIndex>>, RhsA>, equation<sinexpr<variable<ID, SeqIndex>>, RhsB>) {
  return true;
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr bool can_be_solved_together(equation<productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsA>, equation<productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsB>) {
  return true;
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr bool can_be_solved_together(equation<productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsA>, equation<productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsB>) {
  return true;
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr bool can_be_solved_together(equation<productexpr<variable<IDB, SeqIndexB>, sinexpr<variable<IDA, SeqIndexA>>>, RhsA>, equation<productexpr<variable<IDB, SeqIndexB>, cosexpr<variable<IDA, SeqIndexA>>>, RhsB>) {
  return true;
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr bool can_be_solved_together(equation<productexpr<variable<IDB, SeqIndexB>, cosexpr<variable<IDA, SeqIndexA>>>, RhsA>, equation<productexpr<variable<IDB, SeqIndexB>, sinexpr<variable<IDA, SeqIndexA>>>, RhsB>) {
  return true;
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr bool can_be_solved_together(equation<productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsA>, equation<productexpr<minus_one_t, cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsB>) {
  return true;
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr bool can_be_solved_together(equation<productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsA>, equation<productexpr<minus_one_t, sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsB>) {
  return true;
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex, int VSeqIndex>
constexpr bool can_be_solved_together(equation<powerexpr<variable<1, VSeqIndex>, constantsubexpr<productexpr<literal<2, 1>, param<ID, N, I, SeqIndex>>>>, inverse_variable<9>>, equation<powerexpr<variable<1, VSeqIndex>, param<ID, N, I, SeqIndex>>, inverse_variable<11>>) {
  return true;
}

template <typename Lhs, typename Rhs, typename... Eqs>
constexpr bool has_compatible_equation(equation<Lhs, Rhs> eq, TypeList<Eqs...>) {
  return (can_be_solved_together(eq, Eqs{}) || ... || false);
}

template <typename Lhs, typename Rhs, typename Eq, typename... Eqs, EnableIf<can_be_solved_together(equation<Lhs, Rhs>{}, Eq{})> = 0>
constexpr auto get_compatible_equation(equation<Lhs, Rhs> eq, TypeList<Eq, Eqs...>) {
  return List(List(eq, Eq{}), TypeList<Eqs...>{});
}

template <typename Lhs, typename Rhs, typename Eq, typename... Eqs, EnableIf<!can_be_solved_together(equation<Lhs, Rhs>{}, Eq{})> = 0>
constexpr auto get_compatible_equation(equation<Lhs, Rhs> eq, TypeList<Eq, Eqs...>) {
  constexpr auto result = get_compatible_equation(eq, TypeList<Eqs...>{});
  return List(at<0>(result), List(Eq{}).Concat(at<1>(result)));
}


template <int ID, int SeqIndex, typename RhsA, typename RhsB>
constexpr auto solve_compatible_equations(equation<powerexpr<variable<ID, SeqIndex>, minus_one_t>, RhsA>, equation<powerexpr<variable<ID, SeqIndex>, literal<-1, 2>>, RhsB>) {
  return make_equation(variable<ID, SeqIndex>{}, when(RhsB{} > zero, rcp(sq(RhsB{}))));
}

template <int ID, int SeqIndex, typename RhsA, typename RhsB>
constexpr auto solve_compatible_equations(equation<sinexpr<variable<ID, SeqIndex>>, RhsA>, equation<cosexpr<variable<ID, SeqIndex>>, RhsB>) {
  return make_equation(variable<ID, SeqIndex>{}, arctan2(RhsA{}, RhsB{}));
}

template <int ID, int SeqIndex, typename RhsA, typename RhsB>
constexpr auto solve_compatible_equations(equation<cosexpr<variable<ID, SeqIndex>>, RhsA> eq_a, equation<sinexpr<variable<ID, SeqIndex>>, RhsB> eq_b) {
  return solve_compatible_equations(eq_b, eq_a);
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr auto solve_compatible_equations(equation<productexpr<variable<IDB, SeqIndexB>, sinexpr<variable<IDA, SeqIndexA>>>, RhsA>, equation<productexpr<variable<IDB, SeqIndexB>, cosexpr<variable<IDA, SeqIndexA>>>, RhsB>) {
  return solve_compatible_equations(
    make_equation(productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>{}, RhsA{})
    , make_equation(productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>{}, RhsB{})
  );
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr auto solve_compatible_equations(equation<productexpr<variable<IDB, SeqIndexB>, cosexpr<variable<IDA, SeqIndexA>>>, RhsA>, equation<productexpr<variable<IDB, SeqIndexB>, sinexpr<variable<IDA, SeqIndexA>>>, RhsB>) {
  return solve_compatible_equations(
    make_equation(productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>{}, RhsA{})
    , make_equation(productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>{}, RhsB{})
  );
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr auto solve_compatible_equations(equation<productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsA>, equation<productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsB>) {
  return List(
    make_equation(variable<IDA, SeqIndexA>{}, arctan2(RhsA{}, RhsB{}))
    , make_equation(variable<IDB, SeqIndexB>{}, sqrt(sq(RhsA{}) + sq(RhsB{})))
  );
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr auto solve_compatible_equations(equation<productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsA> eq_a, equation<productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsB> eq_b) {
  return solve_compatible_equations(eq_b, eq_a);
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr auto solve_compatible_equations(equation<productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsA> eq_a, equation<productexpr<minus_one_t, cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsB>) {
  return solve_compatible_equations(eq_a, make_equation(productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>{}, minus_one * RhsB{}));
}

template <int IDA, int SeqIndexA, typename RhsA, int IDB, int SeqIndexB, typename RhsB>
constexpr auto solve_compatible_equations(equation<productexpr<cosexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsA> eq_a, equation<productexpr<minus_one_t, sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>, RhsB>) {
  return solve_compatible_equations(eq_a, make_equation(productexpr<sinexpr<variable<IDA, SeqIndexA>>, variable<IDB, SeqIndexB>>{}, minus_one * RhsB{}));
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex, int VSeqIndex>
constexpr auto solve_compatible_equations(equation<powerexpr<variable<1, VSeqIndex>, constantsubexpr<productexpr<literal<2, 1>, param<ID, N, I, SeqIndex>>>>, inverse_variable<9>>, equation<powerexpr<variable<1, VSeqIndex>, param<ID, N, I, SeqIndex>>, inverse_variable<11>> eq_b) {
  return make_equation(variable<1, VSeqIndex>{}, when(inverse_variable<11>{} > zero, pow(inverse_variable<11>{}, one / param<ID, N, I, SeqIndex>{})));
}

template <typename Lhs, typename Rhs, typename Solved>
constexpr bool can_be_reconciled(equation<Lhs, Rhs>, Solved) {
  return false;
}

template <int ID, int SeqIndex, typename Rhs, typename Solved, EnableIf<!Solved{}.HasKey(variable<ID, SeqIndex>{})> = 0>
constexpr bool can_be_reconciled(equation<cosexpr<variable<ID, SeqIndex>>, Rhs>, Solved) {
  return false;
} 

template <int ID, int SeqIndex, typename Rhs, typename Solved, EnableIf<Solved{}.HasKey(variable<ID, SeqIndex>{})> = 0>
constexpr bool can_be_reconciled(equation<cosexpr<variable<ID, SeqIndex>>, Rhs>, Solved solved) {
  return is_arcsin(solved.Get(variable<ID, SeqIndex>{}));
} 

template <typename... Vs, typename Map, typename Eq, typename... Eqs, EnableIf<has_compatible_equation(Eq{}, TypeList<Eqs...>{})> = 0>
constexpr auto invert_var_subexprs_impl(TypeSet<Vs...> vars, Map, TypeList<Eq, Eqs...>) {
  constexpr auto compatible_equations = get_compatible_equation(Eq{}, TypeList<Eqs...>{});
  constexpr auto remaining_equations = at<1>(compatible_equations);
  constexpr auto eq_a = replace_from_map(at<0>(at<0>(compatible_equations)), Map{});
  constexpr auto eq_b = replace_from_map(at<1>(at<0>(compatible_equations)), Map{});

  constexpr auto inverted = solve_compatible_equations(eq_a, eq_b);
  constexpr auto updated_map = add_inverted_subexpr_to_map_dispatch(vars, Map{}, make_type_map(inverted));
  return invert_var_subexprs_impl(vars, updated_map, remaining_equations);
}

template <typename... Vs, typename Map, typename Eq, typename... Eqs, EnableIf<(vars_excluding_conditionals(replace_from_map(Eq{}, Map{})).Size() > 0) && !has_compatible_equation(Eq{}, TypeList<Eqs...>{})> = 0>
constexpr auto invert_var_subexprs_impl(TypeSet<Vs...> vars, Map, TypeList<Eq, Eqs...>) {
  constexpr auto substituted = replace_from_map(Eq{}, Map{});
  constexpr auto inverted = devectorize(solve(substituted));
  constexpr auto updated_map = add_inverted_subexpr_to_map_dispatch(vars, Map{}, make_type_map(inverted));
  return invert_var_subexprs_impl(vars, updated_map, TypeList<Eqs...>{});
}

template <typename T, typename Map, EnableIf<is_leaf(T{})> = 0>
constexpr T replace_subexprs_with_vars_impl(baseexpr<T>, Map) {
  return {};
}

template <typename T, typename Map>
constexpr T replace_subexprs_with_vars_impl(coordinate_system<T>, Map) {
  return {};
}

template <std::size_t I, typename Map>
constexpr _size_t<I> replace_subexprs_with_vars_impl(_size_t<I>, Map) {
  return {};
}

template <typename T, typename Map, EnableIf<!is_leaf(T{})> = 0>
constexpr auto replace_subexprs_with_vars_impl(baseexpr<T>, Map) {
  return make_expr(tag_of(T{}), replace_subexprs_with_vars_impl(operands(T{}), Map{}));
}

template <typename T, typename Map, EnableIf<Map{}.HasKey(T{})> = 0>
constexpr auto replace_subexprs_with_vars_impl(varsubexpr<T>, Map) {
  return Map{}.Get(T{});
}

template <typename T, typename Map, EnableIf<!Map{}.HasKey(T{})> = 0>
constexpr auto replace_subexprs_with_vars_impl(varsubexpr<T>, Map) {
  return varsubexpr<T>{};
}

template <typename Lhs, typename Rhs, typename Map>
constexpr auto replace_subexprs_with_vars_impl(equation<Lhs, Rhs>, Map m) {
  return make_equation(replace_subexprs_with_vars_impl(Lhs{}, m), Rhs{});
}

template <typename... Ts, typename Map>
constexpr auto replace_subexprs_with_vars_impl(TypeList<Ts...>, Map) {
  return List(replace_subexprs_with_vars_impl(Ts{}, Map{})...);
}

template <typename... Ts, typename Map>
constexpr auto replace_subexprs_with_vars(TypeList<Ts...>, Map) {
  return List(replace_subexprs_with_vars_impl(Ts{}, Map{})...);
}


template <std::size_t B, std::size_t... Is>
constexpr auto make_inverse_variable_for_impl(_size_t<B>, std::index_sequence<Is...>) {
  return make_vector_expr(inverse_variable<B + Is>{}...);
}

template <std::size_t B, typename T, EnableIf<is_scalarexpr(T{})> = 0>
constexpr auto make_inverse_variable_for(_size_t<B> base, baseexpr<T>) {
  return List(inverse_variable<B>{}, _size_t<B + 1>{});
}

template <std::size_t B, typename T, EnableIf<is_vector(T{})> = 0>
constexpr auto make_inverse_variable_for(_size_t<B> base, baseexpr<T>) {
  constexpr auto N = dimensions_of(T{}).Rows();
  return List(make_inverse_variable_for_impl(base, std::make_index_sequence<N>{}), _size_t<B + N>{});
}

template <std::size_t B, typename... Rs>
constexpr auto make_inverse_variable_for_vector_impl(_size_t<B> base, TypeList<>, TypeList<Rs...> results) {
  return List(make_vector_expr(results), base);
}

template <std::size_t B, typename T, typename... Ts, typename... Rs>
constexpr auto make_inverse_variable_for_vector_impl(_size_t<B> base, TypeList<T, Ts...>, TypeList<Rs...> result) {
  constexpr auto N = dimensions_of(T{}).Rows();
  constexpr auto next = make_inverse_variable_for_impl(base, std::make_index_sequence<N>{});
  return make_inverse_variable_for_vector_impl(_size_t<B + N>{}, TypeList<Ts...>{}, append(result, next));
}

template <std::size_t B, typename C, typename... Ts>
constexpr auto make_inverse_variable_for_vector(_size_t<B> base, vectorexpr<C, Ts...>) {
  //constexpr auto N = (dimensions_of(Ts{}).Rows() + ... + 0);
  return make_inverse_variable_for_vector_impl(base, TypeList<Ts...>{}, List());
}



constexpr TypeList<> make_subexpr_equations_impl(TypeList<>, TypeList<>) {
  return {};
}

template <typename Lhs, typename Rhs>
constexpr auto make_subexpr_equations_impl(TypeList<equation<Lhs, Rhs>> eqs, TypeList<>) {
  return eqs;
}

template <typename Lhs, typename Rhs, typename... Ts, typename I, typename... Is>
constexpr auto make_subexpr_equations_impl(TypeList<equation<Lhs, Rhs>, Ts...>, TypeList<I, Is...>) {
  constexpr auto eq = equation<Lhs, Rhs>{};
  return List(eq).Concat(make_subexpr_equations_impl(TypeList<Ts...>{}, TypeList<I, Is...>{}));
}

template <typename T, typename... Ts, typename I, typename... Is>
constexpr auto make_subexpr_equations_impl(TypeList<T, Ts...>, TypeList<I, Is...>) {
  constexpr auto result = make_inverse_variable_for(I{}, T{});
  constexpr auto eq = make_equation(T{}, at<0>(result));
  return List(eq).Concat(make_subexpr_equations_impl(TypeList<Ts...>{}, TypeList<Is...>{}));
}

template <std::size_t ID, typename... Ts, typename... Is>
constexpr auto make_subexpr_equations(_size_t<ID>, TypeList<Ts...> subexprs, TypeList<Is...>) {
  constexpr auto replaced = replace_subexprs_with_vars(
    TypeList<Ts...>{}
    , make_type_map(remove_if(is_equation, subexprs), List(variable<ID + Is::value>{}...))
  );

  constexpr auto inverse_variable_ids = List(_size_t<ID + Is::value>{}...);
  return make_subexpr_equations_impl(replaced, inverse_variable_ids);
}

template <typename... Vs, int ID, int SeqIndex, EnableIf<!contains(variable<ID, SeqIndex>{}, TypeList<Vs...>{})> = 0>
constexpr inverse_variable<ID> var_to_inverse_var(TypeList<Vs...>, variable<ID, SeqIndex>) {
  return {};
}

template <typename... Vs, int ID, int SeqIndex, EnableIf<contains(variable<ID, SeqIndex>{}, TypeList<Vs...>{})> = 0>
constexpr variable<ID, SeqIndex> var_to_inverse_var(TypeList<Vs...>, variable<ID, SeqIndex>) {
  return {};
}

template <typename... Vs, typename C, typename... Ts>
constexpr auto var_to_inverse_var(TypeList<Vs...> vars, vectorexpr<C, Ts...>) {
  return make_vector_expr(var_to_inverse_var(vars, Ts{})...);
}

template <typename Lhs, typename Rhs, typename Map, EnableIf<!Map{}.HasKey(Lhs{})> = 0>
constexpr equation<Lhs, Rhs> replace_vars_with_inverse_vars(equation<Lhs, Rhs>, Map) {
  return {};
}

template <typename Lhs, typename Rhs, typename Map, EnableIf<Map{}.HasKey(Lhs{})> = 0>
constexpr auto replace_vars_with_inverse_vars(equation<Lhs, Rhs>, Map) {
  constexpr auto key = Lhs{};
  return make_equation(Map{}[key], Rhs{});
}

template <typename Var, typename Map>
constexpr auto compose_inverted_subexpr(Var, Map) {
  constexpr auto key = Var{};
  return replace_from_map_key_found(key, Map{}[key], Map{});
}

template <typename T>
constexpr T remove_conditions_from_mappings_impl(baseexpr<T>) {
  return {};
}

template <typename Cond, typename V>
constexpr V remove_conditions_from_mappings_impl(branchcaseexpr<Cond, V>) {
  return {};
}

template <typename... Ks, typename... Vs>
constexpr auto remove_conditions_from_mappings(TypeMap<TypeList<Ks...>, TypeList<Vs...>>) {
}

template <typename... Vs, typename Map>
constexpr auto compose_inverted_subexprs_impl(TypeList<Vs...>, Map) {
  return List(compose_inverted_subexpr(Vs{}, Map{})...);
}

template <std::size_t N, typename T, EnableIf<is_leaf(T{})> = 0>
constexpr T adjust_inverse_vars_impl(_size_t<N>, baseexpr<T>) {
  return {};
}

template <std::size_t N, std::size_t I>
constexpr _size_t<I> adjust_inverse_vars_impl(_size_t<N>, _size_t<I> i) {
  return i;
}

template <std::size_t N, typename T>
constexpr T adjust_inverse_vars_impl(_size_t<N>, coordinate_system<T>) {
  return {};
}

template <std::size_t N, int ID>
constexpr inverse_variable<ID - N> adjust_inverse_vars_impl(_size_t<N>, inverse_variable<ID>) {
  return {};
}

template <std::size_t N, typename... Ts>
constexpr auto adjust_inverse_vars_impl(_size_t<N>, TypeList<Ts...>) {
  return List(adjust_inverse_vars_impl(_size_t<N>{}, Ts{})...);
}

template <std::size_t N, typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto adjust_inverse_vars_impl(_size_t<N>, baseexpr<T>) {
  return make_expr(tag_of(T{}), adjust_inverse_vars_impl(_size_t<N>{}, operands(T{})));
}

template <std::size_t N, typename T>
constexpr auto adjust_inverse_vars(_size_t<N>, baseexpr<T>) {
  return adjust_inverse_vars_impl(_size_t<N>{}, T{});
}

template <std::size_t N, typename... Ts>
constexpr auto adjust_inverse_vars(_size_t<N>, TypeList<Ts...>) {
  return List(adjust_inverse_vars_impl(_size_t<N>{}, Ts{})...);
}

template <std::size_t N, typename K, typename V>
constexpr auto adjust_inverse_vars(_size_t<N>, TypeMap<K, V>) {
  return make_type_map(adjust_inverse_vars(_size_t<N>{}, K{}), adjust_inverse_vars(_size_t<N>{}, V{}));
}




template <typename T, typename Map, EnableIf<is_leaf(T{}) && !Map{}.HasKey(T{})> = 0>
constexpr T compose_subexprs_in_conditions_impl(baseexpr<T>, Map) {
  return {};
}

template <typename... Ts, typename Map>
constexpr auto compose_subexprs_in_conditions_impl(TypeList<Ts...>, Map m) {
  return List(compose_subexprs_in_conditions_impl(Ts{}, m)...);
}

template <typename T, typename Map>
constexpr T compose_subexprs_in_conditions_impl(coordinate_system<T>, Map) {
  return {};
}

template <std::size_t I, typename Map>
constexpr _size_t<I> compose_subexprs_in_conditions_impl(_size_t<I> i, Map) {
  return i;
}

template <typename T, typename Map, EnableIf<!is_leaf(T{}) && !Map{}.HasKey(T{})> = 0>
constexpr auto compose_subexprs_in_conditions_impl(baseexpr<T>, Map) {
  return make_expr(tag_of(T{}), compose_subexprs_in_conditions_impl(operands(T{}), Map{}));
}

template <typename Map, typename K, typename... Ks, typename C, typename V, typename... Vs>
constexpr auto compose_subexprs_in_conditions(Map m, TypeList<K, Ks...>, TypeList<branchcaseexpr<C, V>, Vs...>) {
  constexpr auto rest = compose_subexprs_in_conditions(TypeList<Ks...>{}, TypeList<Vs...>{});
  m.Remove(K{});

  return make_type_map(List(K{}), List(V{})).Merge(rest);
}

template <typename Map, typename K, typename... Ks, typename V, typename... Vs>
constexpr auto compose_subexprs_in_conditions(Map m, TypeList<K, Ks...>, TypeList<V, Vs...>) {
  constexpr auto rest = compose_subexprs_in_conditions(m, TypeList<Ks...>{}, TypeList<Vs...>{});
  return make_type_map(List(K{}), List(V{})).Merge(rest);
}

template <typename V, typename... Vs, typename Map>
constexpr auto compose_subexprs_in_conditions(TypeList<V, Vs...>, Map m) {
  constexpr auto var = V{};
  return compose_subexprs_in_conditions(m, var, m[var]);
}




template <std::size_t ID, typename... Vs, typename... Eqs>
constexpr auto compose_inverted_subexprs(_size_t<ID> num_rows, TypeList<Vs...> vars, TypeList<Eqs...>) {
  constexpr auto mappings = make_type_map(
    List(var_to_inverse_var(vars, Eqs{}.Lhs())...)
    , List(Eqs{}.Rhs()...)
  );

  return make_type_map(vars, adjust_inverse_vars(num_rows, compose_inverted_subexprs_impl(vars, mappings)));
}

template <typename T>
constexpr auto insert_by_num_vars(T, TypeList<>) {
  return TypeList<T>{};
}

template <typename T, typename TRhs, typename S, typename SRhs, typename... Sorted, EnableIf<(vars(T{}).Size() >= vars(S{}).Size())> = 0>
constexpr auto insert_by_num_vars(equation<T, TRhs>, TypeList<equation<S, SRhs>, Sorted...>) {
  return TypeList<equation<S, SRhs>>{}.Concat(insert_by_num_vars(equation<T, TRhs>{}, TypeList<Sorted...>{}));
}

template <typename T, typename TRhs, typename S, typename SRhs, typename... Sorted, EnableIf<(vars(T{}).Size() < vars(S{}).Size())> = 0>
constexpr auto insert_by_num_vars(equation<T, TRhs>, TypeList<equation<S, SRhs>, Sorted...>) {
  return TypeList<equation<T, TRhs>, equation<S, SRhs>, Sorted...>{};
}

template <typename... Sorted>
constexpr auto sort_equations_by_num_vars_impl(TypeList<Sorted...>, TypeList<>) {
  return TypeList<Sorted...>{};
}

template <typename... Sorted, typename T, typename... Ts>
constexpr auto sort_equations_by_num_vars_impl(TypeList<Sorted...>, TypeList<T, Ts...>) {
  return sort_equations_by_num_vars_impl(insert_by_num_vars(T{}, TypeList<Sorted...>{}), TypeList<Ts...>{});
}

template <typename... Eqs>
constexpr auto sort_equations_by_num_vars(TypeList<Eqs...>) {
  return sort_equations_by_num_vars_impl(List(), TypeList<Eqs...>{});
}

template <typename... Bs>
constexpr auto find_first_common_var_subexpr(TypeSet<>, TypeSet<Bs...>) {
  static_assert(sizeof...(Bs) != sizeof...(Bs), "Cannot invert.");
  return List();
}

template <typename A, typename... As, typename... Bs, EnableIf<!contains(A{}, TypeList<Bs...>{})> = 0>
constexpr auto find_first_common_var_subexpr(TypeSet<A, As...>, TypeSet<Bs...>) {
  return find_first_common_var_subexpr(TypeSet<As...>{}, TypeSet<Bs...>{});
}

template <typename A, typename... As, typename... Bs, EnableIf<contains(A{}, TypeList<Bs...>{})> = 0>
constexpr A find_first_common_var_subexpr(TypeSet<A, As...>, TypeSet<Bs...>) {
  return {}; 
}

template <typename... Ts>
constexpr auto find_first_common_var_subexpr_all(TypeSet<>, Ts...) {
  static_assert(sizeof...(Ts) != sizeof...(Ts), "Cannot invert.");
  return List();
}

template <typename A, typename... As, typename... Ts, EnableIf<!((dependents(A{}) == dependents(Ts{})) && ... && true)> = 0>
constexpr auto find_first_common_var_subexpr_all(TypeSet<A, As...>, Ts... sets) {
  return find_first_common_var_subexpr_all(TypeSet<As...>{}, sets...);
}

template <typename A, typename... As, typename... Ts, EnableIf<((dependents(A{}) == dependents(Ts{})) && ... && true)> = 0>
constexpr A find_first_common_var_subexpr_all(TypeSet<A, As...>, Ts...) {
  return {}; 
}

template <typename... Ts>
constexpr auto find_first_common_var_subexpr_all(TypeList<Ts...>) {
  return find_first_common_var_subexpr_all(Ts{}...); 
}

template <typename... Ds>
constexpr bool has_dependencies(TypeList<>, TypeSet<Ds...>) {
  return false;
}

template <typename T, typename... Ts, typename... Ds, EnableIf<!(dependents(T{}) == TypeSet<Ds...>{})> = 0>
constexpr bool has_dependencies(TypeList<T, Ts...>, TypeSet<Ds...> deps) {
  return has_dependencies(TypeList<Ts...>{}, deps);
}

template <typename T, typename... Ts, typename... Ds, EnableIf<(dependents(T{}) == TypeSet<Ds...>{})> = 0>
constexpr bool has_dependencies(TypeList<T, Ts...>, TypeSet<Ds...>) {
  return true;
}


template <typename... Ts>
constexpr bool have_common_dependencies(TypeList<>, Ts...) {
  return false;
}

template <typename A, typename... As, typename... Ts, EnableIf<!(has_dependencies(Ts{}, dependents(A{})) && ... && true)> = 0>
constexpr bool have_common_dependencies(TypeList<A, As...>, Ts... lists) {
  return have_common_dependencies(TypeList<As...>{}, lists...);
}

template <typename A, typename... As, typename... Ts, EnableIf<(has_dependencies(Ts{}, dependents(A{})) && ... && true)> = 0>
constexpr bool have_common_dependencies(TypeList<A, As...>, Ts...) {
  return true; 
}

template <typename... Ts>
constexpr bool have_common_dependencies(TypeList<Ts...>) {
  return have_common_dependencies(Ts{}...); 
}



template <typename A, typename... As, typename... Ts, EnableIf<!(has_dependencies(Ts{}, dependents(A{})) && ... && true)> = 0>
constexpr auto find_common_dependencies_impl(TypeList<A, As...>, Ts... lists) {
  return find_common_dependencies_impl(TypeList<As...>{}, lists...);
}

template <typename A, typename... As, typename... Ts, EnableIf<(has_dependencies(Ts{}, dependents(A{})) && ... && true)> = 0>
constexpr auto find_common_dependencies_impl(TypeList<A, As...>, Ts...) {
  return dependents(A{}); 
}

template <typename... Ts>
constexpr auto find_common_dependencies(TypeList<Ts...>) {
  return find_common_dependencies_impl(Ts{}...); 
}



template <typename T>
constexpr T expand_var_subexprs_until(baseexpr<T>, TypeList<>) {
  return {}; 
}

template <typename T, typename V, typename... Vs>
constexpr auto expand_var_subexprs_until(baseexpr<T>, TypeList<V ,Vs...>) {
  return expand_var_subexprs_until(expand_var_subexpr(T{}, V{}), TypeList<Vs...>{});
}

template <typename... Ts, typename... Vs>
constexpr auto expand_var_subexprs_until(TypeList<Ts...>, TypeList<Vs...>) {
  return List(expand_var_subexprs_until(Ts{}, TypeList<Vs...>{})...);
}




template <typename A>
constexpr auto find_compatible_subexpr(baseexpr<A>, TypeSet<>) {
  static_assert(!std::is_same<A, A>::value, "Cannot invert.");
  return List();
}

template <typename A, typename B, typename... Ts, EnableIf<(collect_var_subexprs(A{}).Difference(collect_var_subexprs(B{}))).Size() != 1 || (collect_var_subexprs(B{}).Difference(collect_var_subexprs(A{}))).Size() != 1> = 0>
constexpr B find_compatible_subexpr(baseexpr<A>, TypeSet<B, Ts...>) {
  return find_compatible_subexpr(A{}, TypeSet<Ts...>{});
}

template <typename A, typename B, typename... Ts, EnableIf<(collect_var_subexprs(A{}).Difference(collect_var_subexprs(B{}))).Size() == 1 && (collect_var_subexprs(B{}).Difference(collect_var_subexprs(A{}))).Size() == 1> = 0>
constexpr auto find_compatible_subexpr(baseexpr<A>, TypeSet<B, Ts...>) {
  return B{};
}

template <typename A>
constexpr bool has_compatible_subexpr(baseexpr<A>, TypeSet<>) {
  return false;
}

template <typename A, typename B>
constexpr bool has_compatible_subexpr_impl(baseexpr<A>, baseexpr<B>) {
  return (collect_var_subexprs(A{}).Difference(collect_var_subexprs(B{}))).Size() == 1
    && (collect_var_subexprs(B{}).Difference(collect_var_subexprs(A{}))).Size() == 1;
}

template <typename A, typename... Ts>
constexpr bool has_compatible_subexpr(baseexpr<A>, TypeSet<Ts...>) {
  return (has_compatible_subexpr_impl(A{}, Ts{}) || ... || false);
}

constexpr auto reconcile_underdetermined(TypeSet<>) {
  return List(TypeSet<>{}, make_type_map(), TypeSet<>{}, TypeSet<>{});
}

template <typename T, typename... Ts, EnableIf<!has_compatible_subexpr(T{}, TypeSet<Ts...>{})> = 0>
constexpr auto reconcile_underdetermined(TypeSet<T, Ts...>) {
  constexpr auto rest = reconcile_underdetermined(TypeSet<Ts...>{});

  return List(
    at<0>(rest)
    , at<1>(rest)
    , at<2>(rest)
    , make_type_set(T{}).Merge(at<3>(rest))
  );
}

template <typename... Is, typename... Ts>
constexpr auto get_subexprs_to_expand(TypeList<Is...>, TypeList<Ts...>) {
  return List(slice(_size_t<0>{}, Is{}, Ts{})...);
}

template <typename... Es, typename... Ts>
constexpr auto expand_var_subexprs_until_all(TypeList<Es...>, TypeList<Ts...>) {
  return List(expand_var_subexprs_until(Es{}, Ts{})...);
}

template <typename... Ds, typename T, typename... Ts, EnableIf<TypeSet<Ds...>{} != dependents(T{})> = 0>
constexpr auto find_subexpr_with_dependents(TypeSet<Ds...> deps, TypeList<T, Ts...>) {
  return find_subexpr_with_dependents(deps, TypeList<Ts...>{}); 
}

template <typename... Ds, typename T, typename... Ts, EnableIf<TypeSet<Ds...>{} == dependents(T{})> = 0>
constexpr T find_subexpr_with_dependents(TypeSet<Ds...>, TypeList<T, Ts...>) {
  return {}; 
}

template <typename... Ds, typename... Ts>
constexpr auto index_of_subexpr_with_dependents(TypeSet<Ds...> deps, TypeList<Ts...> subexpr_list) {
  return _size_t<index_of(find_subexpr_with_dependents(deps, subexpr_list), subexpr_list)>{};
}

template <typename... Ds, typename... Ts>
constexpr auto index_of_subexpr_with_dependents_all(TypeSet<Ds...> deps, TypeList<Ts...>) {
  return List(index_of_subexpr_with_dependents(deps, Ts{})...);
}

template <typename... Ts, EnableIf<have_common_dependencies(TypeList<Ts...>{})> = 0>
constexpr auto find_first_common_dependencies_indices(TypeList<Ts...> subexpr_lists) {
  constexpr auto common_dependencies = find_common_dependencies(subexpr_lists);
  return index_of_subexpr_with_dependents_all(common_dependencies, subexpr_lists);
}

template <typename... Ts, EnableIf<!have_common_dependencies(TypeList<Ts...>{})> = 0>
constexpr auto find_first_common_dependencies_indices(TypeList<Ts...>) {
  static_assert(sizeof...(Ts) != sizeof...(Ts), "Cannot invert.");
  return List(_size_t<Ts{}.Size()>{}...); 
}

template <typename T, EnableIf<(collect_var_subexprs(T{}).Size() == 0)> = 0>
constexpr auto expand_var_subexprs_individually(baseexpr<T>) {
  return List(T{});
}

template <typename T, EnableIf<(collect_var_subexprs(T{}).Size() > 0)> = 0>
constexpr auto expand_var_subexprs_individually(baseexpr<T>) {
  constexpr auto next_var_subexpr = at<0>(collect_var_subexprs(T{}));
  return List(T{}).Concat(expand_var_subexprs_individually(expand_var_subexpr(T{}, next_var_subexpr)));
}

template <typename... Ts>
constexpr auto expand_var_subexprs_individually_all(TypeList<Ts...>) {
  return List(expand_var_subexprs_individually(Ts{})...);
}

template <typename... Ts, typename... Rs>
constexpr auto reconcile_underdetermined(TypeSet<branchexpr<Ts...>, Rs...>) {
  constexpr auto exprs = List(get_value(Ts{})...);
  constexpr auto exprs_with_expanded_var_subexprs = expand_var_subexprs_individually_all(exprs);

  constexpr auto indices = find_first_common_dependencies_indices(exprs_with_expanded_var_subexprs);
  constexpr auto subexprs_to_expand = get_subexprs_to_expand(indices, exprs_with_expanded_var_subexprs);
  constexpr auto replacements = at_all(indices, exprs_with_expanded_var_subexprs);
  constexpr auto subexprs_to_expand_merged = merge_list_of_sets(make_type_set_all(subexprs_to_expand));
  constexpr auto expanded_subexprs = merge_list_of_sets(collect_var_subexprs_all(make_list(subexprs_to_expand_merged)));

  constexpr auto mappings = make_type_map(exprs, replacements);
  constexpr auto rest = reconcile_underdetermined(TypeSet<Rs...>{});
  constexpr auto reconciled = make_type_set(make_branch_expr(List(get_condition(Ts{})...), replacements));

  return List(
    reconciled.Merge(at<0>(rest))
    , mappings.Merge(at<1>(rest))
    , expanded_subexprs.Merge(at<2>(rest))
    , at<3>(rest)
  );
}

template <typename T, typename... Ts, EnableIf<has_compatible_subexpr(T{}, TypeSet<Ts...>{})> = 0>
constexpr auto reconcile_underdetermined(TypeSet<T, Ts...>) {
  constexpr auto a = T{};
  constexpr auto b = find_compatible_subexpr(T{}, TypeSet<Ts...>{});
  constexpr auto sa = collect_var_subexprs(a);
  constexpr auto sb = collect_var_subexprs(b);
  constexpr auto unique_to_a = at<0>(sa.Difference(sb));
  constexpr auto unique_to_b = at<0>(sb.Difference(sa));
  constexpr auto subexpr_list_a = collect_var_subexprs_recursive(unique_to_a);
  constexpr auto subexpr_list_b = collect_var_subexprs_recursive(unique_to_b);

  constexpr auto first_common_var_subexpr = find_first_common_var_subexpr(subexpr_list_a, subexpr_list_b);

  constexpr std::size_t i_a = index_of(first_common_var_subexpr, make_list(subexpr_list_a));
  constexpr std::size_t i_b = index_of(first_common_var_subexpr, make_list(subexpr_list_b));

  constexpr auto subexprs_to_expand_in_a = slice(_size_t<0>{}, _size_t<i_a>{}, make_list(subexpr_list_a));
  constexpr auto expanded_a = expand_var_subexprs_until(a, subexprs_to_expand_in_a);
  constexpr auto subexprs_to_expand_in_b = slice(_size_t<0>{}, _size_t<i_b>{}, make_list(subexpr_list_b));
  constexpr auto expanded_b = expand_var_subexprs_until(b, subexprs_to_expand_in_b);
  constexpr auto expanded_subexprs = make_type_set(subexprs_to_expand_in_a).Merge(make_type_set(subexprs_to_expand_in_b));

  constexpr auto reconciled = make_type_set(expanded_a, expanded_b);

  constexpr auto mappings = make_type_map(List(a, b), List(expanded_a, expanded_b));
  constexpr auto rest = reconcile_underdetermined(TypeSet<Ts...>{}.Remove(b));

  return List(
    reconciled.Merge(at<0>(rest))
    , mappings.Merge(at<1>(rest))
    , expanded_subexprs.Merge(at<2>(rest))
    , at<3>(rest)
  );
}

template <typename T>
constexpr int var_subexpr_count(varsubexpr<T>) {
  return 1;
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr int var_subexpr_count(baseexpr<T>) {
  return 0;
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr int var_subexpr_count(baseexpr<T>) {
  return count(is_varsubexpr, operands(T{}));
}

template <typename... Ds, typename T>
constexpr int unsolved_var_subexpr_count(TypeList<Ds...>, varsubexpr<T> vse) {
  constexpr auto solved_var_subexprs = unique(filter(is_varsubexpr, operands(Ds{}))...);
  return difference(List(vse), solved_var_subexprs).Size();
}

template <typename... Ds, typename T, EnableIf<is_leaf(T{})> = 0>
constexpr int unsolved_var_subexpr_count(TypeList<Ds...>, baseexpr<T>) {
  return 0;
}

template <typename... Ds, typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr int unsolved_var_subexpr_count(TypeList<Ds...>, baseexpr<T>) {
  constexpr auto var_subexprs = filter(is_varsubexpr, operands(T{}));
  constexpr auto solved_var_subexprs = unique(List(filter(is_varsubexpr, operands(Ds{}))...));
  return difference(var_subexprs, solved_var_subexprs).Size();
}


template <typename... Rs, typename... Ts>
constexpr auto fix_underdetermined(TypeSet<Rs...>, TypeSet<Ts...> underdetermined) {
  constexpr auto reconciled = reconcile_underdetermined(underdetermined);
  constexpr auto subexprs = at<0>(reconciled);
  constexpr auto mappings = at<1>(reconciled);
  constexpr auto expanded = at<2>(reconciled);
  constexpr auto remaining_underdetermined = at<3>(reconciled);

  constexpr auto updated_var_subexprs = make_type_set(replace_from_map(Rs{}, mappings)...);
  return concat(
    make_list(updated_var_subexprs.Difference(expanded))
    , make_list(subexprs)
    , replace_from_map(make_list(remaining_underdetermined), mappings)
  );
}

template <typename Solved, typename T>
constexpr int unsolved_var_subexpr_count(Solved solved, baseexpr<T>) {
  constexpr auto deps = dependents(T{});
  return (deps.Difference(solved)).Size();
}

template <typename Solved, typename... Ts>
constexpr int unsolved_var_subexpr_count(Solved solved, branchexpr<Ts...>) {
  return (MergeAll(dependents(get_value(Ts{}))...).Difference(solved)).Size();
}

template <typename Solved, typename C, typename V>
constexpr int unsolved_var_subexpr_count(Solved solved, branchcaseexpr<C, V>) {
  return (dependents(V{}).Difference(solved)).Size();
}

template <typename Solved, typename T, EnableIf<is_vector(T{})> = 0>
constexpr bool solveable(Solved solved, baseexpr<T>) {
  return unsolved_var_subexpr_count(solved, T{}) <= dimensions_of(T{}).Rows();
}

template <typename Solved, typename T, EnableIf<!is_vector(T{})> = 0>
constexpr bool solveable(Solved solved, baseexpr<T>) {
  return unsolved_var_subexpr_count(solved, T{}) <= 1;
}

template <typename Solved, typename... Ts>
constexpr bool solveable(Solved solved, branchexpr<Ts...>) {
  return (solveable(solved, Ts{}) && ... && true);
}

template <typename Solved, typename Lhs, typename Rhs>
constexpr bool solveable(Solved solved, equation<Lhs, Rhs>) {
  return solveable(solved, Lhs{});
}

template <typename... Rs, typename T, typename... Ts, EnableIf<!solveable(TypeSet<Rs...>{}, T{})> = 0>
constexpr auto find_solveable_subexpr(TypeSet<Rs...> solved_var_subexprs, TypeList<T, Ts...>) {
  constexpr auto result = find_solveable_subexpr(solved_var_subexprs, TypeList<Ts...>{});
  return List(at<0>(result), concat(List(T{}), at<1>(result)));
}

template <typename... Rs, typename T, typename... Ts, EnableIf<solveable(TypeSet<Rs...>{}, T{})> = 0>
constexpr auto find_solveable_subexpr(TypeSet<Rs...>, TypeList<T, Ts...>) {
  return List(T{}, List(Ts{}...));
}

template <typename... Rs, typename... Ts>
constexpr bool has_solveable(TypeSet<Rs...> solved_var_subexprs, TypeList<Ts...>) {
  return (solveable(solved_var_subexprs, Ts{}) || ... || false);
}

template <typename... Rs, typename... Vs, typename... Ts, EnableIf<has_solveable(TypeSet<Rs...>{}, TypeList<Ts...>{})> = 0>
constexpr auto sort_by_dependencies_impl(TypeSet<Rs...> solved_var_subexprs, TypeSet<Vs...> solved_subexprs, TypeList<Ts...> subexprs) {
  constexpr auto result = find_solveable_subexpr(solved_var_subexprs, subexprs);
  constexpr auto new_solved = at<0>(result);
  constexpr auto remaining_subexprs = at<1>(result);

  return sort_by_dependencies_impl(
    solved_var_subexprs.Merge(dependents(new_solved))
    , solved_subexprs.Put(new_solved)
    , remaining_subexprs
  );
}


template <typename... Rs, typename... Vs, typename... Ts, EnableIf<!has_solveable(TypeSet<Rs...>{}, TypeList<Ts...>{})> = 0>
constexpr auto sort_by_dependencies_impl(TypeSet<Rs...>, TypeSet<Vs...> solved_subexprs, TypeList<Ts...> subexprs) {
  return List(solved_subexprs, make_type_set(subexprs));
}

template <typename... Ts>
constexpr auto sort_by_dependencies(TypeSet<Ts...>) {
  return sort_by_dependencies_impl(make_type_set(), make_type_set(), TypeList<Ts...>{});
}




template <int N>
constexpr auto inverse_variable_ids_impl(_int<N>, TypeList<>) {
  return List();
}

template <int N, typename Lhs, typename Rhs, typename... Ts>
constexpr auto inverse_variable_ids_impl(_int<N> id, TypeList<equation<Lhs, Rhs>, Ts...>) {
  return inverse_variable_ids_impl(id, TypeList<Ts...>{});
}

template <int N, typename T, typename... Ts>
constexpr auto inverse_variable_ids_impl(_int<N> id, TypeList<T, Ts...>) {
  constexpr auto cur_id = id;
  constexpr auto next_id = _int<N + dimensions_of(T{}).Rows()>{};
  return concat(List(cur_id), inverse_variable_ids_impl(next_id, TypeList<Ts...>{}));
}

template <typename... Ts>
constexpr auto inverse_variable_ids(TypeList<Ts...> exprs) {
  return inverse_variable_ids_impl(_int<0>{}, exprs);
}




template <typename Lhs, typename Rhs>
constexpr bool is_lhs_dotproduct(equation<Lhs, Rhs>) {
  return false;
}

template <typename A, typename B, typename Rhs>
constexpr bool is_lhs_dotproduct(equation<dotproduct<A, B>, Rhs>) {
  return true;
}

constexpr auto remove_dot_product(TypeList<>) {
  return List();
}

template <typename Eq, typename... Eqs, EnableIf<!is_lhs_dotproduct(Eq{})> = 0>
constexpr auto remove_dot_product(TypeList<Eq, Eqs...>) {
  return List(Eq{}).Concat(remove_dot_product(TypeList<Eqs...>{}));
}

template <typename Eq, typename... Eqs, EnableIf<is_lhs_dotproduct(Eq{})> = 0>
constexpr auto remove_dot_product(TypeList<Eq, Eqs...>) {
  return remove_dot_product(TypeList<Eqs...>{});
}

template <typename... Vars, std::size_t NumRows, std::size_t ID, typename... Ts>
constexpr auto invert_var_subexpr_branch(TypeSet<Vars...> vs, _size_t<NumRows> num_rows, _size_t<ID> inverse_var_id, TypeSet<Ts...> subexprs) {
  constexpr auto sorted = sort_by_dependencies(remove_front(subexprs));
  constexpr auto solveable_alone = make_type_set(front(subexprs)).Merge(at<0>(sorted));
  constexpr auto unsolveable_alone = at<1>(sorted);

  constexpr auto invertible = fix_underdetermined(solveable_alone, unsolveable_alone);

  constexpr auto eqs = remove_dot_product(make_subexpr_equations(
    inverse_var_id
    , invertible 
    , inverse_variable_ids(invertible)
  ));

  constexpr auto inverted = invert_var_subexprs_impl(vars(subexprs), make_type_map(), eqs);

  return compose_inverted_subexprs(num_rows, make_list(vars(subexprs)), inverted);
}

template <typename... Mappings>
struct invert_result {};

template <typename... Ts>
constexpr invert_result<Ts...> make_invert_result(Ts...) {
  return {};
}

template <typename... Ts>
constexpr TypeList<Ts...> as_list(invert_result<Ts...>) {
  return {};
}

template <typename C, typename A, typename B, typename D, typename S, typename T0, typename T1, typename R0, typename R1, typename M>
constexpr auto invert_var_subexprs(vectorexpr<C, addexpr<A, productexpr<T0, T1, M, S>, B, D>, addexpr<A, productexpr<minus_one_t, R0, R1, M, S>, B, D>>) {
  constexpr auto lhs = vectorexpr<C, productexpr<T1, M, S>, productexpr<R1, M, S>>{};

  constexpr auto replaced = replace_var_subexprs(lhs);
  constexpr auto min_inverse_variable_id = _size_t<get_id(max(make_list(vars(replaced)))) + 1>{};
  constexpr auto result = make_inverse_variable_for_vector(min_inverse_variable_id, replaced);
  constexpr auto inverse_var = at<0>(result);
  constexpr auto next_inverse_variable_id = at<1>(result);
  constexpr auto dir = at<0>(inverse_var) - at<1>(inverse_var);
  constexpr auto eq = preprocess(make_equation(at<0>(replaced), dir));

  constexpr auto dir_solution = invert_var_subexpr_branch(
    vars(lhs)
    , min_inverse_variable_id
    , next_inverse_variable_id
    , make_type_set(eq).Merge(collect_var_subexprs_recursive(eq))
  );

  constexpr auto triangle_vertices = get_barycentric_constants(addexpr<A, B, D>{});
  constexpr auto e1 = at<0>(triangle_vertices) - at<2>(triangle_vertices);
  constexpr auto e2 = at<1>(triangle_vertices) - at<2>(triangle_vertices);
  constexpr auto N = normalize(cross(e1, e2)); 
  constexpr auto org = at<1>(inverse_var);
  constexpr auto t = dot(at<0>(triangle_vertices) - org, N) / dot(normalize(dir), N);

  constexpr auto plane_intersection_point = org + t * normalize(dir);

  constexpr auto condition = adjust_inverse_vars(min_inverse_variable_id, dot(normalize(dir), N) != zero && t > zero);

  constexpr auto weighted_without_back = combine_weights_with_constants(
    remove_back(get_barycentric_weights(addexpr<A, B, D>{}))
    , remove_back(triangle_vertices)
  );
  constexpr auto replaced_weighted_without_back = replace_var_subexprs(weighted_without_back);
  constexpr auto back_weight = one - add_list(get_barycentric_weights(replaced_weighted_without_back));
  constexpr auto back_constant = back(triangle_vertices);

  constexpr auto replaced_barycentric = replaced_weighted_without_back + back_weight * back_constant;
  constexpr auto barycentric_eq = preprocess(make_equation(replaced_barycentric, plane_intersection_point));
  constexpr auto intersection_solution = invert_var_subexpr_branch(
    vars(addexpr<A, B, D>{})
    , min_inverse_variable_id
    , next_inverse_variable_id
    , make_type_set(barycentric_eq).Merge(collect_var_subexprs_recursive(barycentric_eq))
  );

  return add_coplanar_condition_to_results(intersection_solution, condition).Merge(dir_solution);
}

template <typename... Ws, typename... Cs>
constexpr auto combine_weights_with_constants(TypeList<Ws...>, TypeList<Cs...>) {
  return add_list(List(Ws{} * Cs{}...));
}

template <typename... Ks, typename... Vs, typename C>
constexpr auto add_coplanar_condition_to_results(TypeMap<TypeList<Ks...>, TypeList<Vs...>>, baseexpr<C>) {
  return make_type_map(TypeList<Ks...>{}, List(pattern(when(C{}, Vs{}), otherwise(minus_one))...));
}

template <typename T, EnableIf<is_barycentric(T{})> = 0>
constexpr auto invert_var_subexprs(baseexpr<T>) {
  constexpr auto constants = get_barycentric_constants(T{});
  constexpr auto weighted_without_back = combine_weights_with_constants(
    remove_back(get_barycentric_weights(T{}))
    , remove_back(constants)
  );
  constexpr auto replaced_weighted_without_back = replace_var_subexprs(weighted_without_back);
  constexpr auto back_weight = one - add_list(get_barycentric_weights(replaced_weighted_without_back));
  constexpr auto back_constant = back(constants);

  constexpr auto replaced = replaced_weighted_without_back + back_weight * back_constant;

  constexpr auto min_inverse_variable_id = _size_t<get_id(max(make_list(vars(replaced)))) + 1>{};
  constexpr auto result = make_inverse_variable_for(min_inverse_variable_id, replaced);
  constexpr auto inverse_var = at<0>(result);
  constexpr auto next_inverse_variable_id = at<1>(result);
  constexpr auto eq = preprocess(make_equation(replaced, inverse_var));

  constexpr auto X = eq.Rhs();
  constexpr auto a = at<0>(constants);
  constexpr auto b = at<1>(constants);
  constexpr auto c = at<2>(constants);

  constexpr auto v0 = b - a;
  constexpr auto v1 = c - a;
  constexpr auto v2 = X - a;

  constexpr auto coplanar = invert_var_subexpr_branch(
    vars(T{})
    , min_inverse_variable_id
    , next_inverse_variable_id
    , make_type_set(eq).Merge(collect_var_subexprs_recursive(eq))
  );

  constexpr auto coplanar_condition = adjust_inverse_vars(min_inverse_variable_id, abs(dot(normalize(cross(v0, v1)), v2)) < literal<1, 1000>{});
  return add_coplanar_condition_to_results(coplanar, coplanar_condition);
}

template <typename T, EnableIf<!is_barycentric(T{})> = 0>
constexpr auto invert_var_subexprs(baseexpr<T>) {
  constexpr auto replaced = replace_var_subexprs(T{});
  constexpr auto min_inverse_variable_id = _size_t<get_id(max(make_list(vars(replaced)))) + 1>{};
  constexpr auto result = make_inverse_variable_for(min_inverse_variable_id, replaced);
  constexpr auto inverse_var = at<0>(result);
  constexpr auto next_inverse_variable_id = at<1>(result);
  constexpr auto eq = preprocess(make_equation(replaced, inverse_var));
  return invert_var_subexpr_branch(
    vars(T{})
    , min_inverse_variable_id
    , next_inverse_variable_id
    , make_type_set(eq).Merge(collect_var_subexprs_recursive(eq))
  );
}



template <typename Tag, typename Conds, typename... Ts>
constexpr auto unpack_operands_impl(Tag tag, TypeList<Conds, TypeList<Ts...>>) {
  return List(
    Conds{}
    , make_expr(tag, TypeList<Ts...>{})
  );
}

template <typename Tag, typename... Ts>
constexpr auto unpack_operands(Tag tag, TypeList<Ts...>) {
  return List(unpack_operands_impl(tag, Ts{})...);
}

template <typename... Ts>
constexpr auto invert_branches_impl(TypeList<Ts...>) {
  return List(invert_var_subexprs(Ts{})...);
}

template <typename V>
constexpr V add_condition_to_inverse_values_impl(trueexpr, V) {
  return {};
}

template <typename Cond, typename V>
constexpr auto add_condition_to_inverse_values_impl(Cond, V) {
  return make_expr(branch_case_tag, Cond{}, V{});
}

template <typename V>
constexpr V add_extracted_condition_to_inverse_values(TypeList<trueexpr, V>) {
  return {};
}

template <typename Cond, typename V>
constexpr auto add_extracted_condition_to_inverse_values(TypeList<Cond, V>) {
  return when(Cond{}, V{});
}

template <typename... Ts>
constexpr TypeList<Ts...> check_if_inverse_is_valid(TypeList<Ts...>) {
  static_assert((is_constant(Ts{}) && ... && true), "Failed to invert.");
  return {};
}

template <typename... Ks, typename... Vs, typename Expr>
constexpr auto add_condition_to_inverse_values(TypeList<>, TypeMap<TypeList<Ks...>, TypeList<Vs...>> m, Expr) {
  constexpr auto keys = TypeList<Ks...>{};
  constexpr auto values = List(add_extracted_condition_to_inverse_values(extract_conditions(Vs{}))...);
  return make_type_map(keys, check_if_inverse_is_valid(values));
}

template <typename T, int ID, EnableIf<!is_guaranteed_positive(T{})> = 0>
constexpr auto get_additional_conditions_impl(baseexpr<T>, inverse_variable<ID>) {
  return trueexpr{};
}

template <typename T, int ID, EnableIf<is_guaranteed_positive(T{})> = 0>
constexpr auto get_additional_conditions_impl(baseexpr<T>, inverse_variable<ID> v) {
  return v > zero;
}

template <typename... Ts, std::size_t... Is>
constexpr auto get_additional_conditions(TypeList<Ts...>, std::index_sequence<Is...>) {
  return and_all(List(get_additional_conditions_impl(Ts{}, inverse_variable<Is>{})...));
}

template <typename C, typename... Ts>
constexpr auto get_additional_conditions(vectorexpr<C, Ts...>) {
  return get_additional_conditions(TypeList<Ts...>{}, std::index_sequence_for<Ts...>{});
}

template <typename T>
constexpr auto get_additional_conditions(baseexpr<T>) {
  return trueexpr{};
}

template <typename... Conds, typename... Ks, typename... Vs, typename Expr>
constexpr auto add_condition_to_inverse_values(TypeList<Conds...>, TypeMap<TypeList<Ks...>, TypeList<Vs...>>, Expr) {
  constexpr auto cond = (Conds{} && ... && trueexpr{});
  constexpr auto keys = TypeList<Ks...>{};
  constexpr auto values = List(make_expr(branch_case_tag, cond && at<0>(extract_conditions(Vs{})), at<1>(extract_conditions(Vs{})))...);
  return make_type_map(keys, values);
}


template <typename... Ks, typename... Vs>
constexpr auto extract_conditions_from_mapping(TypeMap<TypeList<Ks...>, TypeList<Vs...>>) {
  constexpr auto keys = TypeList<Ks...>{};
  return make_type_map(keys, List(at<1>(extract_conditions(Vs{}))...));
}

template <typename... Conds, typename... Mappings, typename... Exprs>
constexpr auto combine_conditions_with_inverse(TypeList<Conds...>, TypeList<Mappings...>, TypeList<Exprs...>) {
  return List(add_condition_to_inverse_values(replace_from_map(make_list(Conds{}), extract_conditions_from_mapping(Mappings{})), Mappings{}, Exprs{})...);
}

constexpr auto filter_inconsistent_conditions(TypeList<>) {
  return TypeList<>{};
}

template <typename Conds, typename Expr, typename... Ts, EnableIf<!consistent(and_all(Conds{}))> = 0>
constexpr auto filter_inconsistent_conditions(TypeList<TypeList<Conds, Expr>, Ts...>) {
  return filter_inconsistent_conditions(TypeList<Ts...>{});
}

template <typename Conds, typename Expr, typename... Ts, EnableIf<consistent(and_all(Conds{}))> = 0>
constexpr auto filter_inconsistent_conditions(TypeList<TypeList<Conds, Expr>, Ts...>) {
  return List(TypeList<Conds, Expr>{}).Concat(filter_inconsistent_conditions(TypeList<Ts...>{}));
}

template <typename... Ts>
constexpr auto invert_branches(TypeList<Ts...>) {
  constexpr auto conds = pick<0>(List(Ts{}...));
  constexpr auto exprs = pick<1>(List(Ts{}...));
  return combine_conditions_with_inverse(conds, invert_branches_impl(exprs), exprs);
}

template <typename T>
constexpr auto invert(baseexpr<T>) {
  return invert_branches(filter_inconsistent_conditions(move_conditions_outward(T{})));
}



} // end namespace aether


#endif

