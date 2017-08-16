#ifndef EQUATION_H
#define EQUATION_H

#include "Expr.h"
#include "Variable.h"
#include "aether/typelist/TypeMap.h"

namespace aether {

template <typename L, typename R>
struct equation {
  constexpr L Lhs() const {
    return {};
  }

  constexpr R Rhs() const {
    return {};
  }

  constexpr TypeList<L, R> List() const {
    return {};
  }
};

struct is_equation_t {
  template <typename Lhs, typename Rhs>
  constexpr bool operator()(equation<Lhs, Rhs>) const {
    return true;
  }

  template <typename T>
  constexpr bool operator()(T) const {
    return false;
  }
};

constexpr is_equation_t is_equation{};

template <int ID, typename T>
constexpr equation<T, inverse_variable<ID>> make_equation(baseexpr<T>) {
  return {};
}

template <typename Lhs, typename Rhs>
constexpr equation<Lhs, Rhs> make_equation(baseexpr<Lhs>, baseexpr<Rhs>) {
  return {};
}

template <typename Lhs, typename Rhs>
constexpr equation<Lhs, Rhs> make_equation(baseexpr<Lhs>, Rhs) {
  return {};
}

template <typename T>
constexpr auto make_equation_list(baseexpr<T>) {
  return List(make_equation<0>(T{}));
}

template <typename C, typename... Lhs, typename... Rhs>
constexpr auto make_equation_list(vectorexpr<C, Lhs...>, vectorexpr<C, Rhs...>) {
  return List(make_equation(Lhs{}, Rhs{})...);
}

template <typename... Lhs, typename... Rhs>
constexpr auto make_equation_list(TypeList<Lhs...>, TypeList<Rhs...>) {
  return List(make_equation(Lhs{}, Rhs{})...);
}

template <typename... Lhs, typename... Rhs>
constexpr auto make_vector_equation(TypeList<Lhs...>, TypeList<Rhs...>) {
  return make_equation(make_vector_expr(Lhs{}...), make_vector_expr(Rhs{}...));
}

template <typename K, typename V>
constexpr auto make_equation(TypeMap<K, V>) {
  return make_equation(make_vector_expr(K{}), make_vector_expr(V{}));
}

template <typename Lhs, typename Rhs>
constexpr auto make_equation(equation<Lhs, Rhs> eq) {
  return eq;
}

template <typename K, typename V>
constexpr auto make_equation_list(TypeMap<K, V>) {
  return make_equation_list(K{}, V{});
}

template <typename... Ts>
constexpr auto key_values(TypeList<Ts...>) {
  return List(Ts{}.List()...);
}

template <typename... Eqs>
constexpr auto make_type_map(TypeList<Eqs...>) {
  return make_type_map(List(Eqs{}.Lhs()...), List(Eqs{}.Rhs()...));
}

template <typename Lhs, typename Rhs>
constexpr auto make_type_map(equation<Lhs, Rhs>) {
  return make_type_map(List(equation<Lhs, Rhs>{}));
}

template <std::size_t... I>
constexpr auto make_inverse_variables(std::index_sequence<I...>) {
  return List(inverse_variable<I>{}...);
}

template <typename C, typename... Lhs>
constexpr auto make_equation_list(vectorexpr<C, Lhs...>) {
  return make_equation_list(
    vectorexpr<C, Lhs...>{}
    , make_vector_expr(make_inverse_variables(std::make_index_sequence<sizeof...(Lhs)>{}))
  );
}

template <typename Lhs, typename Rhs>
constexpr auto vars(equation<Lhs, Rhs>) {
  return MergeAll(vars(Lhs{}), vars(Rhs{}));
}

template <typename Var>
struct has_var_in_lhs {
template <typename Lhs, typename Rhs>
  constexpr bool operator()(equation<Lhs, Rhs>) const {
    return vars(Lhs{}).Has(Var{});
  }
};

template <typename Lhs, typename Rhs>
constexpr auto vars_excluding_conditionals(equation<Lhs, Rhs>) {
  return MergeAll(vars_excluding_conditionals(Lhs{}), vars_excluding_conditionals(Rhs{}));
}

template <typename T, typename Var>
constexpr zero_t num_occurrences(coordinate_system<T>, Var) {
  return {};
}

template <typename T, typename Var, EnableIf<!depends_on(T{}, Var{}) || is_leaf(T{})> = 0>
constexpr zero_t num_occurrences(baseexpr<T>, Var) {
  return {};
}

template <std::size_t I, typename Var>
constexpr zero_t num_occurrences(_size_t<I>, Var) {
  return {};
}

template <typename... Ts, typename Var>
constexpr auto num_occurrences(TypeList<Ts...>, Var) {
  return (... + num_occurrences(Ts{}, Var{}));
}

template <typename T, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
constexpr auto num_occurrences(baseexpr<T>, Var) {
  return num_occurrences(operands(T{}), Var{});
}

template <typename Var>
constexpr one_t num_occurrences(Var, Var) {
  return {};
}

template <typename Lhs, typename Rhs>
constexpr auto collect_constant_subexprs(equation<Lhs, Rhs>) {
  return collect_constant_subexprs(Lhs{}).Merge(collect_constant_subexprs(Rhs{}));
}

template <typename T>
constexpr auto decompose(baseexpr<T>) {
}

template <typename... Ts>
constexpr auto decompose(productexpr<Ts...>) {
  return create_equations(vectorexpr<cartesian_t, Ts...>{});
}

template <typename... Ts>
constexpr auto decompose_impl(vectorexpr<cartesian_t, Ts...>) {
  return create_equations(vectorexpr<cartesian_t, Ts...>{});
}

template <typename T>
constexpr auto var_subexpr_as_list(baseexpr<T>) {
  return List(T{});
}

template <typename T>
constexpr auto var_subexpr_as_list(varsubexpr<T>) {
  return concat(List(varsubexpr<T>{}), var_subexpr_as_list(T{}));
}
template <typename Lhs, typename Rhs>
constexpr auto collect_var_subexprs_recursive(equation<Lhs, Rhs>) {
  return collect_var_subexprs_recursive(Lhs{}).Merge(collect_var_subexprs_recursive(Rhs{}));
}


template <typename Lhs, typename Rhs, typename Map>
constexpr auto replace_from_map(equation<Lhs, Rhs>, Map) {
  return make_equation(replace_from_map(Lhs{}, Map{}), replace_from_map(Rhs{}, Map{}));
}


} // end namespace aether

#endif
