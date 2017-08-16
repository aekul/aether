#ifndef VECTOR_H
#define VECTOR_H

#include <utility>

#include "Expr.h"
#include "typelist/All.h"
#include "typelist/Sum.h"
#include "typelist/TypeList.h"
#include "typelist/Fill.h"
#include "aether/typelist/Pick.h"
#include "aether/typelist/Front.h"
#include <boost/hana.hpp>
#include "Variable.h"

namespace aether {

struct cartesian_t;
struct barycentric_t;
struct spherical_t;
struct polar_t {};


template <typename T>
constexpr T replace_constants_recursive(coordinate_system<T>) {
  return {};
}

template <typename T>
constexpr TypeSet<> collect_constant_subexprs(coordinate_system<T>) {
  return {};
}

template <typename T>
constexpr T replace_var_subexprs(coordinate_system<T>) {
  return {};
}

template <typename T>
constexpr TypeSet<> collect_var_subexprs_recursive_impl(coordinate_system<T>) {
  return {};
}

struct cartesian_t : coordinate_system<cartesian_t> {
  template <typename R, typename Theta, typename Phi>
  constexpr auto impl(vectorexpr<spherical_t, R, Theta, Phi>) const {
    return make_vector_expr(
      R{} * sin(Theta{}) * cos(Phi{})
      , R{} * sin(Theta{}) * sin(Phi{})
      , R{} * cos(Theta{})
    );
  }

  template <typename... Ts>
  constexpr auto operator()(vectorexpr<spherical_t, Ts...>) const {
    static_assert(sizeof...(Ts) == 3, "Spherical coordinates must have 3 dimensions.");
    return impl(vectorexpr<spherical_t, Ts...>{});
  }

  template <typename... Ts>
  constexpr vectorexpr<cartesian_t, Ts...> operator()(vectorexpr<cartesian_t, Ts...>) const {
    return {};
  }
};

constexpr cartesian_t cartesian{};

template <typename... Ts>
constexpr auto make_vector_expr(Ts... ts) {
  return make_expr(vector_tag, cartesian, ts...);
}

template <typename... Ts>
constexpr vectorexpr<cartesian_t, Ts...> make_vector_expr(TypeList<Ts...>) {
  return {};
}

template <typename A, typename B>
constexpr int size(dotproduct<A, B>) {
  return size(A{}) + size(B{}) + 1;
}

template <typename A, typename B>
constexpr int size(crossproduct<A, B>) {
  return size(A{}) + size(B{}) + 1;
}

template <typename C, typename... Ts>
constexpr int size(vectorexpr<C, Ts...>) {
  return (size(Ts{}) + ... + 0);
}

template <typename... Ts>
constexpr int size(matrixexpr<Ts...>) {
  return (size(Ts{}) + ... + 0);
}

template <typename... Ts>
constexpr auto make_matrix_expr(Ts... ts) {
  return make_expr(matrix_tag, ts...);
}

template <typename... Ts>
constexpr auto make_rotation_matrix_expr(Ts... ts) {
  return make_expr(rotation_matrix_tag, ts...);
}

template <typename... Ts>
constexpr matrixexpr<Ts...> make_matrix_expr(TypeList<Ts...>) {
  return {};
}

template <typename T>
constexpr auto determinant(constantsubexpr<T>) {
  return determinant(T{});
}

template <typename... Ts>
constexpr auto determinant(matrixexpr<Ts...> mat) {
  constexpr auto cols = operands(mat);
  constexpr auto m00 = at<0, 0>(cols);
  constexpr auto m10 = at<0, 1>(cols);
  constexpr auto m20 = at<0, 2>(cols);

  constexpr auto m01 = at<1, 0>(cols);
  constexpr auto m11 = at<1, 1>(cols);
  constexpr auto m21 = at<1, 2>(cols);

  constexpr auto m02 = at<2, 0>(cols);
  constexpr auto m12 = at<2, 1>(cols);
  constexpr auto m22 = at<2, 2>(cols);

  return (m00 * m11 * m22)
    + (m01 * m12 * m20) 
    + (m02 * m10 * m21) 
    - (m02 * m11 * m20) 
    - (m01 * m10 * m22) 
    - (m00 * m12 * m21);
}

template <std::size_t R>
constexpr auto make_one(dimensions<R, 1>) {
  return make_vector_expr(fill(_size_t<R>{}, one));
}

template <std::size_t R>
constexpr auto make_zero(dimensions<R, 1>) {
  return make_vector_expr(fill(_size_t<R>{}, zero));
}

template <std::size_t R, std::size_t C>
constexpr auto make_zero(dimensions<R, C>) {
  return make_matrix_expr(fill(_size_t<C>{}, make_zero(dimensions<R, 1>{})));
}



struct is_zero_t {
  constexpr bool operator()(zero_t) const {
    return true;
  }

  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }

  template <typename T>
  constexpr bool operator()(constantsubexpr<T>) const {
    return this->operator()(T{});
  }

  template <typename C, typename... Ts>
  constexpr bool operator()(vectorexpr<C, Ts...>) const {
    return (this->operator()(Ts{}) && ...);
  }

  template <typename... Ts>
  constexpr bool operator()(matrixexpr<Ts...>) const {
    return (this->operator()(Ts{}) && ...);
  }

  template <typename... Ts>
  constexpr bool operator()(rotationmatrixexpr<Ts...>) const {
    return (this->operator()(Ts{}) && ...);
  }
};

constexpr is_zero_t is_zero{};

struct is_zero_vector_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }

  template <typename C, typename... Ts>
  constexpr bool operator()(vectorexpr<C, Ts...>) const {
    return (is_zero(Ts{}) && ...);
  }
};

constexpr is_zero_vector_t is_zero_vector{};

struct is_zero_matrix_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }

  template <typename... Ts>
  constexpr bool operator()(matrixexpr<Ts...>) const {
    return (is_zero_vector(Ts{}) && ...);
  }
};

constexpr is_zero_matrix_t is_zero_matrix{};


template <typename... Ts>
constexpr auto vars(vectorexpr<cartesian_t, Ts...>) {
  return MergeAll(vars(Ts{})...);
}

template <typename T>
constexpr auto vars_excluding_conditionals(coordinate_system<T>) {
  return make_type_set();
}

template <typename... Ts>
constexpr auto vars(TypeList<Ts...>) {
  return MergeAll(vars(Ts{})...);
}

template <typename... Ts>
constexpr auto inverse_vars(vectorexpr<cartesian_t, Ts...>) {
  return MergeAll(inverse_vars(Ts{})...);
}

template <typename... Ts>
constexpr auto params(vectorexpr<cartesian_t, Ts...>) {
  return MergeAll(params(Ts{})...);
}

template <typename T>
constexpr auto named_params(coordinate_system<T>) {
  return make_type_set();
}


template <std::size_t N>
constexpr auto make_vector_zeros() {
  return make_vector_expr(fill(_size_t<N>{}, zero));
}


template <typename C, typename... Ts>
constexpr auto make_vector_expr_with_cs(coordinate_system<C>, TypeList<Ts...>) {
  return make_vector_expr<C>(TypeList<Ts...>{});
}

template <typename... Ts>
constexpr TypeList<Ts...> sort_coordinates(TypeList<Ts...>) {
  return {};
}

template <typename V, typename U>
constexpr TypeList<TypeList<u_t, U>, TypeList<v_t, V>> sort_coordinates(TypeList<TypeList<v_t, V>, TypeList<u_t, U>>) {
  return {};
}

template <typename R, typename Phi, typename Theta>
constexpr TypeList<TypeList<radius_t, R>, TypeList<theta_t, Theta>, TypeList<phi_t, Phi>> sort_coordinates(TypeList<TypeList<radius_t, R>, TypeList<phi_t, Phi>, TypeList<theta_t, Theta>>) {
  return {};
}

struct spherical_t : coordinate_system<spherical_t> {
  template <typename R, typename Theta, typename Phi>
  constexpr auto operator()(TypeList<radius_t, R>, TypeList<theta_t, Theta>, TypeList<phi_t, Phi>) {
    return make_vector_expr<spherical_t>(R{}, Theta{}, Phi{});
  }
};

constexpr spherical_t spherical{};


struct barycentric_t : coordinate_system<barycentric_t> {
};

constexpr barycentric_t barycentric{};

template <typename U, typename V>
constexpr auto infer_coordinate_system(TypeList<TypeList<u_t, U>, TypeList<v_t, V>>) {
  return barycentric;
}

template <typename V, typename U>
constexpr auto infer_coordinate_system(TypeList<TypeList<v_t, V>, TypeList<u_t, U>>) {
  return barycentric;
}

template <typename R, typename Theta, typename Phi>
constexpr auto infer_coordinate_system(TypeList<TypeList<radius_t, R>, TypeList<theta_t, Theta>, TypeList<phi_t, Phi>>) {
  return spherical;
}


template <typename... Rs, typename... Ts, typename... Vs, EnableIf<condition_list(branchexpr<Rs...>{}) == condition_list(branchexpr<Ts...>{}) && condition_list(branchexpr<Rs...>{}) == condition_list(branchexpr<Vs...>{})> = 0>
constexpr auto make_vector_expr(branchexpr<Rs...>, branchexpr<Ts...>, branchexpr<Vs...>) {
  return make_branch_expr(List(make_expr(branch_case_tag, get_condition(Rs{}), make_vector_expr(get_value(Rs{}), get_value(Ts{}), get_value(Vs{})))...));
}

template <std::size_t I, typename... Ts>
constexpr auto col(matrixexpr<Ts...>) {
  return at(_size_t<I>{}, TypeList<Ts...>{});
}

template <std::size_t I, typename T, EnableIf<is_matrix(T{})> = 0>
constexpr auto col(constantsubexpr<T>) {
  return col<I>(T{});
}

template <std::size_t I, typename T>
constexpr auto at(constantsubexpr<T>) {
  return at<I>(T{});
}

template <std::size_t I, typename... Rs, typename... Ts>
constexpr auto at(addexpr<constantsubexpr<vectorexpr<cartesian_t, Rs...>>, vectorexpr<cartesian_t, Ts...>>) {
  return at(_size_t<I>{}, TypeList<Rs...>{}) + at(_size_t<I>{}, TypeList<Ts...>{});
}

template <std::size_t I, typename... Ts>
constexpr auto at(vectorexpr<cartesian_t, Ts...>) {
  return at(_size_t<I>{}, TypeList<Ts...>{});
}

template <std::size_t I, typename C, typename... Ts, EnableIf<is_scalarexpr(C{})> = 0>
constexpr auto at(productexpr<C, vectorexpr<cartesian_t, Ts...>>) {
  return C{} * at<I>(vectorexpr<cartesian_t, Ts...>{});
}

template <std::size_t I1, std::size_t I2, typename... Ts>
constexpr auto at(vectorexpr<cartesian_t, Ts...>) {
  return at<I2>(at(_size_t<I1>{}, TypeList<Ts...>{}));
}

template <std::size_t I, typename T, EnableIf<(I == 0)> = 0>
constexpr auto at(expr<T>) {
  return T{};
}


template <std::size_t I, typename... Ts>
constexpr auto col(vectorexpr<cartesian_t, Ts...>) {
  return make_vector_expr(at<I>(Ts{})...);
}

template <typename C, typename... Ts>
constexpr TypeList<Ts...> as_list(vectorexpr<C, Ts...>) {
  return {};
}

template <typename T>
constexpr T transpose(expr<T>) {
  return {};
}



template <std::size_t I, typename... Ts>
constexpr auto row(matrixexpr<Ts...>) {
  return make_vector_expr(at<I>(Ts{})...);
}

template <typename... Ts, std::size_t... Is>
constexpr auto rows_impl(matrixexpr<Ts...> m, std::index_sequence<Is...>) {
  return List(row<Is>(m)...);
}

template <typename... Ts, EnableIf<all(is_vectorexpr, List(Ts{}...))> = 0>
constexpr auto rows(matrixexpr<Ts...> m) {
  return rows_impl(m, std::make_index_sequence<dimensions_of(matrixexpr<Ts...>{}).Rows()>{});
}

template <typename... Ts, std::size_t... Is>
constexpr auto transpose(matrixexpr<Ts...>, std::index_sequence<Is...>) {
  return make_expr(matrix_tag, row<Is>(matrixexpr<Ts...>{})...);
}

template <typename... Ts>
constexpr auto transpose(matrixexpr<Ts...>) {
  return transpose(matrixexpr<Ts...>{}, std::make_index_sequence<dimensions_of(at<0>(List(Ts{}...))).Rows()>{});
}

template <typename T>
constexpr auto inverse(baseexpr<T>) {
  static_assert(is_matrix(T{}), "inverse() requires a matrix argument.");
  return make_expr(matrix_inverse_tag, T{});
}


template <typename A, typename B>
constexpr auto cross_impl(baseexpr<A>, baseexpr<B>) {
  static_assert(dimensions_match(cross_product_tag, A{}, B{}), "cross_impl() requires vectors of the same length.");
  constexpr A a{};
  constexpr B b{};
  return make_vector_expr(
    at<1>(a) * at<2>(b) - at<2>(a) * at<1>(b),
    at<2>(a) * at<0>(b) - at<0>(a) * at<2>(b),
    at<0>(a) * at<1>(b) - at<1>(a) * at<0>(b)
  );
}

template <typename A, typename B>
constexpr auto dot_impl(baseexpr<A>, baseexpr<B>) {
  static_assert(dimensions_match(dot_product_tag, A{}, B{}), "dot_impl() requires vectors of the same length.");
  constexpr A a{};
  constexpr B b{};
  return at<0>(a) * at<0>(b) + at<1>(a) * at<1>(b) + at<2>(a) * at<2>(b);
}

template <typename A, typename B, EnableIf<is_zero(A{}) || is_zero(B{})> = 0>
constexpr auto dot(baseexpr<A>, baseexpr<B>) {
  static_assert(dimensions_match(dot_product_tag, A{}, B{}), "dot() requires vectors of the same length.");
  return zero;
}

template <typename... Ts, typename B, EnableIf<!is_zero(B{})> = 0>
constexpr auto dot(balanceexpr<branchexpr<Ts...>> br, baseexpr<B>) {
  static_assert(dimensions_match(dot_product_tag, br, B{}), "dot() requires vectors of the same length.");

  return make_balance_expr(make_branch_expr(List(get_condition(Ts{})...), List(dot(get_value(Ts{}), B{})...)));
}

template <typename A, typename... Ts, EnableIf<!is_zero(A{})> = 0>
constexpr auto dot(baseexpr<A>, balanceexpr<branchexpr<Ts...>> br) {
  static_assert(dimensions_match(dot_product_tag, br, A{}), "dot() requires vectors of the same length.");

  return make_balance_expr(make_branch_expr(List(get_condition(Ts{})...), List(dot(A{}, get_value(Ts{}))...)));
}

template <typename... Ts, typename B, EnableIf<!is_zero(B{})> = 0>
constexpr auto dot(branchexpr<Ts...> br, baseexpr<B>) {
  static_assert(dimensions_match(dot_product_tag, br, B{}), "dot() requires vectors of the same length.");

  return make_branch_expr(List(get_condition(Ts{})...), List(dot(get_value(Ts{}), B{})...));
}

template <typename A, typename... Ts, EnableIf<!is_zero(A{})> = 0>
constexpr auto dot(baseexpr<A>, branchexpr<Ts...> br) {
  static_assert(dimensions_match(dot_product_tag, br, A{}), "dot() requires vectors of the same length.");

  return make_branch_expr(List(get_condition(Ts{})...), List(dot(A{}, get_value(Ts{}))...));
}

template <typename A, typename B, EnableIf<!is_zero(A{}) && !is_zero(B{})> = 0>
constexpr auto dot(baseexpr<A>, baseexpr<B>) {
  static_assert(dimensions_match(dot_product_tag, A{}, B{}), "dot() requires vectors of the same length.");
  return make_expr(dot_product_tag, A{}, B{});
}

template <typename A, typename B, EnableIf<is_zero_vector(A{}) || is_zero_vector(B{})> = 0>
constexpr auto cross(baseexpr<A>, baseexpr<B>) {
  static_assert(dimensions_match(cross_product_tag, A{}, B{}), "cross() requires vectors of the same length.");
  return make_zero(dimensions_of(A{}));
}

template <typename A, typename B, EnableIf<!is_zero_vector(A{}) && !is_zero_vector(B{})> = 0>
constexpr auto cross(baseexpr<A>, baseexpr<B>) {
  static_assert(dimensions_match(cross_product_tag, A{}, B{}), "cross() requires vectors of the same length.");
  return make_expr(cross_product_tag, A{}, B{});
}

template <typename... Ts, typename... Vs>
constexpr auto join(vectorexpr<cartesian_t, Ts...>, vectorexpr<cartesian_t, Vs...>) {
  return make_vector_expr(Ts{}..., Vs{}...);
}

template <typename... Ts, typename V>
constexpr auto add_to_end(vectorexpr<cartesian_t, Ts...>, V) {
  return make_vector_expr(Ts{}..., V{});
}

template <typename... Ts>
constexpr auto length(constantsubexpr<branchexpr<Ts...>> br) {
  static_assert(is_vector(br), "length() can only be called on vectors.");

  return make_branch_expr(List(make_expr(branch_case_tag, get_condition(Ts{}), length(get_value(Ts{})))...));
}

template <typename... Ts>
constexpr auto length(branchexpr<Ts...> br) {
  static_assert(is_vector(br), "length() can only be called on vectors.");

  return make_branch_expr(List(make_expr(branch_case_tag, get_condition(Ts{}), length(get_value(Ts{})))...));
}

template <typename T>
constexpr auto length(baseexpr<T>) {
  static_assert(is_vector(T{}), "length() can only be called on vectors.");
  return make_expr(vector_length_tag, T{});
}

template <typename T>
constexpr auto normalize(baseexpr<T>) {
  static_assert(is_vector(T{}), "normalize() can only be called on vectors.");
  return T{} / length(T{});
}

} // end namespace aether

#endif
