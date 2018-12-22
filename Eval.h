#ifndef AETHER_EVAL_H
#define AETHER_EVAL_H

#include <cmath>

#include "aether/EvalEigen.h"
#include "Add.h"
#include "Literal.h"
#include "Param.h"
#include "Derivative.h"
#include "Pow.h"
#include "Replace.h"
#include "Product.h"
#include "Trig.h"
#include <boost/hana.hpp>
#include "typelist/All.h"
#include "fwd/typelist/TypeList.h"
#include "Variable.h"
#include "Vector.h"


namespace aether {

using namespace boost;

//static bool debug = true;
static bool debug = false;

template <typename C, typename... Ts>
constexpr auto dependent_variables(vectorexpr<C, Ts...>) {
  return MergeAll(dependent_variables(Ts{})...);
}

template <typename... Ts>
constexpr auto dependent_variables(productexpr<Ts...>) {
  return MergeAll(dependent_variables(Ts{})...);
}

template <typename... Ts>
constexpr auto dependent_variables(addexpr<Ts...>) {
  return MergeAll(dependent_variables(Ts{})...);
}

template <typename T>
constexpr auto dependent_variables(sinexpr<T>) {
  return dependent_variables(T{});
}

template <typename T>
constexpr auto dependent_variables(cosexpr<T>) {
  return dependent_variables(T{});
}

template <typename T, typename E>
constexpr auto dependent_variables(powerexpr<T, E>) {
  return MergeAll(dependent_variables(T{}), dependent_variables(E{}));
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto dependent_variables(literal<N, D>) {
  return make_type_set();
}

template <int ID, int SeqIndex>
constexpr auto dependent_variables(variable<ID, SeqIndex>) {
  return make_type_set();
}

template <int ID, typename T>
constexpr auto dependent_variables(subexpr<ID, T>) {
  return make_type_set(subexpr<ID, T>{});
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr auto dependent_variables(param<ID, N, I, SeqIndex>) {
  return make_type_set();
}




template <typename T, typename... Vars>
constexpr auto partials(baseexpr<T>, TypeList<Vars...>) {
  return List(dx(T{}, Vars{})...);
}

template <typename T, int ID, int SeqIndex> 
constexpr auto partials(baseexpr<T>, variable<ID, SeqIndex> var) {
  return dx(T{}, var);
}


template <typename C, typename... Ts, typename... Vs>
constexpr auto jacobian_impl(vectorexpr<C, Ts...>, TypeList<Vs...>) {
  return make_expr(matrix_tag, partials(vectorexpr<C, Ts...>{}, Vs{})...);
}

template <typename T, typename... Vs>
constexpr auto jacobian_impl(baseexpr<T>, TypeList<Vs...>) {
  return make_expr(matrix_tag, partials(T{}, Vs{})...);
}

template <typename C, typename... Ts>
constexpr auto jacobian(vectorexpr<C, Ts...>) {
  return jacobian_impl(vectorexpr<C, Ts...>{}, make_list(vars(vectorexpr<C, Ts...>{})));
}

template <typename T>
constexpr auto jacobian(baseexpr<T>) {
  return jacobian_impl(T{}, make_list(vars(T{})));
}

template <typename C, typename... Ts, EnableIf<sizeof...(Ts) == 2 && !(is_vector(Ts{}) && ... && true)> = 0>
constexpr auto determinant_impl(vectorexpr<C, Ts...>) {
  constexpr auto jac = jacobian(vectorexpr<C, Ts...>{});
  return at<0, 0>(jac) * at<1, 1>(jac) - at<0, 1>(jac) * at<1, 0>(jac);
}

template <typename C, typename... Ts, EnableIf<sizeof...(Ts) == 3 && vars(vectorexpr<C, Ts...>{}).Size() == 3> = 0>
constexpr auto determinant_impl(vectorexpr<C, Ts...>) {
  constexpr auto jac = jacobian(vectorexpr<C, Ts...>{});
  return at<0, 0>(jac) * (at<1, 1>(jac) * at<2, 2>(jac) - at<1, 2>(jac) * at<2, 1>(jac))
    - at<0, 1>(jac) * (at<1, 0>(jac) * at<2, 2>(jac) - at<1, 2>(jac) * at<2, 0>(jac))
    + at<0, 2>(jac) * (at<1, 0>(jac) * at<2, 1>(jac) - at<1, 1>(jac) * at<2, 0>(jac));
}

template <typename C, typename... Ts, EnableIf<sizeof...(Ts) == 3 && vars(vectorexpr<C, Ts...>{}).Size() == 2> = 0>
constexpr auto determinant_impl(vectorexpr<C, Ts...>) {
  constexpr auto jac = jacobian(vectorexpr<C, Ts...>{});
  constexpr auto A = col<0>(jac);
  constexpr auto B = col<1>(jac);
  constexpr auto AA = dot_impl(A, A);
  constexpr auto AB = dot_impl(A, B);
  constexpr auto BB = dot_impl(B, B);
  return sqrt(AA * BB - AB * AB);
}

template <typename C, typename V, EnableIf<is_constant(C{}) && is_vector(V{})> = 0>
constexpr auto determinant_impl(productexpr<C, V>) {
  return make_expr(determinant_tag, C{}) * determinant_impl(V{});
}

template <typename C, typename P, typename N, typename... Ms, typename V, EnableIf<is_constant(C{}) && is_vector(V{})> = 0>
constexpr auto determinant_impl(addexpr<C, productexpr<constantsubexpr<dotproduct<P, N>>, powerexpr<dotproduct<productexpr<constantsubexpr<rotationmatrixexpr<Ms...>>, V>, N>, minus_one_t>, constantsubexpr<rotationmatrixexpr<Ms...>>, V>>) {
  constexpr auto t = dotproduct<P, N>{} / dotproduct<productexpr<rotationmatrixexpr<Ms...>, V>, N>{};
  constexpr auto ct = dot(productexpr<rotationmatrixexpr<Ms...>, V>{}, normalize(N{}));
  return (abs(ct) * determinant_impl(V{})) / (t * t);
}

template <typename C, typename P, typename N, typename... Ms, typename V, EnableIf<is_constant(C{}) && is_vector(V{})> = 0>
constexpr auto determinant_impl(addexpr<C, productexpr<constantsubexpr<dotproduct<P, N>>, powerexpr<dotproduct<V, N>, minus_one_t>, V>>) {
  constexpr auto t = dotproduct<P, N>{} / dotproduct<V, N>{};
  constexpr auto ct = dot(V{}, normalize(N{}));
  return (abs(ct) * rcp(det(V{}))) / (t * t);
}

template <typename... Ts, EnableIf<all_same(List(determinant_impl(get_value(Ts{}))...))> = 0>
constexpr auto determinant_impl(branchexpr<Ts...>) {
  return determinant_impl(get_value(front(TypeList<Ts...>{})));
}

template <typename C, typename... Ts, EnableIf<is_constant(C{})> = 0>
constexpr auto determinant_impl(addexpr<C, branchexpr<Ts...>>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(determinant_impl(C{} + get_value(Ts{}))...));
}

template <typename... Ts, EnableIf<all(is_constant, List(get_condition(Ts{})...)) && !all_same(List(get_value(Ts{})...))> = 0>
constexpr auto jacobian(branchexpr<Ts...>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(jacobian(get_value(Ts{}))...));
}


template <typename A, typename E = void_t<>>
struct has_symbolic_determinant_t : std::false_type {
};

template <typename A>
struct has_symbolic_determinant_t<A, void_t<decltype(determinant_impl(A{}))>> : std::true_type {
};

template <typename A, EnableIf<is_scalarexpr(A{}) && (vars(A{}).Size() <= 1)> = 0>
constexpr bool has_symbolic_determinant(baseexpr<A>) {
  return true;
}

template <typename A, EnableIf<!is_scalarexpr(A{}) || (vars(A{}).Size() != 1)> = 0>
constexpr bool has_symbolic_determinant(baseexpr<A>) {
  return has_symbolic_determinant_t<A>::value;
}






template <typename... Ts>
constexpr auto denominator(TypeList<Ts...>) {
  return product_list(List(denominator(Ts{})...));
}

template <typename... Ts>
constexpr auto denominator(addexpr<Ts...>) {
  // TODO: fix this
  return one;
}

template <typename... Ts>
constexpr auto denominator(productexpr<Ts...>) {
  return denominator(TypeList<Ts...>{});
}

template <typename B, std::intmax_t N, std::intmax_t D, EnableIf<is_negative(literal<N, D>{})> = 0>
constexpr auto denominator(powerexpr<B, literal<N, D>>) {
  return pow(B{}, -literal<N, D>{});
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto denominator(literal<N, D>) {
  return literal<D, 1>{};
}

template <typename T>
constexpr auto denominator(baseexpr<T>) {
  return one;
}



constexpr auto numerator(TypeList<>) {
  return one;
}

template <typename T, typename... Ts>
constexpr auto numerator(TypeList<T, Ts...>) {
  return numerator(T{}) * numerator(TypeList<Ts...>{});
}

template <typename... Ts>
constexpr auto numerator(productexpr<Ts...>) {
  return numerator(TypeList<Ts...>{});
}

template <typename B, std::intmax_t N, std::intmax_t D, EnableIf<is_negative(literal<N, D>{})> = 0>
constexpr auto numerator(powerexpr<B, literal<N, D>>) {
  return one;
}

template <std::intmax_t N, std::intmax_t D>
constexpr auto numerator(literal<N, D>) {
  return literal<N, 1>{};
}

template <typename T>
constexpr T numerator(baseexpr<T>) {
  return {};
}






template <typename T, typename Map, EnableIf<is_leaf(T{})> = 0>
constexpr T replace_subexprs_with_vars_det_impl(baseexpr<T>, Map) {
  return {};
}

template <typename T, typename Map>
constexpr T replace_subexprs_with_vars_det_impl(coordinate_system<T>, Map) {
  return {};
}

template <std::size_t I, typename Map>
constexpr _size_t<I> replace_subexprs_with_vars_det_impl(_size_t<I>, Map) {
  return {};
}

template <typename T, typename Map, EnableIf<!is_leaf(T{})> = 0>
constexpr auto replace_subexprs_with_vars_det_impl(baseexpr<T>, Map) {
  return make_expr(tag_of(T{}), replace_subexprs_with_vars_det_impl(operands(T{}), Map{}));
}

template <typename T, typename Map, EnableIf<Map{}.HasKey(T{})> = 0>
constexpr auto replace_subexprs_with_vars_det_impl(varsubexpr<T>, Map) {
  return Map{}.Get(T{});
}

template <typename T, typename Map, EnableIf<!Map{}.HasKey(T{})> = 0>
constexpr auto replace_subexprs_with_vars_det_impl(varsubexpr<T>, Map) {
  return varsubexpr<T>{};
}

template <typename... Ts, typename Map>
constexpr auto replace_subexprs_with_vars_det_impl(TypeList<Ts...>, Map) {
  return List(replace_subexprs_with_vars_det_impl(Ts{}, Map{})...);
}

template <typename... Ts, typename Map>
constexpr auto replace_subexprs_with_vars_det(TypeList<Ts...>, Map) {
  return List(replace_subexprs_with_vars_det_impl(Ts{}, Map{})...);
}




template <typename... Ts, EnableIf<all_same(List(det(get_value(Ts{}))...))> = 0>
constexpr auto det(branchexpr<Ts...>) {
  return det(get_value(front(TypeList<Ts...>{})));
}

template <typename... Ts>
constexpr auto det(constantsubexpr<rotationmatrixexpr<Ts...>>) {
  return one;
}

template <typename... Ts>
constexpr auto det(rotationmatrixexpr<Ts...>) {
  return one;
}

template <typename C, typename... Ts, EnableIf<sizeof...(Ts) == 3 && vars(vectorexpr<C, Ts...>{}).Size() == 2> = 0>
constexpr auto det(vectorexpr<C, Ts...>) {
  constexpr auto jac = jacobian(vectorexpr<C, Ts...>{});
  constexpr auto A = col<0>(jac);
  constexpr auto B = col<1>(jac);
  constexpr auto AA = dot_impl(A, A);
  constexpr auto AB = dot_impl(A, B);
  constexpr auto BB = dot_impl(B, B);
  return abs(rcp(sqrt(AA * BB - AB * AB)));
}

template <typename C, typename... Ts, EnableIf<(sizeof...(Ts) < vars(vectorexpr<C, Ts...>{}).Size())> = 0>
constexpr auto det(vectorexpr<C, Ts...>) {
  return zero;
}

template <typename C, typename V, EnableIf<is_constant(C{}) && is_vector(V{})> = 0>
constexpr auto det(productexpr<C, V>) {
  return det(C{}) * det(V{});
}

template <typename T>
constexpr auto det(constantsubexpr<indicatorexpr<T>>) {
  return dirac(T{});
}

template <typename T>
constexpr auto det(indicatorexpr<T>) {
  return dirac(T{});
}

template <typename T, EnableIf<is_constant(T{})> = 0>
constexpr auto det(baseexpr<T>) {
  return one;
}

template <typename T, EnableIf<is_scalarexpr(T{}) && (vars(T{}).Size() > 1)> = 0>
constexpr auto det(baseexpr<T>) {
  return zero;
}

template <typename T, EnableIf<is_scalarexpr(T{}) && (vars(T{}).Size() == 1)> = 0>
constexpr auto det(baseexpr<T>) {
  return abs(rcp(dx(T{}, at<0>(vars(T{})))));
}

template <typename C, typename P, typename N, typename... Ms, typename V, EnableIf<is_constant(C{}) && is_vector(V{})> = 0>
constexpr auto det(addexpr<C, productexpr<constantsubexpr<dotproduct<P, N>>, powerexpr<dotproduct<productexpr<constantsubexpr<rotationmatrixexpr<Ms...>>, V>, N>, minus_one_t>, constantsubexpr<rotationmatrixexpr<Ms...>>, V>>) {
  constexpr auto t = dotproduct<P, N>{} / dotproduct<productexpr<rotationmatrixexpr<Ms...>, V>, N>{};
  constexpr auto ct = dot(productexpr<rotationmatrixexpr<Ms...>, V>{}, normalize(N{}));
  return (abs(ct) * det(V{})) / (t * t);
}

template <typename C, typename P, typename N, typename... Ms, typename V, EnableIf<is_constant(C{}) && is_vector(V{})> = 0>
constexpr auto det(addexpr<C, productexpr<constantsubexpr<dotproduct<P, N>>, powerexpr<dotproduct<V, N>, minus_one_t>, V>>) {
  constexpr auto t = dotproduct<P, N>{} / dotproduct<V, N>{};
  constexpr auto ct = dot(V{}, normalize(N{}));
  return (abs(ct) * det(V{})) / (t * t);
}

template <typename T, std::size_t... Is>
constexpr auto flatten_det(baseexpr<T>, std::index_sequence<Is...>) {
  return List(at<Is>(T{})...);
}

//template <typename C, typename R, typename T, EnableIf<(is_vector(R{}) && is_vector(T{}))> = 0>
//constexpr auto det2(vectorexpr<C, R, T>) {
  //constexpr auto v = vars(vectorexpr<C, R, T>{});
  ////Ty<decltype(det(R{}))> S{};
  ////Ty<_size_t<dimensions_of(jacobian(R{})).Rows()>> s{};
  ////Ty<_size_t<dimensions_of(jacobian(R{})).Cols()>> ss{};
  ////Ty<_size_t<operands(jacobian(R{})).Size()>> sokjs{};
  ////Ty<_size_t<operands(jacobian(R{})).Size()>> sks{};
  //return jacobian(vectorexpr<C, R, T>{});
  ////det(T{})
  ////Ty<bool_t<is_vectorexpr(R{})>> s{};
  ////Ty<bool_t<is_vectorexpr(T{})>> ss{};
  ////return det(vectorexpr<C, Rs..., Ts...>{});
//}

template <typename T, EnableIf<!is_scalarexpr(T{}) && !is_constant(T{})> = 0>
constexpr auto det(baseexpr<T>) {
  return abs(rcp(make_expr(determinant_tag, jacobian(T{}))));
}

template <typename... Ts, EnableIf<all(is_constant, List(get_condition(Ts{})...)) && !all_same(List(det(get_value(Ts{}))...))> = 0>
constexpr auto det(branchexpr<Ts...>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(det(get_value(Ts{}))...));
}

template <typename C, typename... Ts, EnableIf<is_constant(C{})> = 0>
constexpr auto det(addexpr<C, branchexpr<Ts...>>) {
  return make_branch_expr(List(get_condition(Ts{})...), List(det(C{} + get_value(Ts{}))...));
}


template <typename T, typename... Vs, EnableIf<!has_symbolic_determinant(T{})> = 0>
auto determinant(baseexpr<T>, const Map<Vs...>& values) {
  constexpr auto jac = jacobian(T{});
  return rcp(abs(det(evaluate(jac, values))));
}

template <typename T, typename... Vs, EnableIf<has_symbolic_determinant(T{})> = 0>
auto determinant(baseexpr<T>, const Map<Vs...>& values) {
  constexpr auto d = det(T{});
  return evaluate(d, values);
}



} // end namespace aether

#endif
