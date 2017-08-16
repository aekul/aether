#ifndef AETHER_DERIVATIVE_H
#define AETHER_DERIVATIVE_H

#include "Add.h"
#include "Literal.h"
#include "Param.h"
#include "Pow.h"
#include "Replace.h"
#include "Product.h"
#include "Trig.h"
#include "fwd/typelist/TypeList.h"
#include "Variable.h"
#include "Vector.h"

namespace aether {

template <typename A, typename Var>
constexpr auto dx(productexpr<A>, Var) {
  return dx(A{}, Var{});
}

template <typename A, typename B, typename... Ts, typename Var>
constexpr auto dx(productexpr<A, B, Ts...>, Var) {
  return dx(A{}, Var{}) * productexpr<B, Ts...>{} + A{} * dx(productexpr<B, Ts...>{}, Var{});
}

template <typename... Ts, typename Var>
constexpr auto dx(addexpr<Ts...>, Var) {
  return (... + dx(Ts{}, Var{}));
}

template <typename T, typename E, typename Var>
constexpr auto dx(powerexpr<T, E>, Var) {
  return E{} * pow(T{}, E{} - one) * dx(T{}, Var{});
}

template <typename T, typename Var>
constexpr auto dx(logexpr<T>, Var) {
  return dx(T{}, Var{}) / T{};
}

template <typename T, typename Var>
constexpr auto dx(expexpr<T>, Var) {
  return dx(T{}, Var{}) * expexpr<T>{};
}

template <typename T, typename Var>
constexpr auto dx(constantsubexpr<T> c, Var) {
  return make_zero(dimensions_of(c));
}

template <typename T, typename Var>
constexpr auto dx(sinexpr<T>, Var) {
  return cos(T{}) * dx(T{}, Var{});
}

template <typename T, typename Var>
constexpr auto dx(cosexpr<T>, Var) {
  return -sin(T{}) * dx(T{}, Var{});
}

template <std::intmax_t N, std::intmax_t D, typename Var>
constexpr auto dx(literal<N, D>, Var) {
  return zero;
}

template <int ID, int SeqIndex>
constexpr auto dx(variable<ID, SeqIndex>, variable<ID, SeqIndex>) {
  return one;
}

template <int ID, int SeqIndex, typename Var>
constexpr auto dx(variable<ID, SeqIndex>, Var) {
  return zero;
}

template <int ID, typename T>
constexpr auto dx(subexpr<ID, T>, subexpr<ID, T>) {
  return one;
}

template <int ID, typename T, typename Var>
constexpr auto dx(subexpr<ID, T>, Var) {
  return zero;
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex, typename Var>
constexpr auto dx(param<ID, N, I, SeqIndex>, Var) {
  return zero;
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex, typename Var>
constexpr auto dx(named_param<T, ID, N, I, SeqIndex>, Var) {
  return zero;
}

template <int ID, std::size_t N, std::size_t I, typename Var>
constexpr auto dx(valueexpr<ID, N, I>, Var) {
  return zero;
}

template <typename C, typename... Ts, typename Var>
constexpr auto dx(vectorexpr<C, Ts...>, Var) {
  return make_vector_expr(dx(Ts{}, Var{})...);
}

template <typename... Ts, typename Var>
constexpr auto dx(rotationmatrixexpr<Ts...>, Var) {
  return make_expr(rotation_matrix_tag, dx(Ts{}, Var{})...);
}

template <typename... Ts, typename Var>
constexpr auto dx(matrixexpr<Ts...>, Var) {
  return make_expr(matrix_tag, dx(Ts{}, Var{})...);
}

template <typename S, typename D, typename Var>
constexpr auto dx(dotproduct<S, D>, Var) {
  return dot(dx(S{}, Var{}), D{}) + dot(S{}, dx(D{}, Var{}));
}

template <typename S, typename D, typename Var>
constexpr auto dx(crossproduct<S, D>, Var) {
  return cross(dx(S{}, Var{}), D{}) + cross(S{}, dx(D{}, Var{}));
}

template <typename C, typename V, typename Var>
constexpr auto dx(branchcaseexpr<C, V>, Var) {
  return make_expr(branch_case_tag, C{}, dx(V{}, Var{})); 
}

template <typename... Ts, typename Var>
constexpr auto dx(branchexpr<Ts...>, Var) {
  return make_branch_expr(List(dx(Ts{}, Var{})...)); 
}

template <typename T, typename Var>
constexpr auto dx(varsubexpr<T>, Var) {
  return partialderivative<varsubexpr<T>, Var>{};
}

template <typename T, typename Var>
constexpr auto dx(vectorlength<T> v, Var) {
  return half * rcp(v) * dx(dot(T{}, T{}), Var{});
}

template <typename... Ts, typename Var>
constexpr auto dx(TypeSet<Ts...>, Var) {
  return List(dx(Ts{}, Var{})...); 
}


} // end namespace aether

#endif
