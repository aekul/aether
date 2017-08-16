#ifndef AETHER_PRINT_HTML_H
#define AETHER_PRINT_HTML_H

#include <cstdio>
#include <fstream>
#include <iostream>
#include <string>
#include <typeinfo>

#include "Add.h"
#include "typelist/All.h"
#include "Equation.h"
#include "fwd/RandomVar.h"
#include "Literal.h"

namespace aether {

struct print_html_t {
  template <int ID, int SeqIndex, typename... Vs, typename... Cs, typename M, EnableIf<!has<variable<ID, SeqIndex>>(M{})> = 0>
  inline std::string print(variable<ID, SeqIndex>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "u_(" + std::to_string(ID) + ", " + std::to_string(SeqIndex) + ")";
  }

  template <int ID, int SeqIndex, typename... Vs, typename... Cs, typename M, EnableIf<has<variable<ID, SeqIndex>>(M{})> = 0>
  inline std::string print(variable<ID, SeqIndex>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "u_(" + std::to_string(ID) + ", " + std::to_string(SeqIndex) + ") (" + std::to_string(get<variable<ID, SeqIndex>>(m)) + ")";
  }

  template <typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(literal<491701844, 78256779>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "2pi";
  }

  template <typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(literal<245850922, 78256779>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "pi";
  }

  template <std::intmax_t N, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(literal<N, 1>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return std::to_string(N);
  }

  template <std::intmax_t N, std::intmax_t D, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(literal<N, D>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return std::to_string(N) + "/" + std::to_string(D) + " ";
  }

  template <int ID, std::size_t N, std::size_t I, int SeqIndex, typename... Vs, typename... Cs, typename M, EnableIf<!has<param<ID, N, I, SeqIndex>>(M{})> = 0>
  inline std::string print(param<ID, N, I, SeqIndex>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "p_(" + std::to_string(ID) + ", " + std::to_string(N) + ", " + std::to_string(I) + ", " + std::to_string(SeqIndex) + ")";
  }

  template <int ID, std::size_t N, std::size_t I, int SeqIndex, typename... Vs, typename... Cs, typename M, EnableIf<has<param<ID, N, I, SeqIndex>>(M{})> = 0>
  inline std::string print(param<ID, N, I, SeqIndex>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "p_(" + std::to_string(ID) + ", " + std::to_string(N) + ", " + std::to_string(I) + ", " + std::to_string(SeqIndex) + ")" + " (" + std::to_string(get<param<ID, N, I, SeqIndex>>(m)) + ": " + std::to_string(valid<param<ID, N, I, SeqIndex>>(m)) + ")";
  }

  template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex, typename... Vs, typename... Cs, typename M, EnableIf<!has<named_param<T, ID, N, I, SeqIndex>>(M{})> = 0>
  inline std::string print(named_param<T, ID, N, I, SeqIndex>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "np_(" + std::to_string(N) + ", " + std::to_string(ID) + ", " + std::to_string(I) + ", " + std::to_string(SeqIndex) + ")";
  }

  template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex, typename... Vs, typename... Cs, typename M, EnableIf<has<named_param<T, ID, N, I, SeqIndex>>(M{})> = 0>
  inline std::string print(named_param<T, ID, N, I, SeqIndex>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "np_(" + std::to_string(N) + ", " + std::to_string(ID) + ", " + std::to_string(I) + ", " + std::to_string(SeqIndex) + ")" + " (" + std::to_string(get<named_param<T, ID, N, I, SeqIndex>>(m)) + ": " + std::to_string(valid<named_param<T, ID, N, I, SeqIndex>>(m)) + ")";
  }

  template <int ID, std::size_t N, std::size_t I, typename... Vs, typename... Cs, typename M, EnableIf<!has<valueexpr<ID, N, I>>(M{})> = 0>
  inline std::string print(valueexpr<ID, N, I>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "a_(" + std::to_string(ID) + ", " + std::to_string(N) + ", " + std::to_string(I) + ")";
  }

  template <int ID, std::size_t N, std::size_t I, typename... Vs, typename... Cs, typename M, EnableIf<has<valueexpr<ID, N, I>>(M{})> = 0>
  inline std::string print(valueexpr<ID, N, I>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "a_(" + std::to_string(ID) + ", " + std::to_string(N) + ", " + std::to_string(I) + ")" + " (" + std::to_string(get<valueexpr<ID, N, I>>(m)) + ": " + std::to_string(valid<valueexpr<ID, N, I>>(m)) + ")";
  }

  template <typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print_list(TypeList<>, const std::string&, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m, const std::string& l = "", const std::string& r = "") {
    return "";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print_list(TypeList<T>, const std::string&, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m, const std::string& l = "", const std::string& r = "") {
    return l + print(T{}, vse, cse, m) + r;
  }

  template <typename A, typename B, typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print_list(TypeList<A, B, Ts...>, const std::string& op, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m, const std::string& l = "", const std::string& r = "") {
    return l + print(A{}, vse, cse, m) + r + op + print_list(TypeList<B, Ts...>{}, op, vse, cse, m, l, r);
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(productexpr<minus_one_t, Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "-" + print_list(List(Ts{}...), "", vse, cse, m);
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(productexpr<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_list(List(Ts{}...), "", vse, cse, m);
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string pr_add_impl(TypeList<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(T{}, vse, cse, m);
  }

  template <typename A, typename B, typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string pr_add_impl(TypeList<A, B, Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    std::string rest = pr_add_impl(TypeList<B, Ts...>{}, vse, cse, m);
    if (rest[0] == '-') {
      return print(A{}, vse, cse, m) + rest;
    }
    return print(A{}, vse, cse, m) + " + " + rest;
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(addexpr<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "(" + pr_add_impl(List(Ts{}...), vse, cse, m) + ")";
  }

  template <typename A, typename B, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(dotproduct<A, B>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_list(TypeList<A, B>{}, " * ", vse, cse, m);
  }

  template <typename Lhs, typename Rhs, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(equation<Lhs, Rhs>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_list(TypeList<Lhs, Rhs>{}, " = ", vse, cse, m);
  }

  template <typename A, typename B, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(crossproduct<A, B>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "(" + print_list(TypeList<A, B>{}, " xx ", vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(powerexpr<T, half_t>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "sqrt(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename E, typename... Vs, typename... Cs, typename M, EnableIf<is_leaf(T{})> = 0>
  inline std::string print(powerexpr<T, E>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m) {
    return print(T{}, vse, cse, m) + "^(" + print(E{}, vse, cse, m) + ")";
  }

  template <typename... Ts, typename E, typename... Vs, typename... Cs, typename M, EnableIf<!std::is_same<E, half_t>::value> = 0>
  inline std::string print(powerexpr<addexpr<Ts...>, E>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(addexpr<Ts...>{}, vse, cse, m) + "^(" + print(E{}, vse, cse, m) + ")";
  }

  template <typename T, typename E, typename... Vs, typename... Cs, typename M, EnableIf<!is_leaf(T{})> = 0>
  inline std::string print(powerexpr<T, E>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "(" + print(T{}, vse, cse, m) + ")^(" + print(E{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(determinantexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "det(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(absexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "|\\" + print(T{}, vse, cse, m) + "|";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(named_parameter<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return typeid(T{}).name();
  }

  template <std::size_t I, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(refexpr<I>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "r_" + std::to_string(I);
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(balanceexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "bal(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(logexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "log(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(expexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "exp(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(sinexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "sin(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(arcsinexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "arcsin(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(cosexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "cos(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(arccosexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "arccos(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(tanexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "tan(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(arctanexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "arctan(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename A, typename B, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(arctan2expr<A, B>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "arctan2(" + print_list(TypeList<A, B>{}, ", ", vse, cse, m) + ")";
  }

  template <typename C, typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(vectorexpr<C, Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "[" + print_list(List(Ts{}...), ", ", vse, cse, m, "[", "]") + "]";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(vectorlength<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "norm(" + print(T{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(matrixinverseexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(T{}, vse, cse, m) + "^(-1)";
  }

  template <typename C, typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print_by_row(vectorexpr<C, Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "[" + print_list(List(Ts{}...), ", ", vse, cse, m) + "]";
  }

  template <typename T, typename... Vs, typename... Cs, typename M, EnableIf<is_vectorexpr(T{})> = 0>
  inline std::string print_by_row(constantsubexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_by_row(T{}, vse, cse, m);
  }

  template <typename A, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print_by_rows(TypeList<A>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_by_row(A{}, vse, cse, m);
  }

  template <typename A, typename B, typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print_by_rows(TypeList<A, B, Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_by_row(A{}, vse, cse, m) + ", " + print_by_rows(List(B{}, Ts{}...), vse, cse, m);
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string pr_impl(baseexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(T{}, vse, cse, m);
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string pr_impl(TypeList<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_by_rows(List(Ts{}...), vse, cse, m);
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M, EnableIf<all(is_vectorexpr, List(Ts{}...))> = 0>
  inline std::string print(matrixexpr<Ts...> mat, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "[" + pr_impl(rows(mat), vse, cse, m) + "]";
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M, EnableIf<!all(is_vectorexpr, List(Ts{}...))> = 0>
  inline std::string print(matrixexpr<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "[" + print_list(List(Ts{}...), ", ", vse, cse, m, "", "") + "]";
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M>
  inline std::string print(rotationmatrixexpr<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(matrixexpr<Ts...>{}, vse, cse, m);
  }

  template <typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(trueexpr, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "TT";
  }

  template <typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(falseexpr, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "_|_";
  }

  template <typename Lhs, typename Rhs, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(lessthanequalexpr<Lhs, Rhs>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(Lhs{}, vse, cse, m) + " <= " + print(Rhs{}, vse, cse, m);
  }

  template <typename Lhs, typename Rhs, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(lessthanexpr<Lhs, Rhs>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(Lhs{}, vse, cse, m) + " < " + print(Rhs{}, vse, cse, m);
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(logicalandexpr<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_list(List(Ts{}...), " and ", vse, cse, m);
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(logicalnotexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "!" + print(T{}, vse, cse, m);
  }

  template <typename Lhs, typename Rhs, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(equalexpr<Lhs, Rhs>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(Lhs{}, vse, cse, m) + " = " + print(Rhs{}, vse, cse, m);
  }

  template <typename Lhs, typename Rhs, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(notequalexpr<Lhs, Rhs>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(Lhs{}, vse, cse, m) + " != " + print(Rhs{}, vse, cse, m);
  }

  template <typename Lhs, typename Rhs, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(greaterthanexpr<Lhs, Rhs>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(Lhs{}, vse, cse, m) + " > " + print(Rhs{}, vse, cse, m);
  }

  template <typename C, typename V, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(branchcaseexpr<C, V>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "(" + print(V{}, vse, cse, m) + ", if " + print(C{}, vse, cse, m) + ")";
  }

  template <typename V, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(branchcaseexpr<trueexpr, V>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "(" + print(V{}, vse, cse, m) + ", text{otherwise})";
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(branchexpr<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "{" + print_list(List(Ts{}...), ", ", vse, cse, m) + ":}";
  }

  template <int ID, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(inverse_variable<ID>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "x_(" + std::to_string(ID) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M, EnableIf<!TypeSet<Cs...>{}.Has(T{})> = 0>
  inline std::string print(constantsubexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m) {
    return print(T{}, vse, cse, m);
  }

  template <typename T, typename... Vs, typename... Cs, typename M, EnableIf<TypeSet<Cs...>{}.Has(T{})> = 0>
  inline std::string print(constantsubexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m) {
    return constant_label(T{}) + "_(" + std::to_string(index_of(T{}, TypeList<Cs...>{})) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M, EnableIf<(make_list(vars(T{})).Size() == 0)> = 0>
  inline std::string print(varsubexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "f_(" + std::to_string(index_of(T{}, vse)) + ")";
  }

  template <typename T, typename Var, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(partialderivative<T, Var>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "(del" + print(T{}, vse, cse, m) + ") / (del" + print(Var{}, vse, cse, m) + ")";
  }

  template <typename T, typename... Vs, typename... Cs, typename M, EnableIf<(make_list(vars(T{})).Size() > 0)> = 0>
  inline std::string print(varsubexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "f_(" + std::to_string(index_of(T{}, vse)) + ")(" + print_list(make_list(vars(T{})), ",", vse, cse, m) + ")";
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(TypeList<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_list(TypeList<Ts...>{}, "`" + br(2) + "`", vse, cse, m);
  }

  template <typename... Ts, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(TypeSet<Ts...>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print_list(TypeList<Ts...>{}, "`" + br(2) + "`", vse, cse, m);
  }

  template <std::size_t I, typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(indexaccessexpr<I, T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return print(T{}, vse, cse, m) + "[" + std::to_string(I) + "]";
  }

  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(indicatorexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "bb \"1\"(" + print(T{}, vse, cse, m) + ")";
  }
  
  template <typename T, typename... Vs, typename... Cs, typename M = empty_tuple_t>
  inline std::string print(diracexpr<T>, TypeSet<Vs...> vse, TypeSet<Cs...> cse, const M& m = {}) {
    return "delta(" + print(T{}, vse, cse, m) + ")";
  }

  inline std::string br(int n = 1) {
    std::string output;
    for (int i = 0; i < n; ++i) {
      output += "<br />";
    }
    return output;
  }

  template <typename T, EnableIf<is_matrix(T{})> = 0>
  inline std::string constant_label(baseexpr<T>) {
    return "m";
  }

  template <typename T, EnableIf<is_vector(T{})> = 0>
  inline std::string constant_label(baseexpr<T>) {
    return "vec v";
  }

  template <typename T, EnableIf<!is_vector(T{}) && !is_matrix(T{})> = 0>
  inline std::string constant_label(baseexpr<T>) {
    return "c";
  }

  template <typename... Vs, typename... Cs, std::size_t... Is, typename M = empty_map_t>
  inline void print_var_subexprs(TypeSet<Vs...> vse, TypeSet<Cs...> cse, std::index_sequence<Is...>, std::string& output, const M& m = {}) {
    output += (... + "<div>`f_(" + std::to_string(int(Is)) + ") = " + print(Vs{}, vse, cse, m) + "`</div>" + br());
  }

  template <typename... Vs, typename... Cs, std::size_t... Is, typename M = empty_map_t>
  inline void print_constant_subexprs(TypeSet<Vs...> vse, TypeSet<Cs...> cse, std::index_sequence<Is...>, std::string& output, const M& m = {}) {
    output += (... + "<div>`" + constant_label(Cs{}) + "_(" + std::to_string(int(Is)) + ") = " + print(Cs{}, vse, cse, m) + "`</div>" + br());
  }

  template <bool Computed, typename E, typename M, typename Data>
  inline void operator()(const random_var<Computed, E, M, Data>& rv) {
    this->operator()(E{}, rv.values);
  }

  template <typename K, typename V>
  inline void operator()(TypeMap<K, V> map) {
    this->operator()(make_equation_list(map));
  }

  template <typename... Ts>
  inline void operator()(TypeSet<Ts...>) {
    return this->operator()(TypeList<Ts...>{});
  }

  template <typename... Ts>
  inline void operator()(TypeList<Ts...>) {
    std::string output = header();

    output += (print(Ts{}) + ... + "");
    output += br(2);
    output += "<hr />";
    output += br(2);
    output += (print_subexprs(Ts{}) + ... + "");

    output += footer();
    write_to_file(output);
  }

  template <typename T, typename M = empty_map_t>
  inline void operator()(baseexpr<T>, const M& m = {}) {
    std::string output = header();
    output += print(T{}, m);
    output += br(2);
    output += "<hr />";
    output += br(2);
    output += print_subexprs(T{}, m);
    output += footer();
    write_to_file(output);
  }

  template <typename Lhs, typename Rhs>
  inline void operator()(equation<Lhs, Rhs> eq) {
    this->operator()(make_type_set(eq));
  }

  template <typename K, typename V>
  inline std::string print(TypeMap<K, V> map) {
    return print(make_equation_list(map));
  }

  template <typename T, typename M = empty_map_t>
  inline std::string print(T, const M& m = {}) {
    constexpr auto cse = collect_constant_subexprs(T{});
    constexpr auto vse = collect_var_subexprs_recursive(T{});
    std::string output;
    output += "`";
    output += print(T{}, vse, cse, m);
    output += "`";
    output += br(2);
    return output;
  }

  template <typename K, typename V, typename M = empty_map_t>
  inline std::string print_subexprs(TypeMap<K, V> map, const M& m = {}) {
    return print_subexprs(make_equation_list(map), m);
  }

  template <typename T, typename M = empty_map_t>
  inline std::string print_subexprs(T, const M& m = {}) {
    constexpr auto cse = collect_constant_subexprs(T{});
    constexpr auto vse = collect_var_subexprs_recursive(T{});
    std::string output;
    print_var_subexprs(vse, cse, std::make_index_sequence<vse.Size()>{}, output, m);
    print_constant_subexprs(vse, cse, std::make_index_sequence<cse.Size()>{}, output, m);
    return output;
  }

  inline std::string header() {
    return "<html><script type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML\"></script><style>body { margin-top: 50px; font-size: 1.5em; text-align: center; } </style><body>";
  }

  inline std::string footer() {
    return "</body></html>";
  }

  inline void write_to_file(const std::string output) {
    std::ofstream file("output.html");
    file << output;
    file.close();
  }
};

print_html_t print_html{};


} // end namespace aether


#endif
