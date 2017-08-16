#ifndef VARIABLE_H
#define VARIABLE_H

#include "Int.h"
#include "Expr.h"
#include "Literal.h"
#include <boost/hana.hpp>
#include "aether/typelist/TypeSet.h"

namespace aether {

using namespace boost;

template <int ID>
struct boundvariable {
  float value;
};

template <int ID>
struct boundinversevariable {
  float value;
};


constexpr isa_t<variable_tag_t> is_variable{};
constexpr isa_t<inverse_variable_tag_t> is_inverse_variable{};

struct is_refvar_t {
  template <typename T, EnableIf<is_variable(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return get_id(T{}) < 0;
  }

  template <typename T, EnableIf<!is_variable(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }
};

constexpr is_refvar_t is_refvar{};

template <int ID1, int ID2>
constexpr bool less_than(inverse_variable<ID1>, inverse_variable<ID2>) {
  return ID1 < ID2;
}



template <typename T, int ID, int SeqIndex>
constexpr zero_t degree(baseexpr<T>, variable<ID, SeqIndex>) {
  return {};
}

template <int ID, int SeqIndex>
constexpr one_t degree(variable<ID, SeqIndex>, variable<ID, SeqIndex>) {
  return {};
}


template <int ID, int SeqIndex>
constexpr auto vars(variable<ID, SeqIndex> v) {
  return make_type_set(v);
}

template <int ID, int SeqIndex>
constexpr auto vars_excluding_conditionals(variable<ID, SeqIndex> v) {
  return make_type_set(v);
}

template <int ID, int SeqIndex>
constexpr int get_id(variable<ID, SeqIndex>) {
  return ID;
}

template <int ID, int SeqIndex>
constexpr auto inverse_vars(variable<ID, SeqIndex>) {
  return make_type_set();
}

template <int ID>
constexpr auto vars(inverse_variable<ID>) {
  return make_type_set();
}

template <int ID>
constexpr variable<ID> inverse_var_to_var(inverse_variable<ID>) {
  return {};
}

template <typename C, typename... Ts>
constexpr auto inverse_var_to_var(vectorexpr<C, Ts...>) {
  return make_type_set(inverse_var_to_var(Ts{})...);
}

template <int ID>
constexpr auto inverse_vars(inverse_variable<ID>) {
  return make_type_set(inverse_variable<ID>{});
}

template <int ID, typename T>
constexpr auto vars(subexpr<ID, T>) {
  return make_type_set();
}


template <int ID, typename T>
constexpr auto inverse_vars(subexpr<ID, T>) {
  return inverse_vars(T{});
}

template <int A, int SeqIndex1, int B, int SeqIndex2>
constexpr bool less_than(variable<A, SeqIndex1>, variable<B, SeqIndex2>) {
  return A < B;
}

template <int A, int SeqIndex1, int SeqIndex2>
constexpr bool less_than(variable<A, SeqIndex1>, variable<A, SeqIndex2>) {
  return SeqIndex1 < SeqIndex2;
}




struct is_subexpr_t {
  template <int ID, typename T>
  constexpr bool operator()(subexpr<ID, T>) {
    return true;
  }

  template <typename T>
  constexpr bool operator()(T) {
    return false;
  }
};

template <int ID, typename T>
constexpr subexpr<ID, T> make_subexpr(_int<ID>, T) {
  return {};
}

template <int ID, typename T>
constexpr subexpr<ID, T> make_subexpr(T) {
  return {};
}

constexpr is_subexpr_t is_subexpr{};


template <int A, typename TA, int B, typename TB>
constexpr bool less_than(subexpr<A, TA>, subexpr<B, TB>) {
  return A < B;
}


constexpr auto u0 = variable<0>{};
constexpr auto u1 = variable<1>{};
constexpr auto u2 = variable<2>{};
constexpr auto u3 = variable<3>{};
constexpr auto u4 = variable<4>{};
constexpr auto u5 = variable<5>{};
constexpr auto u6 = variable<6>{};
constexpr auto u7 = variable<7>{};
constexpr auto u8 = variable<8>{};
constexpr auto u9 = variable<9>{};

using radius_t = variable<10>;
constexpr radius_t radius{};
using theta_t = variable<11>;
constexpr theta_t theta{};
using phi_t = variable<12>;
constexpr phi_t phi{};

using x_t = variable<13>;
constexpr x_t x{};
using y_t = variable<14>;
constexpr y_t y{};
using z_t = variable<15>;
constexpr z_t z{};

using u_t = variable<16>;
constexpr u_t u{};
using v_t = variable<17>;
constexpr v_t v{};

constexpr auto X = inverse_variable<0>{};
constexpr auto Y = inverse_variable<1>{};
constexpr auto Z = inverse_variable<2>{};

} // end namespace aether

#endif
