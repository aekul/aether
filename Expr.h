#ifndef EXPR_H
#define EXPR_H

#include "fwd/Expr.h"
#include "aether/typelist/Unpack.h"
#include "aether/typelist/At.h"
#include "aether/typelist/Any.h"
#include "aether/typelist/Max.h"
#include "aether/typelist/Concat.h"
#include "aether/typelist/Contains.h"
#include "aether/typelist/Sort.h"
#include "aether/typelist/TypeSet.h"
#include "aether/NamedParameter.h"


namespace aether {

template <int I>
struct _int;

template <std::intmax_t I>
struct _intmax;

template <typename T>
struct coordinate_system {};

template <typename T>
struct tag_t {};

struct condition_tag_t : tag_t<condition_tag_t> {};
constexpr condition_tag_t condition_tag{};

struct less_than_tag_t : tag_t<less_than_tag_t> {};
constexpr less_than_tag_t less_than_tag{};

struct less_than_equal_tag_t : tag_t<less_than_equal_tag_t> {};
constexpr less_than_equal_tag_t less_than_equal_tag{};

struct greater_than_tag_t : tag_t<greater_than_tag_t> {};
constexpr greater_than_tag_t greater_than_tag{};

struct greater_than_equal_tag_t : tag_t<greater_than_equal_tag_t> {};
constexpr greater_than_equal_tag_t greater_than_equal_tag{};

struct equal_tag_t : tag_t<equal_tag_t> {};
constexpr equal_tag_t equal_tag{};

struct not_equal_tag_t : tag_t<not_equal_tag_t> {};
constexpr not_equal_tag_t not_equal_tag{};

struct logical_and_tag_t : tag_t<logical_and_tag_t> {};
constexpr logical_and_tag_t logical_and_tag{};

struct logical_or_tag_t : tag_t<logical_or_tag_t> {};
constexpr logical_or_tag_t logical_or_tag{};

struct logical_not_tag_t : tag_t<logical_not_tag_t> {};
constexpr logical_not_tag_t logical_not_tag{};

struct literal_tag_t : tag_t<literal_tag_t> {};
constexpr literal_tag_t literal_tag{};

struct constant_subexpr_tag_t : tag_t<constant_subexpr_tag_t> {};
constexpr constant_subexpr_tag_t constant_subexpr_tag{};

struct add_tag_t : tag_t<add_tag_t> {};
constexpr add_tag_t add_tag{};

struct product_tag_t : tag_t<product_tag_t> {};
constexpr product_tag_t product_tag{};

struct power_tag_t : tag_t<power_tag_t> {};
constexpr power_tag_t power_tag{};

struct exp_tag_t : tag_t<exp_tag_t> {};
constexpr exp_tag_t exp_tag{};

struct log_tag_t : tag_t<log_tag_t> {};
constexpr log_tag_t log_tag{};

struct sin_tag_t : tag_t<sin_tag_t> {};
constexpr sin_tag_t sin_tag{};

struct arcsin_tag_t : tag_t<arcsin_tag_t> {};
constexpr arcsin_tag_t arcsin_tag{};

struct cos_tag_t : tag_t<cos_tag_t> {};
constexpr cos_tag_t cos_tag{};

struct arccos_tag_t : tag_t<arccos_tag_t> {};
constexpr arccos_tag_t arccos_tag{};

struct tan_tag_t : tag_t<tan_tag_t> {};
constexpr tan_tag_t tan_tag{};

struct arctan_tag_t : tag_t<arctan_tag_t> {};
constexpr arctan_tag_t arctan_tag{};

struct arctan2_tag_t : tag_t<arctan2_tag_t> {};
constexpr arctan2_tag_t arctan2_tag{};

struct variable_tag_t : tag_t<variable_tag_t> {};
constexpr variable_tag_t variable_tag{};

struct inverse_variable_tag_t : tag_t<inverse_variable_tag_t> {};
constexpr inverse_variable_tag_t inverse_variable_tag{};

struct subexpr_tag_t : tag_t<subexpr_tag_t> {};
constexpr subexpr_tag_t subexpr_tag{};

struct var_subexpr_tag_t : tag_t<var_subexpr_tag_t> {};
constexpr var_subexpr_tag_t var_subexpr_tag{};

struct trueexpr_tag_t : tag_t<trueexpr_tag_t> {};
constexpr trueexpr_tag_t trueexpr_tag{};

struct falseexpr_tag_t : tag_t<falseexpr_tag_t> {};
constexpr falseexpr_tag_t falseexpr_tag{};

struct param_tag_t : tag_t<param_tag_t> {};
constexpr param_tag_t param_tag{};

struct named_param_tag_t : tag_t<named_param_tag_t> {};
constexpr named_param_tag_t named_param_tag{};


struct ref_tag_t : tag_t<ref_tag_t> {};
constexpr ref_tag_t ref_tag{};

struct dot_product_tag_t : tag_t<dot_product_tag_t> {};
constexpr dot_product_tag_t dot_product_tag{};

struct vector_length_tag_t : tag_t<vector_length_tag_t> {};
constexpr vector_length_tag_t vector_length_tag{};

struct cross_product_tag_t : tag_t<cross_product_tag_t> {};
constexpr cross_product_tag_t cross_product_tag{};

struct vector_tag_t : tag_t<vector_tag_t> {};
constexpr vector_tag_t vector_tag{};

struct matrix_tag_t : tag_t<matrix_tag_t> {};
constexpr matrix_tag_t matrix_tag{};

struct matrix_inverse_tag_t : tag_t<matrix_inverse_tag_t> {};
constexpr matrix_inverse_tag_t matrix_inverse_tag{};

struct branch_tag_t : tag_t<branch_tag_t> {};
constexpr branch_tag_t branch_tag{};

struct branch_case_tag_t : tag_t<branch_case_tag_t> {};
constexpr branch_case_tag_t branch_case_tag{};

struct index_access_tag_t : tag_t<index_access_tag_t> {};
constexpr index_access_tag_t index_access_tag{};

struct partial_derivative_tag_t : tag_t<partial_derivative_tag_t> {};
constexpr partial_derivative_tag_t partial_derivative_tag{};

struct indicator_tag_t : tag_t<indicator_tag_t> {};
constexpr indicator_tag_t indicator_tag{};

struct dirac_tag_t : tag_t<dirac_tag_t> {};
constexpr dirac_tag_t dirac_tag{};

struct determinant_tag_t : tag_t<determinant_tag_t> {};
constexpr determinant_tag_t determinant_tag{};

struct value_tag_t : tag_t<value_tag_t> {};
constexpr value_tag_t value_tag{};

struct array_tag_t : tag_t<array_tag_t> {};
constexpr array_tag_t array_tag{};

struct abs_tag_t : tag_t<abs_tag_t> {};
constexpr abs_tag_t abs_tag{};

struct rotation_matrix_tag_t : tag_t<rotation_matrix_tag_t> {};
constexpr rotation_matrix_tag_t rotation_matrix_tag{};

struct balance_tag_t : tag_t<balance_tag_t> {};
constexpr balance_tag_t balance_tag{};

template <typename T>
struct expr_traits;

template <typename T>
struct baseexpr {
};

template <typename Tag, typename... Ts>
struct expr : baseexpr<expr<Tag, Ts...>>, Tag {
};

template <typename... Ts>
using productexpr = expr<product_tag_t, Ts...>;

template <typename... Ts>
using addexpr = expr<add_tag_t, Ts...>;

template <typename T, typename E>
using powerexpr = expr<power_tag_t, T, E>;

template <std::intmax_t N, std::intmax_t D = 1>
using literal = expr<literal_tag_t, _intmax<N>, _intmax<D>>;

template <typename T>
using expexpr = expr<exp_tag_t, T>;

template <typename T>
using logexpr = expr<log_tag_t, T>;

template <typename T>
using sinexpr = expr<sin_tag_t, T>;

template <typename T>
using cosexpr = expr<cos_tag_t, T>;

template <typename T>
using tanexpr = expr<tan_tag_t, T>;

template <typename T>
using arcsinexpr = expr<arcsin_tag_t, T>;

template <typename T>
using arccosexpr = expr<arccos_tag_t, T>;

template <typename T>
using arctanexpr = expr<arctan_tag_t, T>;

template <typename A, typename B>
using arctan2expr = expr<arctan2_tag_t, A, B>;

template <int ID, int SeqIndex = -1>
using variable = expr<variable_tag_t, _int<ID>, _int<SeqIndex>>;

template <int ID>
using inverse_variable = expr<inverse_variable_tag_t, _int<ID>>;

template <int ID, typename T>
using subexpr = expr<subexpr_tag_t, _int<ID>, T>;

template <typename T>
using varsubexpr = expr<var_subexpr_tag_t, T>;

template <typename T>
using constantsubexpr = expr<constant_subexpr_tag_t, T>;

template <typename T>
using balanceexpr = expr<balance_tag_t, T>;

template <int ID, std::size_t N, std::size_t I, int SeqIndex = -1>
using param = expr<param_tag_t, _int<ID>, _size_t<N>, _size_t<I>, _int<SeqIndex>>;

template <std::size_t I>
using refexpr = expr<ref_tag_t, _size_t<I>>;

template <typename Name, int ID, std::size_t N, std::size_t I, int SeqIndex = -1>
using named_param = expr<named_param_tag_t, Name, _int<ID>, _size_t<N>, _size_t<I>, _int<SeqIndex>>;

template <typename C, typename... Ts>
using vectorexpr = expr<vector_tag_t, C, Ts...>;

// column-wise
template <typename... Ts>
using matrixexpr = expr<matrix_tag_t, Ts...>;

template <typename T>
using matrixinverseexpr = expr<matrix_inverse_tag_t, T>;

template <typename T>
using vectorlength = expr<vector_length_tag_t, T>;

template <typename A, typename B>
using dotproduct = expr<dot_product_tag_t, A, B>;

template <typename A, typename B>
using crossproduct = expr<cross_product_tag_t, A, B>;

template <typename... Ts>
using branchexpr = expr<branch_tag_t, Ts...>;

template <typename C, typename V>
using branchcaseexpr = expr<branch_case_tag_t, C, V>;

template <std::size_t I, typename T>
using indexaccessexpr = expr<index_access_tag_t, _size_t<I>, T>;

using trueexpr = expr<trueexpr_tag_t>;

using falseexpr = expr<falseexpr_tag_t>;

template <typename Lhs, typename Rhs>
using lessthanexpr = expr<less_than_tag_t, Lhs, Rhs>;

template <typename Lhs, typename Rhs>
using lessthanequalexpr = expr<less_than_equal_tag_t, Lhs, Rhs>;

template <typename Lhs, typename Rhs>
using greaterthanexpr = expr<greater_than_tag_t, Lhs, Rhs>;

template <typename Lhs, typename Rhs>
using greaterthanequalexpr = expr<greater_than_equal_tag_t, Lhs, Rhs>;

template <typename Lhs, typename Rhs>
using equalexpr = expr<equal_tag_t, Lhs, Rhs>;

template <typename Lhs, typename Rhs>
using notequalexpr = expr<not_equal_tag_t, Lhs, Rhs>;

template <typename... Ts>
using logicalandexpr = expr<logical_and_tag_t, Ts...>;

template <typename Lhs, typename Rhs>
using logicalorexpr = expr<logical_or_tag_t, Lhs, Rhs>;

template <typename Lhs>
using logicalnotexpr = expr<logical_not_tag_t, Lhs>;

template <typename T, typename Var>
using partialderivative = expr<partial_derivative_tag_t, T, Var>;

template <typename T>
using diracexpr = expr<dirac_tag_t, T>;

template <typename T>
using indicatorexpr = expr<indicator_tag_t, T>;

template <typename T>
using determinantexpr = expr<determinant_tag_t, T>;

template <typename... Ts>
using rotationmatrixexpr = expr<rotation_matrix_tag_t, Ts...>;

template <int ID, std::size_t N, std::size_t I>
using valueexpr = expr<value_tag_t, _int<ID>, _size_t<N>, _size_t<I>>;

template <typename T>
using arrayexpr = expr<array_tag_t, T>;

template <typename T>
using absexpr = expr<abs_tag_t, T>;

template <std::size_t R, std::size_t C>
struct dimensions {
  constexpr std::size_t Rows() const {
    return R;
  }

  constexpr std::size_t Cols() const {
    return C;
  }

  constexpr bool IsScalar() const {
    return R == 1 && C == 1;
  }

  constexpr bool IsColumnVector() const {
    return R > 1 && C == 1;
  }

  constexpr bool IsRowVector() const {
    return R == 1 && C > 1;
  }

  constexpr bool IsMatrix() const {
    return R > 1 && C > 1;
  }

  template <std::size_t R2, std::size_t C2>
  constexpr bool operator==(dimensions<R2, C2>) const {
    return R == R2 && C == C2;
  }
};

template <typename T>
constexpr dimensions<1, 1> dimensions_of(baseexpr<T>) {
  return {};
}

template <typename T>
constexpr auto dimensions_of(balanceexpr<T>) {
  return dimensions_of(T{});
}

template <typename T>
constexpr auto dimensions_of(indicatorexpr<T>) {
  return dimensions_of(T{});
}

template <typename T>
constexpr auto dimensions_of(diracexpr<T>) {
  return dimensions_of(T{});
}

template <typename T>
constexpr auto dimensions_of(constantsubexpr<T>) {
  return dimensions_of(T{});
}

template <typename T>
constexpr auto dimensions_of(matrixinverseexpr<T>) {
  return dimensions_of(T{});
}

template <typename T>
constexpr auto dimensions_of_product(TypeList<T>) {
  return dimensions_of(T{});
}

template <typename A, typename B, typename... Ts, EnableIf<dimensions_of(A{}).IsScalar()> = 0>
constexpr auto dimensions_of_product(TypeList<A, B, Ts...>) {
  return dimensions_of_product(TypeList<B, Ts...>{});
}

template <typename A, typename B, typename... Ts, EnableIf<!dimensions_of(A{}).IsScalar()> = 0>
constexpr auto dimensions_of_product(TypeList<A, B, Ts...>) {
  return dimensions<dimensions_of(A{}).Rows(), dimensions_of_product(TypeList<B, Ts...>{}).Cols()>{};
}

template <typename T, typename... Ts>
constexpr auto dimensions_of(addexpr<T, Ts...>) {
  return dimensions_of(T{});
}

template <typename C, typename V, typename... Ts>
constexpr auto dimensions_of(branchexpr<branchcaseexpr<C, V>, Ts...>) {
  return dimensions_of(V{});
}

template <typename C, typename V>
constexpr auto dimensions_of(branchcaseexpr<C, V>) {
  return dimensions_of(V{});
}

template <typename... Ts>
constexpr auto dimensions_of(productexpr<Ts...>) {
  return dimensions_of_product(TypeList<Ts...>{});
}

template <typename A, typename B>
constexpr auto dimensions_of(crossproduct<A, B>) {
  return dimensions_of(A{});
};

template <typename T>
constexpr auto dimensions_of(arrayexpr<T>) {
  return dimensions_of(T{});
}

template <typename C, typename... Ts>
constexpr dimensions<sizeof...(Ts), 1> dimensions_of(vectorexpr<C, Ts...>) {
  return {};
}

template <typename T, typename... Ts>
constexpr auto dimensions_of(matrixexpr<T, Ts...>) {
  return dimensions<dimensions_of(T{}).Rows(), sizeof...(Ts) + 1>{};
}

template <typename T, typename... Ts>
constexpr auto dimensions_of(rotationmatrixexpr<T, Ts...>) {
  return dimensions<dimensions_of(T{}).Rows(), sizeof...(Ts) + 1>{};
}


template <typename Tag, typename A, typename B>
constexpr bool dimensions_match(Tag, baseexpr<A>, baseexpr<B>) {
  return dimensions_of(A{}) == dimensions_of(B{});
};

template <typename A, typename B>
constexpr bool dimensions_match(product_tag_t, baseexpr<A>, baseexpr<B>) {
  constexpr auto dims_a = dimensions_of(A{});
  constexpr auto dims_b = dimensions_of(B{});
  return dims_a.IsScalar() || dims_a.Cols() == dims_b.Rows();
};

template <typename A, typename B>
constexpr bool dimensions_match(dot_product_tag_t, baseexpr<A>, baseexpr<B>) {
  constexpr auto dims_a = dimensions_of(A{});
  constexpr auto dims_b = dimensions_of(B{});
  return dims_a == dims_b && dims_a.Cols() == 1;
};

template <typename A, typename B>
constexpr bool dimensions_match(cross_product_tag_t, baseexpr<A>, baseexpr<B>) {
  constexpr auto dims_a = dimensions_of(A{});
  constexpr auto dims_b = dimensions_of(B{});
  return dims_a == dims_b && (dims_a.Rows() == 3 && dims_a.Cols() == 1);
};

struct is_constant_t {
  template <typename T, EnableIf<is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return true;
  }

  template <typename T>
  constexpr bool operator()(coordinate_system<T>) const {
    return true;
  }

  template <std::size_t I>
  constexpr bool operator()(refexpr<I>) const {
    return true;
  }

  template <int ID, int SeqIndex>
  constexpr bool operator()(variable<ID, SeqIndex>) const {
    return false;
  }

  template <std::size_t I>
  constexpr bool operator()(_size_t<I>) const {
    return true;
  }

  template <typename T>
  constexpr bool operator()(varsubexpr<T>) const {
    return false;
  }

  template <typename... Ts>
  constexpr bool operator()(TypeList<Ts...>) const {
    return (this->operator()(Ts{}) && ... && true);
  }

  template <typename T, EnableIf<!is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return this->operator()(operands(T{}));
  }

  template <typename T>
  constexpr bool operator()(constantsubexpr<T>) const {
    return true;
  }
};

constexpr is_constant_t is_constant{};

struct depends_on_inverse_variables_t {
  template <typename T, EnableIf<is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }

  template <typename T>
  constexpr bool operator()(coordinate_system<T>) const {
    return false;
  }

  template <std::size_t I>
  constexpr bool operator()(refexpr<I>) const {
    return false;
  }

  template <std::size_t I>
  constexpr bool operator()(_size_t<I>) const {
    return false;
  }

  template <int ID>
  constexpr bool operator()(inverse_variable<ID>) const {
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

constexpr depends_on_inverse_variables_t depends_on_inverse_variables{};

template <typename Tag, typename T, typename... Ts>
constexpr auto make_expr(Tag, T, Ts...) {
  return canonicalize(expr<Tag, T, Ts...>{});
}

template <typename T>
constexpr constantsubexpr<T> make_expr(constant_subexpr_tag_t, constantsubexpr<T>) {
  return {};
}

template <typename T>
constexpr varsubexpr<T> make_expr(var_subexpr_tag_t, varsubexpr<T>) {
  return {};
}

template <typename Tag, typename... Ts>
constexpr auto make_expr(Tag tag, TypeList<Ts...>) {
  return make_expr(tag, Ts{}...);
}

template <typename T, typename V>
constexpr bool are_equal(baseexpr<T>, baseexpr<V>) {
  return std::is_same<T, V>::value;
}

template <typename Tag, typename... Ts>
constexpr Tag tag_of(expr<Tag, Ts...>) {
  return {};
}

template <typename T>
constexpr bool is_tag(T) {
  return false;
}

template <typename T>
constexpr bool is_tag(tag_t<T>) {
  return true;
}

template <typename Tag, typename... Ts>
constexpr TypeList<Ts...> operands(expr<Tag, Ts...>) {
  return {};
}

template <typename Tag, typename... Ts>
constexpr bool has_tag(Tag, expr<Tag, Ts...>) {
  return true;
}

template <typename Tag, typename T, typename... Ts>
constexpr bool has_tag(Tag, expr<T, Ts...>) {
  return false;
}

template <typename Tag>
struct isa_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return std::is_base_of<Tag, T>::value;
  }
};

struct is_scalarexpr_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return dimensions_of(T{}).IsScalar();
  }
};

constexpr is_scalarexpr_t is_scalarexpr{};

struct is_vector_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    constexpr auto dims = dimensions_of(T{});
    return  isa_t<vector_tag_t>{}(T{}) || dims.IsColumnVector() || dims.IsRowVector();
  }
};

constexpr is_vector_t is_vector{};

template <std::size_t I, typename T, EnableIf<is_vector(T{})> = 0>
constexpr auto at(baseexpr<T>) {
  return make_expr(index_access_tag, _size_t<I>{}, T{});
}


struct is_matrix_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    constexpr auto dims = dimensions_of(T{});
    return  isa_t<matrix_tag_t>{}(T{}) || dims.IsMatrix();
  }
};

constexpr is_matrix_t is_matrix{};

template <typename Tag>
constexpr isa_t<Tag> isa{};

constexpr isa_t<add_tag_t> is_addexpr{};
constexpr isa_t<product_tag_t> is_productexpr{};
constexpr isa_t<sin_tag_t> is_sin{};
constexpr isa_t<cos_tag_t> is_cos{};
constexpr isa_t<arcsin_tag_t> is_arcsin{};
constexpr isa_t<arccos_tag_t> is_arccos{};
constexpr isa_t<var_subexpr_tag_t> is_varsubexpr{};
constexpr isa_t<vector_tag_t> is_vectorexpr{};
constexpr isa_t<array_tag_t> is_arrayexpr{};
constexpr isa_t<branch_tag_t> is_branchexpr{};
constexpr isa_t<trueexpr_tag_t> is_trueexpr{};
constexpr isa_t<falseexpr_tag_t> is_falseexpr{};

struct is_trigonometry_t {
  template <typename T>
  constexpr bool operator()(T) const {
    return is_sin(T{}) || is_cos(T{});
  }
};

constexpr is_trigonometry_t is_trigonometry{};

constexpr auto leaf_tags = List(
  literal_tag
  , param_tag
  , named_param_tag
  , ref_tag
  , value_tag
  , subexpr_tag
  , inverse_variable_tag
  , variable_tag
  , trueexpr_tag
  , falseexpr_tag
);

template <typename A>
constexpr bool is_leaf(tag_t<A>) {
  return contains(A{}, leaf_tags);
}

constexpr auto precedence = List(
  condition_tag
  , literal_tag
  , param_tag
  , named_param_tag
  , ref_tag
  , subexpr_tag
  , constant_subexpr_tag
  , balance_tag
  , var_subexpr_tag
  , inverse_variable_tag
  , variable_tag
  , index_access_tag
  , trueexpr_tag
  , falseexpr_tag
  , sin_tag
  , arcsin_tag
  , cos_tag
  , arccos_tag
  , tan_tag
  , arctan_tag
  , arctan2_tag
  , abs_tag
  , determinant_tag
  , dirac_tag
  , indicator_tag
  , add_tag
  , product_tag
  , power_tag
  , dot_product_tag
  , vector_length_tag
  , less_than_tag
  , less_than_equal_tag
  , greater_than_tag
  , greater_than_equal_tag
  , equal_tag
  , not_equal_tag
  , branch_tag
  , branch_case_tag
  , vector_tag
  , cross_product_tag
  , matrix_tag
  , rotation_matrix_tag
);


template <typename A>
constexpr std::size_t group(baseexpr<A>) {
  return index_of(tag_of(A{}), precedence);
}

template <std::intmax_t N, std::intmax_t D, typename A>
constexpr std::size_t group(productexpr<literal<N, D>, A>) {
  return index_of(tag_of(A{}), precedence);
}

template <typename A, typename B, EnableIf<group(A{}) == group(B{}) && is_leaf(A{}) && is_leaf(B{})> = 0>
constexpr bool compare_group(baseexpr<A>, baseexpr<B>) {
  return less_than(A{}, B{});
}


constexpr bool compare_group_list(TypeList<>, TypeList<>, TypeList<>, TypeList<>) {
  return false;
}

template <std::size_t B, typename... Bs, typename TB, typename... TBs>
constexpr bool compare_group_list(TypeList<>, TypeList<>, TypeList<_size_t<B>, Bs...>, TypeList<TB, TBs...>) {
  return true;
}

template <std::size_t A, typename... As, typename TA, typename... TAs>
constexpr bool compare_group_list(TypeList<_size_t<A>, As...>, TypeList<TA, TAs...>, TypeList<>, TypeList<>) {
  return false;
}

template <std::size_t A, typename... As, typename TA, typename... TAs, std::size_t B, typename... Bs, typename TB, typename... TBs, EnableIf<(A == B) && (!is_leaf(TA{}) || are_equal(TA{}, TB{}))> = 0>
constexpr bool compare_group_list(TypeList<_size_t<A>, As...>, TypeList<TA, TAs...>, TypeList<_size_t<B>, Bs...>, TypeList<TB, TBs...>) {
  return compare_group_list(TypeList<As...>{}, TypeList<TAs...>{}, TypeList<Bs...>{}, TypeList<TBs...>{});
}

template <std::size_t A, typename... As, typename TA, typename... TAs, std::size_t B, typename... Bs, typename TB, typename... TBs, EnableIf<(A == B) && is_leaf(TA{}) && !are_equal(TA{}, TB{})> = 0>
constexpr bool compare_group_list(TypeList<_size_t<A>, As...>, TypeList<TA, TAs...>, TypeList<_size_t<B>, Bs...>, TypeList<TB, TBs...>) {
  return less_than(TA{}, TB{});
}

template <std::size_t A, typename... As, typename TA, typename... TAs, std::size_t B, typename... Bs, typename TB, typename... TBs, EnableIf<(A > B)> = 0>
constexpr bool compare_group_list(TypeList<_size_t<A>, As...>, TypeList<TA, TAs...>, TypeList<_size_t<B>, Bs...>, TypeList<TB, TBs...>) {
  return false;
}

template <std::size_t A, typename... As, typename TA, typename... TAs, std::size_t B, typename... Bs, typename TB, typename... TBs, EnableIf<(A < B)> = 0>
constexpr bool compare_group_list(TypeList<_size_t<A>, As...>, TypeList<TA, TAs...>, TypeList<_size_t<B>, Bs...>, TypeList<TB, TBs...>) {
  return true;
}


enum class CompareResult {
  a_less_than_b
  , a_equal_to_b
  , a_greater_than_b
};

template <typename A, EnableIf<is_leaf(A{})> = 0>
constexpr int preorder_size(baseexpr<A>) {
  return 1;
}

template <typename A>
constexpr int preorder_size(coordinate_system<A>) {
  return 0;
}

template <std::size_t I>
constexpr int preorder_size(_size_t<I>) {
  return 0;
}

template <typename... As>
constexpr int preorder_size(TypeList<As...>) {
  return (preorder_size(As{}) + ... + 0);
}

template <std::size_t I, typename T>
constexpr auto preorder_size(indexaccessexpr<I, T>) {
  return preorder_size(T{});
}

template <std::intmax_t N, std::intmax_t D, typename T>
constexpr int preorder_size(productexpr<literal<N, D>, T>) {
  return preorder_size(T{});
}

template <std::intmax_t N, std::intmax_t D, typename... Ts>
constexpr auto preorder_size(productexpr<literal<N, D>, Ts...>) {
  return 1 + preorder_size(List(Ts{}...));
}

template <typename A, EnableIf<!is_leaf(A{})> = 0>
constexpr int preorder_size(baseexpr<A>) {
  return 1 + preorder_size(operands(A{}));
}

template <typename C, typename... Ts>
constexpr int preorder_size(vectorexpr<C, Ts...>) {
  return (preorder_size(Ts{}) + ... + 1);
}

template <typename T>
constexpr int preorder_size(constantsubexpr<T>) {
  return 1;
}


template <typename A>
constexpr auto at_in_preorder(_int<0>, baseexpr<A>) {
  return A{};
}

template <int I, typename A, EnableIf<(I > preorder_size(A{}))> = 0>
constexpr auto at_in_preorder(_int<I>, baseexpr<A>) {
  static_assert(!std::is_same<A, A>::value, "at_in_preorder(): I out of bounds");
}

template <int I>
constexpr auto at_in_preorder(_int<I>, TypeList<>) {
  static_assert(!std::is_same<_int<I>, _int<I>>::value, "at_in_preorder(): I out of bounds");
}

template <int I, typename A, typename... As, EnableIf<(I >= preorder_size(A{}))> = 0>
constexpr auto at_in_preorder(_int<I>, TypeList<A, As...>) {
  return at_in_preorder(_int<I - preorder_size(A{})>{}, TypeList<As...>{});
}

template <int I, typename A, typename... As, EnableIf<(I < preorder_size(A{}))> = 0>
constexpr auto at_in_preorder(_int<I> i, TypeList<A, As...>) {
  return at_in_preorder(i, A{});
}

template <int I, typename A, EnableIf<!is_leaf(A{}) && (I > 0)> = 0>
constexpr auto at_in_preorder(_int<I>, baseexpr<A>) {
  return at_in_preorder(_int<I - 1>{}, operands(A{}));
}

template <int I, std::size_t Index, typename T, EnableIf<(I > 0)> = 0>
constexpr auto at_in_preorder(_int<I> i, indexaccessexpr<Index, T>) {
  return at_in_preorder(i, T{});
}

template <int I, std::intmax_t N, std::intmax_t D, typename T, EnableIf<(I > 0)> = 0>
constexpr auto at_in_preorder(_int<I> i, productexpr<literal<N, D>, T>) {
  return at_in_preorder(i, T{});
}

template <int I, std::intmax_t N, std::intmax_t D, typename... Ts, EnableIf<(I > 0)> = 0>
constexpr auto at_in_preorder(_int<I>, productexpr<literal<N, D>, Ts...>) {
  return at_in_preorder(_int<I - 1>{}, List(Ts{}...));
}

template <int I, typename C, typename... Ts, EnableIf<(I > 0)> = 0>
constexpr auto at_in_preorder(_int<I>, vectorexpr<C, Ts...>) {
  return at_in_preorder(_int<I - 1>{}, List(Ts{}...));
}

template <int I, typename T, EnableIf<(I > 0)> = 0>
constexpr auto at_in_preorder(_int<I>, constantsubexpr<T>) {
  static_assert(!std::is_same<T, T>::value);
  return 1;
}

template <int I, typename A, typename B, typename PreorderA, typename PreorderB, EnableIf<(group(PreorderA{}) > group(PreorderB{}))> = 0>
constexpr bool compare_preorder_single(_int<I>, baseexpr<A>, baseexpr<B>, baseexpr<PreorderA>, baseexpr<PreorderB>) {
  return false;
}

template <int I, typename A, typename B, typename PreorderA, typename PreorderB, EnableIf<(group(PreorderA{}) < group(PreorderB{}))> = 0>
constexpr bool compare_preorder_single(_int<I>, baseexpr<A>, baseexpr<B>, baseexpr<PreorderA>, baseexpr<PreorderB>) {
  return true;
}

template <int I, typename A, typename B, typename PreorderA, typename PreorderB, EnableIf<(group(PreorderA{}) == group(PreorderB{}))> = 0>
constexpr bool compare_preorder_single(_int<I> i, baseexpr<A>, baseexpr<B>, baseexpr<PreorderA>, baseexpr<PreorderB>) {
  return compare_preorder_single_impl(i, A{}, B{}, PreorderA{}, PreorderB{});
}

template <int I, typename A, typename B, typename PreorderA, typename PreorderB, EnableIf<is_leaf(PreorderA{}) && !are_equal(PreorderA{}, PreorderB{})> = 0>
constexpr bool compare_preorder_single_impl(_int<I>, baseexpr<A>, baseexpr<B>, baseexpr<PreorderA>, baseexpr<PreorderB>) {
  return less_than(PreorderA{}, PreorderB{});
}

template <int I, typename A, typename B, typename PreorderA, typename PreorderB, EnableIf<is_leaf(PreorderA{}) && are_equal(PreorderA{}, PreorderB{})> = 0>
constexpr bool compare_preorder_single_impl(_int<I>, baseexpr<A>, baseexpr<B>, baseexpr<PreorderA>, baseexpr<PreorderB>) {
  return compare_preorder_impl(_int<I + 1>{}, A{}, B{});
}

template <int I, typename A, typename B, typename PreorderA, typename PreorderB, EnableIf<(!is_leaf(PreorderA{}) && !are_equal(PreorderA{}, PreorderB{}))> = 0>
constexpr bool compare_preorder_single_impl(_int<I>, baseexpr<A>, baseexpr<B>, baseexpr<PreorderA>, baseexpr<PreorderB>) {
  return compare_preorder_impl(_int<I + 1>{}, A{}, B{});
}

template <int I, typename A, typename B, typename PreorderA, typename PreorderB, EnableIf<(!is_leaf(PreorderA{}) && are_equal(PreorderA{}, PreorderB{}))> = 0>
constexpr bool compare_preorder_single_impl(_int<I>, baseexpr<A>, baseexpr<B>, baseexpr<PreorderA>, baseexpr<PreorderB>) {
  return compare_preorder_impl(_int<I + preorder_size(PreorderA{})>{}, A{}, B{});
}

template <int I, typename A, typename B, EnableIf<(I < preorder_size(A{}) && I >= preorder_size(B{}))> = 0>
constexpr bool compare_preorder_impl(_int<I>, baseexpr<A>, baseexpr<B>) {
  return false; 
}

template <int I, typename A, typename B, EnableIf<(I >= preorder_size(A{}) && I < preorder_size(B{}))> = 0>
constexpr bool compare_preorder_impl(_int<I>, baseexpr<A>, baseexpr<B>) {
  return true; 
}

template <int I, typename A, typename B, EnableIf<(I >= preorder_size(A{}) && I >= preorder_size(B{}))> = 0>
constexpr bool compare_preorder_impl(_int<I>, baseexpr<A>, baseexpr<B>) {
  return false; 
}

template <int I, typename A, typename B, EnableIf<(I < preorder_size(A{}) && I < preorder_size(B{}))> = 0>
constexpr bool compare_preorder_impl(_int<I> i, baseexpr<A>, baseexpr<B>) {
  return compare_preorder_single(i, A{}, B{}, at_in_preorder(i, A{}), at_in_preorder(i, B{}));
}

template <typename A, typename B>
constexpr auto compare_preorder(baseexpr<A>, baseexpr<B>) {
  return compare_preorder_impl(_int<0>{}, A{}, B{});
}








template <typename A, typename B>
constexpr bool compare_group_list(baseexpr<A>, baseexpr<B>) {
  constexpr auto group_list_a = preorder(A{});
  constexpr auto group_list_b = preorder(B{});
  return compare_group_list(at<0>(group_list_a), at<1>(group_list_a), at<0>(group_list_b), at<1>(group_list_b));
}


template <typename A, typename... As, typename B, typename... Bs>
constexpr bool compare_first_operand(productexpr<A, As...>, productexpr<B, Bs...>) {
  return A{} < B{};
}

template <typename A, typename B, EnableIf<(group(A{}) == group(B{}) && (!is_leaf(A{}) || !is_leaf(B{}))) && group_lists_equal(A{}, B{})> = 0>
constexpr bool compare_group(baseexpr<A>, baseexpr<B>) {
  return compare_preorder(A{}, B{});
}

template <typename A, typename B>
constexpr bool group_lists_equal(baseexpr<A>, baseexpr<B>) {
  return compare_preorder(A{}, B{}) && compare_preorder(B{}, A{});
}

template <typename A, typename B, EnableIf<(group(A{}) == group(B{}) && (!is_leaf(A{}) || !is_leaf(B{}))) && !group_lists_equal(A{}, B{})> = 0>
constexpr bool compare_group(baseexpr<A>, baseexpr<B>) {
  return compare_preorder(A{}, B{});
}

template <typename A, typename B, EnableIf<group(A{}) != group(B{})> = 0>
constexpr bool compare_group(baseexpr<A>, baseexpr<B>) {
  return group(A{}) < group(B{});
}

template <typename A, typename B, EnableIf<size(A{}) == size(B{})> = 0>
constexpr bool compare_size(baseexpr<A>, baseexpr<B>) {
  return compare_group(A{}, B{});
}

template <typename A, typename B, EnableIf<size(A{}) != size(B{})> = 0>
constexpr bool compare_size(baseexpr<A>, baseexpr<B>) {
  return size(A{}) < size(B{});
}



template <typename A, typename B>
constexpr bool var_list_less_than(baseexpr<A>, TypeList<>, baseexpr<B>, TypeList<>) {
  return compare_size(A{}, B{});
}

template <typename A, typename AV, typename... AVs, typename B>
constexpr bool var_list_less_than(baseexpr<A>, TypeList<AV, AVs...>, baseexpr<B>, TypeList<>) {
  return false;
}

template <typename A, typename B, typename BV, typename... BVs>
constexpr bool var_list_less_than(baseexpr<A>, TypeList<>, baseexpr<B>, TypeList<BV, BVs...>) {
  return true;
}

template <typename A, typename AV, typename... AVs, typename B, typename BV, typename... BVs>
constexpr bool var_list_less_than(baseexpr<A>, TypeList<AV, AVs...>, baseexpr<B>, TypeList<BV, BVs...>) {
  return less_than(AV{}, BV{});
}

template <typename A, typename V, typename... AVs, typename B, typename... BVs, EnableIf<degree(A{}, V{}) < degree(B{}, V{})> = 0>
constexpr bool var_list_less_than(baseexpr<A>, TypeList<V, AVs...>, baseexpr<B>, TypeList<V, BVs...>) {
  return true;
}

template <typename A, typename V, typename... AVs, typename B, typename... BVs, EnableIf<degree(A{}, V{}) == degree(B{}, V{})> = 0>
constexpr bool var_list_less_than(baseexpr<A>, TypeList<V, AVs...>, baseexpr<B>, TypeList<V, BVs...>) {
  return var_list_less_than(A{}, TypeList<AVs...>{}, B{}, TypeList<BVs...>{});
}





template <typename A, typename B, EnableIf<vars(A{}).Size() == 0 && vars(B{}).Size() == 0 && (inverse_vars(A{}).Size() > 0 || inverse_vars(B{}).Size() > 0)> = 0>
constexpr bool compare_var_sets(baseexpr<A>, baseexpr<B>) {
  return var_list_less_than(A{}, sort(inverse_vars(A{})), B{}, sort(inverse_vars(B{})));
}

template <typename A, typename B, EnableIf<!(vars(A{}).Size() == 0 && vars(B{}).Size() == 0 && (inverse_vars(A{}).Size() > 0 || inverse_vars(B{}).Size() > 0))> = 0>
constexpr bool compare_var_sets(baseexpr<A>, baseexpr<B>) {
  return var_list_less_than(A{}, sort(vars(A{})), B{}, sort(vars(B{})));
}

template <typename A, typename B, EnableIf<!(vars(A{}).Size() == 0 && vars(B{}).Size() == 0 && (inverse_vars(A{}).Size() > 0 || inverse_vars(B{}).Size() > 0))> = 0>
constexpr bool compare_var_sets2(baseexpr<A>, baseexpr<B>) {
  return var_list_less_than(A{}, sort(vars(A{})), B{}, sort(vars(B{})));
}

template <typename A>
constexpr bool less_than(baseexpr<A>, baseexpr<A>) {
  return false;
}

template <typename A, typename B>
constexpr bool less_than(baseexpr<A>, baseexpr<B>) {
  return compare_var_sets(A{}, B{});
}

template <typename A, std::intmax_t N, std::intmax_t D>
constexpr bool less_than(baseexpr<A>, productexpr<literal<N, D>, A>) {
  return true;
}

template <std::intmax_t N, std::intmax_t D, typename A>
constexpr bool less_than(productexpr<literal<N, D>, A>, baseexpr<A>) {
  return false;
}


template <typename T, typename Var>
constexpr bool depends_on(baseexpr<T>, Var) {
  return contains(Var{}, make_list(vars(T{})));
}

template <typename... Ts, typename Var>
constexpr bool any_depend_on(TypeList<Ts...>, Var) {
  return (depends_on(Ts{}, Var{}) || ... || false);
}

template <typename... Ts, typename Var>
constexpr bool all_depend_on(TypeList<Ts...>, Var) {
  return (depends_on(Ts{}, Var{}) && ... && true);
}

template <typename Var>
struct has_dependent_variable {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return depends_on(T{}, Var{});
  }
};

template <typename T>
constexpr T canonicalize_impl(baseexpr<T>) {
  return {};
}

template <typename Current>
constexpr auto canonicalize_recursive(baseexpr<Current>, baseexpr<Current>) {
  return replace_if_constant(Current{});
}

template <typename Current, typename Next>
constexpr auto canonicalize_recursive(baseexpr<Current>, baseexpr<Next>) {
  return canonicalize(Next{}); 
}

template <typename T>
constexpr auto canonicalize(baseexpr<T>) {
  return canonicalize_recursive(T{}, canonicalize_impl(T{}));
}

template <typename A>
constexpr bool in_canonical_form(baseexpr<A>) {
  return canonicalize(A{}) == A{};
}


constexpr int size(TypeList<>) {
  return 0;
}

template <typename T, typename Var, EnableIf<depends_on(T{}, Var{})> = 0>
constexpr auto var_subexpr(baseexpr<T>, Var) {
  return T{};
}

template <typename T, typename... Ts>
constexpr int size(TypeList<T, Ts...>) {
  return size(T{}) + size(TypeList<Ts...>{});
}

template <std::size_t I>
constexpr auto vars(_size_t<I>) {
  return make_type_set();
}

template <typename... Ts>
constexpr auto vars(TypeSet<Ts...>) {
  return MergeAll(vars(Ts{})...);
}

template <typename Tag, typename... Ts, EnableIf<is_leaf(Tag{})> = 0>
constexpr auto vars(expr<Tag, Ts...>) {
  return make_type_set();
}

template <typename Tag, typename... Ts, EnableIf<!is_leaf(Tag{})> = 0>
constexpr auto vars(expr<Tag, Ts...>) {
  return MergeAll(vars(Ts{})...);
}

template <std::size_t I>
constexpr auto refs(_size_t<I>) {
  return make_type_set();
}

template <std::size_t I>
constexpr auto refs(refexpr<I> r) {
  return make_type_set(r);
}

template <typename T>
constexpr auto refs(coordinate_system<T>) {
  return make_type_set();
}

template <typename... Ts>
constexpr auto refs(TypeSet<Ts...>) {
  return MergeAll(refs(Ts{})...);
}

template <typename Tag, typename... Ts, EnableIf<is_leaf(Tag{})> = 0>
constexpr auto refs(expr<Tag, Ts...>) {
  return make_type_set();
}

template <typename Tag, typename... Ts, EnableIf<!is_leaf(Tag{})> = 0>
constexpr auto refs(expr<Tag, Ts...>) {
  return MergeAll(refs(Ts{})...);
}


template <std::size_t I>
constexpr auto vars_excluding_conditionals(_size_t<I>) {
  return make_type_set();
}

template <typename... Ts>
constexpr auto vars_excluding_conditionals(TypeSet<Ts...>) {
  return MergeAll(vars_excluding_conditionals(Ts{})...);
}

template <typename Tag, typename... Ts, EnableIf<is_leaf(Tag{})> = 0>
constexpr auto vars_excluding_conditionals(expr<Tag, Ts...>) {
  return make_type_set();
}

template <typename Tag, typename... Ts, EnableIf<!is_leaf(Tag{})> = 0>
constexpr auto vars_excluding_conditionals(expr<Tag, Ts...>) {
  return MergeAll(vars_excluding_conditionals(Ts{})...);
}

template <typename... Ts>
constexpr auto vars_excluding_conditionals(TypeList<Ts...>) {
  return MergeAll(vars_excluding_conditionals(Ts{})...);
}

template <typename C, typename V>
constexpr auto vars_excluding_conditionals(branchcaseexpr<C, V>) {
  return vars_excluding_conditionals(V{});
}


template <typename T>
constexpr auto inverse_vars(T) {
  return make_type_set();
}

template <typename Tag, typename... Ts, EnableIf<is_leaf(Tag{})> = 0>
constexpr auto inverse_vars(expr<Tag, Ts...>) {
  return make_type_set();
}

template <typename Tag, typename... Ts, EnableIf<!is_leaf(Tag{})> = 0>
constexpr auto inverse_vars(expr<Tag, Ts...>) {
  return MergeAll(inverse_vars(Ts{})...);
}

template <typename Tag, typename... Ts>
constexpr auto inverse_vars(expr<condition_tag_t, Tag, Ts...>) {
  return MergeAll(inverse_vars(Ts{})...);
}

template <typename T>
constexpr int size(baseexpr<T>) {
  return 1;
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr T replace_constants_recursive(baseexpr<T>) {
  return {};
}

template <std::size_t I>
constexpr _size_t<I> replace_constants_recursive(_size_t<I>) {
  return {};
}

template <typename T, EnableIf<is_constant(T{}) && !depends_on_inverse_variables(T{}) && !is_leaf(T{})> = 0>
constexpr auto replace_constants_recursive(baseexpr<T>) {
  return unpack<expr>(constant_subexpr_tag, unpack_list<expr>(List(tag_of(T{})).Concat(replace_constants_recursive(operands(T{})))));
}

template <typename... Ts>
constexpr auto replace_constants_recursive(TypeList<Ts...>) {
  return List(replace_constants_recursive(Ts{})...);
}

template <typename C, typename V>
constexpr auto replace_constants_recursive(branchcaseexpr<C, V>) {
  return unpack<expr>(branch_case_tag, replace_constants_recursive(C{}), replace_constants_recursive(V{}));
}

template <typename T>
constexpr constantsubexpr<T> replace_constants_recursive(constantsubexpr<T>) {
  return {};
}

template <typename T, EnableIf<(!is_constant(T{}) || depends_on_inverse_variables(T{})) && !is_leaf(T{})> = 0>
constexpr auto replace_constants_recursive(baseexpr<T>) {
  return unpack_list<expr>(List(tag_of(T{})).Concat(replace_constants_recursive(operands(T{}))));
}



template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr T replace_constant_operands(baseexpr<T>) {
  return {};
}

template <std::size_t I>
constexpr _size_t<I> replace_constant_operands(_size_t<I>) {
  return {};
}

template <typename T, EnableIf<is_constant(T{}) && !depends_on_inverse_variables(T{}) && !is_leaf(T{})> = 0>
constexpr auto replace_constant_operands(baseexpr<T>) {
  return unpack<expr>(constant_subexpr_tag, unpack_list<expr>(List(tag_of(T{})).Concat(replace_constant_operands(operands(T{})))));
}

template <typename... Ts>
constexpr auto replace_constant_operands(TypeList<Ts...>) {
  return List(replace_constant_operands(Ts{})...);
}

template <typename C, typename V>
constexpr auto replace_constant_operands(branchcaseexpr<C, V>) {
  return unpack<expr>(branch_case_tag, replace_constant_operands(C{}), replace_constant_operands(V{}));
}

template <typename T>
constexpr constantsubexpr<T> replace_constant_operands(constantsubexpr<T>) {
  return {};
}

template <typename T, EnableIf<(!is_constant(T{}) || depends_on_inverse_variables(T{})) && !is_leaf(T{})> = 0>
constexpr T replace_constant_operands(baseexpr<T>) {
  return {};
}



template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr T replace_if_constant(baseexpr<T>) {
  return {};
}

template <std::size_t I>
constexpr _size_t<I> replace_if_constant(_size_t<I>) {
  return {};
}

template <typename T>
constexpr named_parameter<T> replace_if_constant(named_parameter<T>) {
  return {};
}

template <typename T, EnableIf<is_constant(T{}) && !depends_on_inverse_variables(T{}) && !is_leaf(T{})> = 0>
constexpr auto replace_if_constant(baseexpr<T>) {
  return unpack<expr>(constant_subexpr_tag, T{});
}

template <typename C, typename V>
constexpr auto replace_if_constant(branchcaseexpr<C, V> b) {
  return b;
}

template <typename T>
constexpr constantsubexpr<T> replace_if_constant(constantsubexpr<T> cs) {
  return cs;
}

template <typename T>
constexpr varsubexpr<T> replace_if_constant(varsubexpr<T> vs) {
  return vs;
}

template <typename T, EnableIf<(!is_constant(T{}) || depends_on_inverse_variables(T{})) && !is_leaf(T{})> = 0>
constexpr T replace_if_constant(baseexpr<T>) {
  return {};
}






template <typename... Ts>
constexpr auto replace_var_subexprs(TypeList<Ts...>) {
  return List(replace_var_subexprs(Ts{})...);
}

struct has_single_dependent_t {
  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return vars(T{}).Size() == 1;
  }
};

constexpr has_single_dependent_t has_single_dependent{};

template <typename T, EnableIf<!is_constant(T{}) && !is_leaf(T{}) && is_scalarexpr(T{})> = 0>
constexpr auto replace_var_subexprs(baseexpr<T>) {
  return unpack<expr>(var_subexpr_tag, unpack_list<expr>(List(tag_of(T{})).Concat(replace_var_subexprs(operands(T{})))));
}

template <typename T, EnableIf<!is_constant(T{}) && !is_leaf(T{}) && !is_scalarexpr(T{})> = 0>
constexpr auto replace_var_subexprs(baseexpr<T>) {
  return unpack_list<expr>(List(tag_of(T{})).Concat(replace_var_subexprs(operands(T{}))));
}

constexpr auto combine_adjacent_product_var_subexprs(TypeList<>) {
  return List();
}

template <typename T, typename... Ts>
constexpr auto combine_adjacent_product_var_subexprs(TypeList<T, Ts...>) {
  return List(T{}).Concat(combine_adjacent_product_var_subexprs(List(Ts{}...)));
}

template <typename A, typename B, typename... Ts>
constexpr auto combine_adjacent_product_var_subexprs(TypeList<varsubexpr<A>, varsubexpr<B>, Ts...>) {
  constexpr auto combined = unpack<expr>(var_subexpr_tag, A{} * B{});
  return combine_adjacent_product_var_subexprs(List(combined, Ts{}...));
}

template <typename A, int B, int SeqIndex, typename... Ts>
constexpr auto combine_adjacent_product_var_subexprs(TypeList<varsubexpr<A>, variable<B, SeqIndex>, Ts...>) {
  constexpr auto combined = unpack<expr>(var_subexpr_tag, A{} * variable<B, SeqIndex>{});
  return combine_adjacent_product_var_subexprs(List(combined, Ts{}...));
}

template <int A, int SeqIndex, typename B, typename... Ts>
constexpr auto combine_adjacent_product_var_subexprs(TypeList<variable<A, SeqIndex>, varsubexpr<B>, Ts...>) {
  constexpr auto combined = unpack<expr>(var_subexpr_tag, variable<A, SeqIndex>{} * varsubexpr<B>{});
  return combine_adjacent_product_var_subexprs(List(combined, Ts{}...));
}

template <typename... Ts, EnableIf<!is_constant(productexpr<Ts...>{}) && !is_scalarexpr(productexpr<Ts...>{})> = 0>
constexpr auto replace_var_subexprs(productexpr<Ts...>) {
  constexpr auto operands = combine_adjacent_product_var_subexprs(replace_var_subexprs(List(Ts{}...)));
  return unpack_list<expr>(List(product_tag).Concat(operands));
}

template <typename... Ts>
constexpr auto replace_var_subexprs(branchexpr<Ts...>) {
  return unpack_list<expr>(List(branch_tag).Concat(replace_var_subexprs(List(Ts{}...))));
}

template <typename C, typename V>
constexpr auto replace_var_subexprs(branchcaseexpr<C, V>) {
  return unpack_list<expr>(List(branch_case_tag, C{}).Concat(replace_var_subexprs(V{})));
}

template <typename T, EnableIf<is_leaf(T{}) || is_constant(T{})> = 0>
constexpr T replace_var_subexprs(baseexpr<T>) {
  return {};
}

template <std::size_t I>
constexpr _size_t<I> replace_var_subexprs(_size_t<I>) {
  return {};
}

template <typename T, typename V, EnableIf<is_leaf(T{})> = 0>
constexpr T expand_var_subexpr(baseexpr<T>, baseexpr<V>) {
  return {};
}

template <typename V>
constexpr V expand_var_subexpr(varsubexpr<V>, baseexpr<V>) {
  return {};
}

template <typename... Ts, typename V>
constexpr auto expand_var_subexpr(TypeList<Ts...>, baseexpr<V>) {
  return List(expand_var_subexpr(Ts{}, V{})...);
}

template <typename T, typename V, EnableIf<!is_leaf(T{})> = 0>
constexpr auto expand_var_subexpr(baseexpr<T>, baseexpr<V>) {
  return make_expr(tag_of(T{}), expand_var_subexpr(operands(T{}), V{}));
}


template <typename T>
constexpr T expand_constant_subexprs(coordinate_system<T>) {
  return {};
}

template <std::size_t N>
constexpr auto expand_constant_subexprs(_size_t<N> s) {
  return s;
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr T expand_constant_subexprs(baseexpr<T>) {
  return {};
}

template <typename... Ts>
constexpr auto expand_constant_subexprs(TypeList<Ts...>) {
  return List(expand_constant_subexprs(Ts{})...);
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto expand_constant_subexprs(baseexpr<T>) {
  return unpack_list<expr>(List(tag_of(T{})).Concat(expand_constant_subexprs(operands(T{}))));
}

template <typename T>
constexpr auto expand_constant_subexprs(constantsubexpr<T>) {
  return expand_constant_subexprs(T{});
}



struct contains_var_subexpr_t {
  template <typename T, EnableIf<is_leaf(T{})> = 0>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }

  template <typename T>
  constexpr bool operator()(coordinate_system<T>) const {
    return false;
  }

  template <std::size_t I>
  constexpr bool operator()(refexpr<I>) const {
    return false;
  }

  template <int ID, int SeqIndex>
  constexpr bool operator()(variable<ID, SeqIndex>) const {
    return false;
  }

  template <std::size_t I>
  constexpr bool operator()(_size_t<I>) const {
    return false;
  }

  template <typename T>
  constexpr bool operator()(varsubexpr<T>) const {
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

  template <typename T>
  constexpr bool operator()(constantsubexpr<T>) const {
    return false;
  }
};

constexpr contains_var_subexpr_t contains_var_subexpr{};


template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr TypeSet<> collect_var_subexprs_recursive_impl(baseexpr<T>) {
  return {};
}
  
template <std::size_t I>
constexpr TypeSet<> collect_var_subexprs_recursive_impl(_size_t<I>) {
  return {};
}

template <typename T>
constexpr TypeSet<> collect_var_subexprs_recursive_impl(named_parameter<T>) {
  return {};
}

template <typename... Ts>
constexpr auto collect_var_subexprs_recursive_impl(TypeSet<Ts...>) {
  return MergeAll(collect_var_subexprs_recursive(Ts{})...);
}

template <typename... Ts>
constexpr auto collect_var_subexprs_recursive_impl(TypeList<Ts...>) {
  return MergeAll(collect_var_subexprs_recursive(Ts{})...);
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto collect_var_subexprs_recursive_impl(baseexpr<T>) {
  return collect_var_subexprs_recursive(operands(T{}));
}

template <typename T>
constexpr auto collect_var_subexprs_recursive_impl(varsubexpr<T>) {
  return make_type_set(T{}).Merge(collect_var_subexprs_recursive(T{}));
}

template <typename T, EnableIf<contains_var_subexpr(T{})> = 0>
constexpr auto collect_var_subexprs_recursive(T) {
  return collect_var_subexprs_recursive_impl(T{});
}

template <typename T, EnableIf<!contains_var_subexpr(T{})> = 0>
constexpr TypeSet<> collect_var_subexprs_recursive(T) {
  return {};
}

template <typename... Ts>
constexpr auto collect_var_subexprs_recursive_all(TypeList<Ts...>) {
  return List(collect_var_subexprs_recursive(Ts{})...);
}
  
  
  

template <typename T>
constexpr TypeSet<> collect_var_subexprs_impl(baseexpr<T>) {
  return {};
}

template <std::size_t I>
constexpr TypeSet<> collect_var_subexprs_impl(_size_t<I>) {
  return {};
}

template <typename T>
constexpr TypeSet<varsubexpr<T>> collect_var_subexprs_impl(varsubexpr<T>) {
  return {};
}

template <typename... Ts>
constexpr auto collect_var_subexprs_impl(TypeList<Ts...>) {
  return MergeAll(collect_var_subexprs_impl(Ts{})...);
}

template <typename... Ts>
constexpr auto collect_var_subexprs_impl(TypeSet<Ts...>) {
  return MergeAll(collect_var_subexprs_impl(Ts{})...);
}

template <typename T>
constexpr auto collect_var_subexprs(baseexpr<T>) {
  return collect_var_subexprs_impl(operands(T{}));
}

template <typename T>
constexpr auto collect_var_subexprs(varsubexpr<T>) {
  return make_type_set(T{});
}

template <typename... Ts>
constexpr auto collect_var_subexprs_all(TypeList<Ts...>) {
  return List(collect_var_subexprs(Ts{})...);
}



template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr TypeSet<> collect_constant_subexprs(baseexpr<T>) {
  return {};
}

template <std::size_t I>
constexpr TypeSet<> collect_constant_subexprs(_size_t<I>) {
  return {};
}

template <typename T>
constexpr TypeSet<> collect_constant_subexprs(named_parameter<T>) {
  return {};
}

template <typename... Ts>
constexpr auto collect_constant_subexprs(TypeSet<Ts...>) {
  return MergeAll(collect_constant_subexprs(Ts{})...);
}

template <typename... Ts>
constexpr auto collect_constant_subexprs(TypeList<Ts...>) {
  return MergeAll(collect_constant_subexprs(Ts{})...);
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto collect_constant_subexprs(baseexpr<T>) {
  return collect_constant_subexprs(operands(T{}));
}

template <typename T>
constexpr auto collect_constant_subexprs(constantsubexpr<T>) {
  return make_type_set(T{}).Merge(collect_constant_subexprs(T{}));
}



template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr T replace_common_subexprs_recursive(baseexpr<T>) {
  return {};
}

template <typename T>
constexpr T replace_common_subexprs_recursive(coordinate_system<T>) {
  return {};
}

template <std::size_t I>
constexpr _size_t<I> replace_common_subexprs_recursive(_size_t<I>) {
  return {};
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto replace_common_subexprs_recursive(baseexpr<T>) {
  return unpack<expr>(constant_subexpr_tag, unpack_list<expr>(List(tag_of(T{})).Concat(replace_common_subexprs_recursive(operands(T{})))));
}

template <typename... Ts>
constexpr auto replace_common_subexprs_recursive(TypeList<Ts...>) {
  return List(replace_common_subexprs_recursive(Ts{})...);
}

template <typename T>
constexpr constantsubexpr<T> replace_common_subexprs_recursive(constantsubexpr<T>) {
  return {};
}

template <std::size_t I>
constexpr auto named_params(_size_t<I>) {
  return make_type_set();
}

template <typename... Ts>
constexpr auto named_params(TypeSet<Ts...>) {
  return MergeAll(named_params(Ts{})...);
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr auto named_params(baseexpr<T>) {
  return make_type_set();
}

template <typename Tag, typename... Ts, EnableIf<!is_leaf(Tag{})> = 0>
constexpr auto named_params(expr<Tag, Ts...>) {
  return MergeAll(named_params(Ts{})...);
}


template <std::size_t I>
constexpr auto params(_size_t<I>) {
  return make_type_set();
}

template <typename... Ts>
constexpr auto params(TypeSet<Ts...>) {
  return MergeAll(params(Ts{})...);
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr auto params(baseexpr<T>) {
  return make_type_set();
}

template <typename Tag, typename... Ts, EnableIf<!is_leaf(Tag{})> = 0>
constexpr auto params(expr<Tag, Ts...>) {
  return MergeAll(params(Ts{})...);
}

template <typename T>
constexpr auto get_max_param_id(baseexpr<T>) {
  constexpr auto id = get_id(max(params(T{})));
  return _int<id>{};
}

template <typename T>
constexpr bool is_guaranteed_non_negative(baseexpr<T>) {
  return false;
}

template <std::size_t I>
constexpr auto values(_size_t<I>) {
  return make_type_set();
}

template <typename T>
constexpr auto values(coordinate_system<T>) {
  return make_type_set();
}

template <typename... Ts>
constexpr auto values(TypeSet<Ts...>) {
  return MergeAll(values(Ts{})...);
}

template <typename... Ts>
constexpr auto values(TypeList<Ts...>) {
  return MergeAll(values(Ts{})...);
}

template <typename T, EnableIf<is_leaf(T{})> = 0>
constexpr auto values(baseexpr<T>) {
  return make_type_set();
}

template <typename T, EnableIf<!is_leaf(T{})> = 0>
constexpr auto values(baseexpr<T>) {
  return values(operands(T{}));
}

template <int ID, std::size_t N, std::size_t I>
constexpr TypeSet<valueexpr<ID, N, I>> values(valueexpr<ID, N, I>) {
  return {};
}

template <typename T, EnableIf<(values(T{}).Size() > 0)> = 0>
constexpr auto get_max_value_id(baseexpr<T>) {
  constexpr auto id = get_id(max(values(T{})));
  return _int<id>{};
}

template <typename T, EnableIf<(values(T{}).Size() == 0)> = 0>
constexpr auto get_max_value_id(baseexpr<T>) {
  return _int<-1>{};
}


struct is_guaranteed_positive_t {
  template <typename... Ts>
  constexpr bool operator()(addexpr<Ts...>) const {
    return (this->operator()(Ts{}) && ... && true);
  }

  template <typename... Ts>
  constexpr bool operator()(productexpr<Ts...>) const {
    return (this->operator()(Ts{}) && ... && true);
  }

  template <typename B>
  constexpr bool operator()(powerexpr<B, literal<1, 2>>) const {
    return true;
  }

  template <typename B>
  constexpr bool operator()(powerexpr<B, literal<-1, 2>>) const {
    return true;
  }

  template <typename T>
  constexpr bool operator()(baseexpr<T>) const {
    return false;
  }
};

constexpr is_guaranteed_positive_t is_guaranteed_positive{};


} // end namespace aether

#endif
