#ifndef PARAM_H
#define PARAM_H

#include <utility>
#include <array>

#include "Expr.h"
#include "Literal.h"
#include <boost/hana.hpp>
#include "aether/Map.h"
#include "aether/fwd/typelist/TypeSet.h"

namespace aether {

NAMED_PARAM(reffn)

using namespace boost;

template <int ID, std::size_t N, std::size_t I>
struct boundparam {
  Real value;
};

template <int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr auto make_boundparam(param<ID, N, I, SeqIndex>) {
  return boundparam<ID, N, I>{};
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr int get_id(param<ID, N, I, SeqIndex>) {
  return ID;
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr bool less_than(param<ID, N, I, SeqIndex>, param<ID, N, I, SeqIndex>) {
  return false;
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex1, int SeqIndex2>
constexpr bool less_than(param<ID, N, I, SeqIndex1>, param<ID, N, I, SeqIndex2>) {
  return SeqIndex1 < SeqIndex2;
}

template <int ID, std::size_t N, std::size_t I1, int SeqIndex1, std::size_t I2, int SeqIndex2>
constexpr bool less_than(param<ID, N, I1, SeqIndex1>, param<ID, N, I2, SeqIndex2>) {
  return I1 < I2;
}

template <int ID, std::size_t N1, std::size_t I1, int SeqIndex1, std::size_t N2, std::size_t I2, int SeqIndex2>
constexpr bool less_than(param<ID, N1, I1, SeqIndex1>, param<ID, N2, I2, SeqIndex2>) {
  return N1 < N2;
}

template <int ID1, std::size_t N1, std::size_t I1, int SeqIndex1, int ID2, std::size_t N2, std::size_t I2, int SeqIndex2>
constexpr bool less_than(param<ID1, N1, I1, SeqIndex1>, param<ID2, N2, I2, SeqIndex2>) {
  return ID1 < ID2;
}

template <std::intmax_t A, std::intmax_t D, int ID1, std::size_t N1, std::size_t I1, int SeqIndex1, int ID2, std::size_t N2, std::size_t I2, int SeqIndex2>
constexpr bool less_than(productexpr<literal<A, D>, param<ID1, N1, I1, SeqIndex1>>, param<ID2, N2, I2, SeqIndex2>) {
  return less_than(param<ID1, N1, I1, SeqIndex1>{}, param<ID2, N2, I2, SeqIndex2>{});
}

template <int ID1, std::size_t N1, std::size_t I1, int SeqIndex1, std::intmax_t A, std::intmax_t D, int ID2, std::size_t N2, std::size_t I2, int SeqIndex2>
constexpr bool less_than(param<ID1, N1, I1, SeqIndex1>, productexpr<literal<A, D>, param<ID2, N2, I2, SeqIndex2>>) {
  return less_than(param<ID1, N1, I1, SeqIndex1>{}, param<ID2, N2, I2, SeqIndex2>{});
}

template <std::intmax_t A1, std::intmax_t D1, int ID1, std::size_t N1, std::size_t I1, int SeqIndex1, std::intmax_t A2, std::intmax_t D2, int ID2, std::size_t N2, std::size_t I2, int SeqIndex2>
constexpr bool less_than(productexpr<literal<A1, D1>, param<ID1, N1, I1, SeqIndex1>>, productexpr<literal<A2, D2>, param<ID2, N2, I2, SeqIndex2>>) {
  return less_than(param<ID1, N1, I1, SeqIndex1>{}, param<ID2, N2, I2, SeqIndex2>{});
}


template <int ID, std::size_t N, std::size_t I>
constexpr int get_id(valueexpr<ID, N, I>) {
  return ID;
}

template <int ID, std::size_t N, std::size_t I>
constexpr bool less_than(valueexpr<ID, N, I>, valueexpr<ID, N, I>) {
  return false;
}

template <int ID, std::size_t N, std::size_t I1, std::size_t I2>
constexpr bool less_than(valueexpr<ID, N, I1>, valueexpr<ID, N, I2>) {
  return I1 < I2;
}

template <int ID, std::size_t N1, std::size_t I1, std::size_t N2, std::size_t I2>
constexpr bool less_than(valueexpr<ID, N1, I1>, valueexpr<ID, N2, I2>) {
  return N1 < N2;
}

template <int ID1, std::size_t N1, std::size_t I1, int ID2, std::size_t N2, std::size_t I2>
constexpr bool less_than(valueexpr<ID1, N1, I1>, valueexpr<ID2, N2, I2>) {
  return ID1 < ID2;
}


struct get_seq_index_t {
  template <int ID, std::size_t N, std::size_t I, int SeqIndex>
  constexpr _int<SeqIndex> operator()(param<ID, N, I, SeqIndex>) const {
    return {};
  }

  template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex>
  constexpr _int<SeqIndex> operator()(named_param<T, ID, N, I, SeqIndex>) const {
    return {};
  }

  template <int ID, int SeqIndex>
  constexpr _int<SeqIndex> operator()(variable<ID, SeqIndex>) const {
    return {};
  }
};

constexpr get_seq_index_t get_seq_index{};

template <typename... Ts>
constexpr auto max_seq_index(TypeList<Ts...>) {
  return max(List(get_seq_index(Ts{})...));
}

struct is_param_t {
  template <int ID, std::size_t N, std::size_t I, int SeqIndex>
  constexpr bool operator()(param<ID, N, I, SeqIndex>) {
    return true;
  }

  template <typename T>
  constexpr bool operator()(T) {
    return false;
  }
};

constexpr is_param_t is_param{};

template <typename T, int ID>
struct name_and_id {};

template <typename T, int ID>
constexpr T get_name(name_and_id<T, ID>) {
  return {};
}

template <typename T, int ID>
constexpr int get_id(name_and_id<T, ID>) {
  return ID;
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr name_and_id<T, ID> get_name_and_id(named_param<T, ID, N, I, SeqIndex>) {
  return {};
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr T get_name(named_param<T, ID, N, I, SeqIndex>) {
  return {};
}

template <typename T>
struct named_param_name;

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex>
struct named_param_name<named_param<T, ID, N, I, SeqIndex>> {
  using type = T;
};

template <typename T>
using named_param_name_t = typename named_param_name<T>::type;

template <typename... Ts>
constexpr auto get_named_param_names(TypeSet<Ts...>) {
  return MergeAll(make_type_set(get_name(Ts{}))...);
}

template <int ID, int NewSeqIndex>
constexpr variable<ID, NewSeqIndex> update_seq_indices(variable<ID, -1>, _int<NewSeqIndex>) {
  return {};
}

template <int ID, int SeqIndex, int NewSeqIndex>
constexpr variable<ID, SeqIndex> update_seq_indices(variable<ID, SeqIndex>, _int<NewSeqIndex>) {
  return {};
}

template <int ID, std::size_t N, std::size_t I, int NewSeqIndex>
constexpr param<ID, N, I, NewSeqIndex> update_seq_indices(param<ID, N, I, -1>, _int<NewSeqIndex>) {
  return {};
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex, int NewSeqIndex>
constexpr param<ID, N, I, SeqIndex> update_seq_indices(param<ID, N, I, SeqIndex>, _int<NewSeqIndex>) {
  return {};
}

template <typename T, int ID, std::size_t N, std::size_t I, int NewSeqIndex>
constexpr named_param<T, ID, N, I, NewSeqIndex> update_seq_indices(named_param<T, ID, N, I, -1>, _int<NewSeqIndex>) {
  return {};
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex, int NewSeqIndex>
constexpr named_param<T, ID, N, I, SeqIndex> update_seq_indices(named_param<T, ID, N, I, SeqIndex>, _int<NewSeqIndex>) {
  return {};
}

template <typename... Ts, int SeqIndex>
constexpr auto update_seq_indices(TypeSet<Ts...>, _int<SeqIndex> n) {
  return List(update_seq_indices(Ts{}, n)...);
}

template <typename A, int SeqIndex>
constexpr auto get_constant_seq_index_replacements(baseexpr<A>, _int<SeqIndex> si) {
  constexpr auto params_and_vars = params(A{}).Merge(named_params(A{})).Merge(vars(A{}));
  return make_type_map(
    make_list(params_and_vars)
    , update_seq_indices(params_and_vars, si)
  );
}


template <int ID, int SeqIndex, int Adjustment>
constexpr variable<ID, SeqIndex + Adjustment> adjust_seq_indices(variable<ID, SeqIndex>, _int<Adjustment>) {
  return {};
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex, int Adjustment>
constexpr param<ID, N, I, SeqIndex + Adjustment> adjust_seq_indices(param<ID, N, I, SeqIndex>, _int<Adjustment>) {
  return {};
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex, int Adjustment>
constexpr named_param<T, ID, N, I, SeqIndex + Adjustment> adjust_seq_indices(named_param<T, ID, N, I, SeqIndex>, _int<Adjustment>) {
  return {};
}

template <typename... Ts, int Adjustment>
constexpr auto adjust_seq_indices(TypeSet<Ts...>, _int<Adjustment> n) {
  return List(adjust_seq_indices(Ts{}, n)...);
}

template <typename... Ps, int Adjustment>
constexpr auto get_constant_seq_index_adjustments(TypeSet<Ps...> params_and_vars, _int<Adjustment>) {
  return make_type_map(
    make_list(params_and_vars)
    , adjust_seq_indices(params_and_vars, _int<Adjustment + 1>{})
  );
}









template <int ID, std::size_t N, std::size_t I, int SeqIndex, int OldSeqIndex, int NewSeqIndex>
constexpr param<ID, N, I, SeqIndex> update_seq_indices(param<ID, N, I, SeqIndex>, _int<OldSeqIndex>, _int<NewSeqIndex>) {
  return {};
}

template <int ID, std::size_t N, std::size_t I, int OldSeqIndex, int NewSeqIndex>
constexpr param<ID, N, I, NewSeqIndex> update_seq_indices(param<ID, N, I, OldSeqIndex>, _int<OldSeqIndex>, _int<NewSeqIndex>) {
  return {};
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex, int OldSeqIndex, int NewSeqIndex>
constexpr named_param<T, ID, N, I, SeqIndex> update_seq_indices(named_param<T, ID, N, I, SeqIndex>, _int<OldSeqIndex>, _int<NewSeqIndex>) {
  return {};
}

template <typename T, int ID, std::size_t N, std::size_t I, int OldSeqIndex, int NewSeqIndex>
constexpr named_param<T, ID, N, I, NewSeqIndex> update_seq_indices(named_param<T, ID, N, I, OldSeqIndex>, _int<OldSeqIndex>, _int<NewSeqIndex>) {
  return {};
}


template <typename... Ts, int OldSeqIndex, int NewSeqIndex>
constexpr auto update_seq_indices(TypeSet<Ts...>, _int<OldSeqIndex> oi, _int<NewSeqIndex> ni) {
  return List(update_seq_indices(Ts{}, oi, ni)...);
}

template <typename A, int OldSeqIndex, int NewSeqIndex>
constexpr auto get_constant_seq_index_replacements(baseexpr<A>, _int<OldSeqIndex> oi, _int<NewSeqIndex> ni) {
  constexpr auto ps = params(A{}).Merge(named_params(A{}));
  return make_type_map(
    make_list(ps)
    , update_seq_indices(ps, oi, ni)
  );
}



template <int ID, std::size_t N, std::size_t I, int Adjustment>
constexpr valueexpr<ID + Adjustment, N, I> update_value_ids(valueexpr<ID, N, I>, _int<Adjustment>) {
  return {};
}

template <typename... Ts, int N>
constexpr auto update_value_ids(TypeSet<Ts...>, _int<N> n) {
  return List(update_value_ids(Ts{}, n)...);
}

template <typename A, typename B>
constexpr auto get_value_id_replacements(baseexpr<A>, baseexpr<B>) {
  constexpr auto vs = values(B{});
  constexpr auto id = get_max_value_id(A{}) + get_max_value_id(B{}) + _int<1>{};
  return make_type_map(
    make_list(vs)
    , update_value_ids(vs, id)
  );
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr auto vars(param<ID, N, I, SeqIndex>) {
  return TypeSet<>{};
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr auto inverse_vars(param<ID, N, I, SeqIndex>) {
  return TypeSet<>{};
}

template <int I>
constexpr bool is_int(_int<I>) {
  return true;
}

template <typename T>
constexpr bool is_int(T) {
  return false;
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr TypeSet<named_param<T, ID, N, I, SeqIndex>> named_params(named_param<T, ID, N, I, SeqIndex>) {
  return {};
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex>
constexpr TypeSet<param<ID, N, I, SeqIndex>> params(param<ID, N, I, SeqIndex>) {
  return {};
}

template <int ID, std::size_t N, std::size_t... Is>
constexpr auto make_value(std::index_sequence<Is...>) {
  return make_vector_expr(valueexpr<ID, N, Is>{}...);
}

template <int ID, std::size_t N, EnableIf<(N > 1)> = 0>
constexpr auto make_value() {
  return make_value<ID, N>(std::make_index_sequence<N>{});
}

template <int ID, std::size_t N, EnableIf<(N == 1)> = 0>
constexpr auto make_value() {
  return valueexpr<ID, 1, 0>{};
}


template <int ID, std::size_t N, std::size_t... Is>
constexpr auto make_param(std::index_sequence<Is...>) {
  return make_vector_expr(param<ID, N, Is>{}...);
}

template <int ID, std::size_t N, EnableIf<(N > 1)> = 0>
constexpr auto make_param() {
  return make_param<ID, N>(std::make_index_sequence<N>{});
}

template <int ID, std::size_t N, EnableIf<(N == 1)> = 0>
constexpr auto make_param() {
  return param<ID, 1, 0>{};
}


template <typename Name, int ID, std::size_t N, std::size_t... Is>
constexpr auto make_named_param(std::index_sequence<Is...>) {
  return make_vector_expr(named_param<Name, ID, N, Is>{}...);
}

template <typename Name, int ID, std::size_t N, EnableIf<(N > 1)> = 0>
constexpr auto make_named_param() {
  return make_named_param<Name, ID, N>(std::make_index_sequence<N>{});
}

template <typename Name, int ID, std::size_t N, EnableIf<(N == 1)> = 0>
constexpr auto make_named_param() {
  return named_param<Name, ID, 1, 0>{};
}

#define make_param(N) make_param<__COUNTER__, N>()


template <int ID, std::size_t N, typename Tuple>
constexpr auto constant_impl(const Tuple& values) {
  return make_random_var(make_param<ID, N>(), values);
}

template <int ID, std::size_t N, std::size_t... Is>
constexpr auto constant_impl(const std::array<Real, N>& values, std::index_sequence<Is...>) {
  return constant_impl<ID, N>(make_map(make_type_set(param<ID, N, Is>{}...), values[Is]...));
}

template <int ID, std::size_t... Is>
constexpr auto constant_impl(const Vector2& values, std::index_sequence<Is...>) {
  return constant_impl<ID, 2>(make_map(make_type_set(param<ID, 2, Is>{}...), values[Is]...));
}

template <int ID, std::size_t... Is>
constexpr auto constant_impl(const Vector3& values, std::index_sequence<Is...>) {
  return constant_impl<ID, 3>(make_map(make_type_set(param<ID, 3, Is>{}...), values[Is]...));
}

template <int ID, std::size_t N>
constexpr auto constant_impl(const std::array<Real, N>& values) {
  return constant_impl<ID>(values, std::make_index_sequence<N>{});
}

template <int ID>
constexpr auto constant_impl(const Vector2& values) {
  return constant_impl<ID>(values, std::make_index_sequence<2>{});
}

template <int ID>
constexpr auto constant_impl(const Vector3& values) {
  return constant_impl<ID>(values, std::make_index_sequence<3>{});
}

template <int ID>
constexpr auto constant_impl(Real value) {
  constexpr auto p = make_param<ID, 1>();
  return make_random_var(p, make_map(make_type_set(param<ID, 1, 0>{}), value));
}




template <typename Name, int ID, std::size_t N, typename Tuple>
constexpr auto named_constant_impl(const Tuple& values) {
  return make_random_var(make_named_param<Name, ID, N>(), values);
}

template <typename Name, int ID, std::size_t N, std::size_t... Is>
constexpr auto named_constant_impl(const std::array<Real, N>& values, std::index_sequence<Is...>) {
  return named_constant_impl<Name, ID, N>(make_map(make_type_set(named_param<Name, ID, N, Is>{}...), values[Is]...));
}

template <typename Name, int ID, std::size_t... Is>
constexpr auto named_constant_impl(const Vector3& values, std::index_sequence<Is...>) {
  return named_constant_impl<Name, ID, 3>(make_map(make_type_set(named_param<Name, ID, 3, Is>{}...), values[Is]...));
}

template <typename Name, int ID, std::size_t N>
constexpr auto named_constant_impl(const std::array<Real, N>& values) {
  return named_constant_impl<Name, ID>(values, std::make_index_sequence<N>{});
}

template <typename Name, int ID>
constexpr auto named_constant_impl(const Vector3& values) {
  return named_constant_impl<Name, ID>(values, std::make_index_sequence<3>{});
}

template <typename Name, int ID>
constexpr auto named_constant_impl(Real value) {
  constexpr auto p = make_named_param<Name, ID, 1>();
  return make_random_var(p, make_map(make_type_set(p), value));
}

template <int ID, typename Name, typename T>
constexpr auto named_constant(Name, T&& t) {
  return named_constant_impl<Name, ID>(std::forward<T>(t));
}



template <int ID, std::size_t N, typename Tuple>
constexpr auto value_impl(const Tuple& values) {
  return make_random_var(make_value<ID, N>(), values);
}

template <int ID, std::size_t N, std::size_t... Is>
constexpr auto value_impl(const std::array<Real, N>& values, std::index_sequence<Is...>) {
  return value_impl<ID, N>(make_map(make_type_set(valueexpr<ID, N, Is>{}...), values[Is]...));
}

template <int ID, std::size_t... Is>
constexpr auto value_impl(const Vector3& values, std::index_sequence<Is...>) {
  return value_impl<ID, 3>(make_map(make_type_set(valueexpr<ID, 3, Is>{}...), values[Is]...));
}

template <int ID, std::size_t N>
constexpr auto value_impl(const std::array<Real, N>& values) {
  return value_impl<ID>(values, std::make_index_sequence<N>{});
}

template <int ID>
constexpr auto value_impl(const Vector3& values) {
  return value_impl<ID>(values, std::make_index_sequence<3>{});
}

template <int ID>
constexpr auto value_impl(Real v) {
  constexpr auto p = make_value<ID, 1>();
  return make_random_var(p, make_map(make_type_set(valueexpr<ID, 1, 0>{}), v));
}

template <int ID>
constexpr auto value_impl(bool v) {
  return value_impl<ID>(Real(v));
}

template <typename T>
constexpr auto value(T&& t) {
  return value_impl<0>(std::forward<T>(t));
}

template <std::size_t N, std::size_t... Is>
constexpr auto make_ref(std::index_sequence<Is...>) {
  return make_vector_expr(variable<-N + int(Is)>{}...);
}

template <std::size_t N, EnableIf<(N > 1)> = 0>
constexpr auto make_ref() {
  return make_ref<N>(std::make_index_sequence<N>{});
}

template <std::size_t N, EnableIf<(N == 1)> = 0>
constexpr auto make_ref() {
  return variable<-1>{};
}

template <std::size_t N, typename Tuple, typename Fn>
constexpr auto ref_impl(const Tuple& values, Fn&& fn) {
  return make_random_var(true, make_ref<N>(), values, hana::make_tuple(reffn_ = std::forward<Fn>(fn))).Sample();
}

template <std::size_t N, typename Fn, std::size_t... Is>
constexpr auto ref_impl(const std::array<Real, N>& values, Fn&& fn, std::index_sequence<Is...>) {
  return ref_impl<N>(make_map(make_type_set(variable<-N + int(Is)>{}...), values[Is]...), std::forward<Fn>(fn));
}

template <std::size_t N, typename Fn, std::size_t... Is>
constexpr auto ref_impl(Real value, Fn&& fn, std::index_sequence<Is...>) {
  return ref_impl<N>(make_map(make_type_set(variable<-1>{}), value), std::forward<Fn>(fn));
}

template <std::size_t N, typename T, typename Fn>
constexpr auto ref(T&& t, Fn&& fn) {
  return ref_impl<N>(std::forward<T>(t), std::forward<Fn>(fn), std::make_index_sequence<N>{});
}

template <int ID, typename T>
constexpr auto make_value(T&& t) {
  return value_impl<ID>(std::forward<T>(t));
}

template <typename T>
constexpr auto make_value(T&& t) {
  return value_impl<0>(std::forward<T>(t));
}

template <typename T>
constexpr auto value_(T&& t) {
  return value_impl<0>(std::forward<T>(t));
}


#define constant constant_impl<__COUNTER__>

} // end namespace aether

#endif
