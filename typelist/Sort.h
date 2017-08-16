#ifndef TYPELIST_SORT_H
#define TYPELIST_SORT_H

#include "aether/fwd/typelist/Sort.h"

#include "aether/typelist/TypeList.h"
#include "aether/fwd/typelist/TypeSet.h"

namespace aether {

template <typename... Sorted>
constexpr TypeList<Sorted...> sort_t::impl(TypeList<Sorted...>, TypeList<>) const {
  return {};
}

template <typename R>
constexpr TypeList<R> sort_t::insert(R, TypeList<>) const {
  return {};
}

template <typename R, typename T, EnableIf<!less_than(R{}, T{})>>
constexpr auto sort_t::insert(R, TypeList<T>) const {
  return TypeList<T, R>{};
}

template <typename R, typename T, EnableIf<less_than(R{}, T{})>>
constexpr auto sort_t::insert(R, TypeList<T>) const {
  return TypeList<R, T>{};
}

template <typename R, typename T, typename... Ts, EnableIf<(less_than(R{}, T{}) && sizeof...(Ts) > 0)>>
constexpr auto sort_t::insert(R, TypeList<T, Ts...>) const {
  return TypeList<R, T, Ts...>{};
}

template <typename R, typename T0, typename T1, typename... Ts, EnableIf<(less_than(R{}, T1{}) && !less_than(R{}, T0{}))>>
constexpr auto sort_t::insert(R, TypeList<T0, T1, Ts...>) const {
  return TypeList<T0, R, T1, Ts...>{};
}

template <typename R, typename T0, typename T1, typename... Ts, EnableIf<(!less_than(R{}, T1{}))>>
constexpr auto sort_t::insert(R, TypeList<T0, T1, Ts...>) const {
  return TypeList<T0, T1>{}.Concat(insert(R{}, TypeList<Ts...>{}));
}

template <typename R, typename T0, typename T1, typename T2, typename T3, typename... Ts, EnableIf<(less_than(R{}, T3{}) && !less_than(R{}, T1{}))>>
constexpr auto sort_t::insert(R, TypeList<T0, T1, T2, T3, Ts...>) const {
  return TypeList<T0, T1>{}.Concat(insert(R{}, TypeList<T2>{})).Concat(TypeList<T3, Ts...>{});
}

template <typename R, typename T0, typename T1, typename T2, typename T3, typename... Ts, EnableIf<(!less_than(R{}, T3{}))>>
constexpr auto sort_t::insert(R, TypeList<T0, T1, T2, T3, Ts...>) const {
  return TypeList<T0, T1, T2, T3>{}.Concat(insert(R{}, TypeList<Ts...>{}));
}

template <typename R, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts, EnableIf<(less_than(R{}, T7{}) && !less_than(R{}, T3{}))>>
constexpr auto sort_t::insert(R, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, Ts...>) const {
  return TypeList<T0, T1, T2, T3>{}.Concat(insert(R{}, TypeList<T4, T5, T6>{})).Concat(TypeList<T7, Ts...>{});
}

template <typename R, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts, EnableIf<(!less_than(R{}, T7{}))>>
constexpr auto sort_t::insert(R, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, Ts...>) const {
  return TypeList<T0, T1, T2, T3, T4, T5, T6, T7>{}.Concat(insert(R{}, TypeList<Ts...>{}));
}

template <typename R, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10, typename T11, typename T12, typename T13, typename T14, typename T15, typename... Ts, EnableIf<(less_than(R{}, T15{}) && !less_than(R{}, T7{}))>>
constexpr auto sort_t::insert(R, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Ts...>) const {
  return TypeList<T0, T1, T2, T3, T4, T5, T6, T7>{}.Concat(insert(R{}, TypeList<T8, T9, T10, T11, T12, T13, T14>{})).Concat(TypeList<T15, Ts...>{});
}

template <typename R, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10, typename T11, typename T12, typename T13, typename T14, typename T15, typename... Ts, EnableIf<(!less_than(R{}, T15{}))>>
constexpr auto sort_t::insert(R, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Ts...>) const {
  return TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15>{}.Concat(insert(R{}, TypeList<Ts...>{}));
}

template <typename... Sorted, typename T, typename... Ts>
constexpr auto sort_t::impl(TypeList<Sorted...>, TypeList<T, Ts...>) const {
  return impl(insert(T{}, TypeList<Sorted...>{}), TypeList<Ts...>{});
}

template <typename... Ts>
constexpr auto sort_t::operator()(TypeList<Ts...> list) const {
  return impl(List(), list);
}

template <typename... Ts>
constexpr auto sort_t::operator()(TypeSet<Ts...>) const {
  return impl(List(), TypeList<Ts...>{});
}

template <typename... Ts>
constexpr auto sort_t::operator()(Ts...) const {
  return impl(List(), TypeList<Ts...>{});
}

} // end namespace aether

#endif
