#ifndef FWD_TYPELIST_SORT_H
#define FWD_TYPELIST_SORT_H

#include "aether/fwd/TypeTraits.h"
#include "aether/fwd/typelist/TypeList.h"
#include "aether/fwd/typelist/TypeSet.h"

namespace aether {

struct sort_t {
  template <typename R>
  constexpr TypeList<R> insert(R, TypeList<>) const;

  template <typename R, typename T, EnableIf<!less_than(R{}, T{})> = 0>
  constexpr auto insert(R, TypeList<T>) const;

  template <typename R, typename T, EnableIf<less_than(R{}, T{})> = 0>
  constexpr auto insert(R, TypeList<T>) const;

  template <typename R, typename T, typename... Ts, EnableIf<(less_than(R{}, T{}) && sizeof...(Ts) > 0)> = 0>
  constexpr auto insert(R, TypeList<T, Ts...>) const;

  template <typename R, typename T0, typename T1, typename... Ts, EnableIf<(less_than(R{}, T1{}) && !less_than(R{}, T0{}))> = 0>
  constexpr auto insert(R, TypeList<T0, T1, Ts...>) const;

  template <typename R, typename T0, typename T1, typename... Ts, EnableIf<(!less_than(R{}, T1{}))> = 0>
  constexpr auto insert(R, TypeList<T0, T1, Ts...>) const;

  template <typename R, typename T0, typename T1, typename T2, typename T3, typename... Ts, EnableIf<(less_than(R{}, T3{}) && !less_than(R{}, T1{}))> = 0>
  constexpr auto insert(R, TypeList<T0, T1, T2, T3, Ts...>) const;

  template <typename R, typename T0, typename T1, typename T2, typename T3, typename... Ts, EnableIf<(!less_than(R{}, T3{}))> = 0>
  constexpr auto insert(R, TypeList<T0, T1, T2, T3, Ts...>) const;

  template <typename R, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts, EnableIf<(less_than(R{}, T7{}) && !less_than(R{}, T3{}))> = 0>
  constexpr auto insert(R, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, Ts...>) const;

  template <typename R, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts, EnableIf<(!less_than(R{}, T7{}))> = 0>
  constexpr auto insert(R, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, Ts...>) const;

  template <typename R, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10, typename T11, typename T12, typename T13, typename T14, typename T15, typename... Ts, EnableIf<(less_than(R{}, T15{}) && !less_than(R{}, T7{}))> = 0>
  constexpr auto insert(R, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Ts...>) const;

  template <typename R, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10, typename T11, typename T12, typename T13, typename T14, typename T15, typename... Ts, EnableIf<(!less_than(R{}, T15{}))> = 0>
  constexpr auto insert(R, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Ts...>) const;

  template <typename... Sorted>
  constexpr TypeList<Sorted...> impl(TypeList<Sorted...>, TypeList<>) const;

  template <typename... Sorted, typename T, typename... Ts>
  constexpr auto impl(TypeList<Sorted...>, TypeList<T, Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(TypeList<Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(TypeSet<Ts...>) const;

  template <typename... Ts>
  constexpr auto operator()(Ts...) const;
};

constexpr sort_t sort{};

} // end namespace aether

#endif
