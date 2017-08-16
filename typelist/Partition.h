#ifndef TYPELIST_PARTITION_H
#define TYPELIST_PARTITION_H

#include "../fwd/typelist/Partition.h"

namespace aether {

template <typename Pred, typename... Pass, typename... Fail, std::size_t I>
constexpr auto partition_impl(Pred, TypeList<>, TypeList<Pass...>, TypeList<Fail...>, _size_t<I>) {
  return List(TypeList<Pass...>{}, TypeList<Fail...>{});
}

template <typename Pred, typename T, typename... Ts, typename... Pass, typename... Fail, EnableIf<Pred{}(T{})>>
constexpr auto partition_impl(Pred, TypeList<T, Ts...>, TypeList<Pass...>, TypeList<Fail...>, _size_t<1>) {
  return List(TypeList<Pass..., T>{}, TypeList<Fail..., Ts...>{});
}

template <typename Pred, typename T, typename... Ts, typename... Pass, typename... Fail, std::size_t I, EnableIf<Pred{}(T{})>>
constexpr auto partition_impl(Pred, TypeList<T, Ts...>, TypeList<Pass...>, TypeList<Fail...>, _size_t<I>) {
  return partition_impl(Pred{}, TypeList<Ts...>{}, TypeList<Pass..., T>{}, TypeList<Fail...>{}, _size_t<I - 1>{});
}

template <typename Pred, typename T, typename... Ts, typename... Pass, typename... Fail, std::size_t I, EnableIf<!Pred{}(T{})>>
constexpr auto partition_impl(Pred, TypeList<T, Ts...>, TypeList<Pass...>, TypeList<Fail...>, _size_t<I>) {
  return partition_impl(Pred{}, TypeList<Ts...>{}, TypeList<Pass...>{}, TypeList<Fail..., T>{}, _size_t<I>{});
}

template <typename Pred, typename... Ts, std::size_t I>
constexpr auto partition(Pred, TypeList<Ts...>, _size_t<I>) {
  static_assert(I >= 1, "Partition size must be greater than or equal to one.");
  return partition_impl(Pred{}, TypeList<Ts...>{}, TypeList<>{}, TypeList<>{}, _size_t<I>{});
}

template <typename Pred, typename... Ts>
constexpr auto partition(Pred, TypeList<Ts...>) {
  return partition_impl(Pred{}, TypeList<Ts...>{}, TypeList<>{}, TypeList<>{}, _size_t<sizeof...(Ts)>{});
}

} // end namespace aether

#endif

