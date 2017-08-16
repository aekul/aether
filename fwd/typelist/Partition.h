#ifndef FWD_TYPELIST_PARTITION_H
#define FWD_TYPELIST_PARTITION_H

#include "../TypeTraits.h"
#include "TypeList.h"

namespace aether {

template <typename Pred, typename... Pass, typename... Fail, std::size_t I>
constexpr auto partition_impl(Pred, TypeList<>, TypeList<Pass...>, TypeList<Fail...>, _size_t<I>);

template <typename Pred, typename T, typename... Ts, typename... Pass, typename... Fail, EnableIf<Pred{}(T{})> = 0>
constexpr auto partition_impl(Pred, TypeList<T, Ts...>, TypeList<Pass...>, TypeList<Fail...>, _size_t<1>);

template <typename Pred, typename T, typename... Ts, typename... Pass, typename... Fail, std::size_t I, EnableIf<Pred{}(T{})> = 0>
constexpr auto partition_impl(Pred pred, TypeList<T, Ts...>, TypeList<Pass...>, TypeList<Fail...>, _size_t<I>);

template <typename Pred, typename T, typename... Ts, typename... Pass, typename... Fail, std::size_t I, EnableIf<!Pred{}(T{})> = 0>
constexpr auto partition_impl(Pred pred, TypeList<T, Ts...>, TypeList<Pass...>, TypeList<Fail...>, _size_t<I>);

template <typename Pred, typename... Ts, std::size_t I>
constexpr auto partition(Pred pred, TypeList<Ts...>, _size_t<I>);

template <typename Pred, typename... Ts>
constexpr auto partition(Pred pred, TypeList<Ts...>);

} // end namespace aether

#endif

