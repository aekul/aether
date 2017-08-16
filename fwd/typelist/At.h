#ifndef FWD_TYPELIST_AT_H
#define FWD_TYPELIST_AT_H

#include "aether/fwd/TypeTraits.h"

#include "aether/fwd/typelist/TypeList.h"
#include "aether/fwd/typelist/TypeSet.h"

namespace aether {

#define AT_PAIRS

template <std::size_t I>
constexpr auto at(_size_t<I>, TypeList<>);

template <typename T, typename... Ts>
constexpr auto at(_size_t<0>, TypeList<T, Ts...>);

template <std::size_t I, typename... Ts, EnableIf<(I > 0)> = 0>
constexpr auto at(_size_t<I>, TypeList<Ts...>);

template <std::size_t I, typename... Ts>
constexpr auto at(TypeList<Ts...> list);

template <std::size_t I, std::size_t J, typename... Ts>
constexpr auto at(TypeList<Ts...> list);

template <std::size_t I, typename... Ts>
constexpr auto at(TypeSet<Ts...>);

template <std::size_t... Is, typename... Ts>
constexpr auto at_all(TypeList<Ts...>);

} // end namespace aether

#endif
