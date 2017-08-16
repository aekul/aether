#ifndef TYPELIST_MATCH_H
#define TYPELIST_MATCH_H

#include "aether/fwd/typelist/Match.h"

#include "aether/typelist/At.h"

namespace aether {


template <typename T>
constexpr bool match<T>::operator()(T) const {
  return true; 
}

template <typename T>
template <typename S>
constexpr bool match<T>::operator()(S) const {
  return false; 
}

template <typename T>
constexpr bool match<wildcard_t>::operator()(T) const {
  return true; 
}

template <typename... Ts>
constexpr bool match<TypeList<Ts...>>::impl(TypeList<>, TypeList<>) const {
  return true;
}

template <typename... Ts>
template <typename P, typename... Ps, typename R, typename... Rs>
constexpr bool match<TypeList<Ts...>>::impl(TypeList<P, Ps...>, TypeList<R, Rs...>) const {
  return match<P>{}(R{}) && impl(TypeList<Ps...>{}, TypeList<Rs...>{});
}

template <typename... Ts>
template <typename... Rs, EnableIf<(sizeof...(Ts) != sizeof...(Rs))>>
constexpr bool match<TypeList<Ts...>>::operator()(TypeList<Rs...>) const {
  return false;
}

template <typename... Ts>
template <typename... Rs, EnableIf<(sizeof...(Ts) == sizeof...(Rs))>>
constexpr bool match<TypeList<Ts...>>::operator()(TypeList<Rs...>) const {
  return impl(TypeList<Ts...>{}, TypeList<Rs...>{});
}

template <typename... Ts>
template <typename R>
constexpr bool match<TypeList<Ts...>>::operator()(R) const {
  return false;
}

} // end namespace aether

#endif
