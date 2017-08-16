#ifndef FWD_TYPELIST_MATCH_H
#define FWD_TYPELIST_MATCH_H

#include "TypeList.h"

namespace aether {

struct wildcard_t {};

template <typename T>
struct match {
  constexpr bool operator()(T) const;

  template <typename S>
  constexpr bool operator()(S) const;
};

template <>
struct match<wildcard_t> {
  template <typename T>
  constexpr bool operator()(T) const;
};

template <typename... Ts>
struct match<TypeList<Ts...>> {
  constexpr bool impl(TypeList<>, TypeList<>) const;

  template <typename P, typename... Ps, typename R, typename... Rs>
  constexpr bool impl(TypeList<P, Ps...>, TypeList<R, Rs...>) const;

  template <typename... Rs, EnableIf<(sizeof...(Ts) != sizeof...(Rs))> = 0>
  constexpr bool operator()(TypeList<Rs...>) const;

  template <typename... Rs, EnableIf<(sizeof...(Ts) == sizeof...(Rs))> = 0>
  constexpr bool operator()(TypeList<Rs...>) const;

  template <typename R>
  constexpr bool operator()(R) const;
};

} // end namespace aether

#endif
