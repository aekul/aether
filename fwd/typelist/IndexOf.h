#ifndef FWD_TYPELIST_INDEX_OF_H
#define FWD_TYPELIST_INDEX_OF_H

#include "aether/fwd/typelist/TypeList.h"
#include "aether/fwd/typelist/TypeSet.h"
#include "aether/fwd/TypeTraits.h"

namespace aether {

struct index_of_t {
  template <typename R, typename... Ts>
  constexpr std::size_t operator()(R, TypeList<Ts...>) const;

  template <typename R, typename... Ts>
  constexpr std::size_t operator()(Type<R>, TypeList<Ts...>) const;

  template <typename R, typename... Ts>
  constexpr std::size_t operator()(R, TypeSet<Ts...>) const;

  template <typename R, typename... Ts>
  constexpr std::size_t operator()(TypeList<Ts...>) const;

  template <typename R, typename... Ts>
  constexpr std::size_t operator()(Type<R>, TypeSet<Ts...>) const;

  template <typename R, typename... Ts>
  constexpr std::size_t operator()(TypeSet<Ts...>) const;
};

constexpr index_of_t index_of{};

} // end namespace aether

#endif
