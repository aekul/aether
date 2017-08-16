#ifndef FWD_TYPELIST_TYPELIST_H
#define FWD_TYPELIST_TYPELIST_H

#include "aether/fwd/typelist/Type.h"

namespace aether {

struct type_not_found_t {};
constexpr type_not_found_t type_not_found{};

template <typename... Ts>
struct TypeList;

template <typename... Ts>
struct TypeList {
  using Self = TypeList<Ts...>;

  template <typename... Rs>
  constexpr TypeList<Ts..., Rs...> Concat(const TypeList<Rs...>&) const;

  template <typename R>
  constexpr TypeList<Ts..., R> Concat(R) const;

  static constexpr int N = sizeof...(Ts);

  constexpr int Size() const;

  template <typename... Rs>
  constexpr bool operator==(TypeList<Rs...>) const;
};

using EmptyList = TypeList<>;

constexpr TypeList<> List(type_not_found_t);

template <typename... Ts>
constexpr TypeList<Ts...> List(Ts...);

} // end namespace aether

#endif
