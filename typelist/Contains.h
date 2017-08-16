#ifndef TYPELIST_CONTAINS_H
#define TYPELIST_CONTAINS_H

#include "aether/fwd/typelist/Contains.h"


namespace aether {

template <typename R, typename... Ts>
constexpr bool contains_t::operator()(R, TypeList<Ts...>) const {
  return (std::is_same<R, Ts>::value || ... || false);
}

template <typename R, typename... Ts>
constexpr bool contains_t::operator()(R, TypeSet<Ts...>) const {
  return has_key<TypeSet<Ts...>, R>::value;
}

template <typename R, typename... Ts>
constexpr bool contains_t::operator()(Type<R>, TypeSet<Ts...>) const {
  return has_key<TypeSet<Ts...>, R>::value;
}

} // end namespace aether

#endif
