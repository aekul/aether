#ifndef TYPELIST_REMOVE_FIRST_H
#define TYPELIST_REMOVE_FIRST_H

#include "aether/fwd/typelist/RemoveFirst.h"

#include "aether/typelist/IndexOf.h"
#include "aether/typelist/Remove.h"

namespace aether {

template <typename T, typename... Ts>
constexpr auto remove_first_t::operator()(T, TypeList<Ts...>) const {
  return remove_at(_size_t<index_of(T{}, TypeList<Ts...>{})>{}, _size_t<1>{}, TypeList<Ts...>{});
}

} // end namespace aether

#endif
