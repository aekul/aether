#ifndef TYPELIST_REMOVE_BACK_H
#define TYPELIST_REMOVE_BACK_H

#include "../fwd/typelist/RemoveBack.h"

#include "../typelist/RemoveFront.h"
#include "../typelist/Reverse.h"

namespace aether {

template <typename... Ts>
constexpr auto remove_back_t::operator()(TypeList<Ts...>) const {
  return reverse(remove_front(reverse(TypeList<Ts...>{})));
}

template <typename... Ts>
constexpr auto remove_back_t::operator()(TypeSet<Ts...>) const {
  return make_type_set(reverse(remove_front(reverse(TypeList<Ts...>{}))));
}

} // end namespace aether

#endif
