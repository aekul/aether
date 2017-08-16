#ifndef TYPELIST_BACK_H
#define TYPELIST_BACK_H

#include "aether/fwd/typelist/Back.h"
#include "aether/typelist/At.h"

namespace aether {

template <typename... Ts>
constexpr auto back_t::operator()(TypeList<Ts...> list) const {
  return at<sizeof...(Ts) - 1>(list);
}

template <typename... Ts>
constexpr auto back_t::operator()(TypeSet<Ts...>) const {
  return this->operator()(TypeList<Ts...>{});
}

} // end namespace aether

#endif
