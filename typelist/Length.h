#ifndef TYPELIST_LENGTH_H
#define TYPELIST_LENGTH_H

#include "aether/fwd/typelist/Length.h"

namespace aether {

template <typename... Ts>
constexpr std::size_t length_t::operator()(TypeList<Ts...>) const {
  return sizeof...(Ts);
};

template <typename... Ts>
constexpr std::size_t length_t::operator()(TypeSet<Ts...>) const {
  return sizeof...(Ts);
};

} // end namespace aether

#endif
