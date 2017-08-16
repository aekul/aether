#ifndef TYPELIST_CONCAT_H
#define TYPELIST_CONCAT_H

#include "aether/fwd/typelist/Concat.h"

namespace aether {

template <typename... Ts, typename... Rs, typename... Os>
constexpr auto concat_t::operator()(TypeList<Ts...>, TypeList<Rs...>, Os...) const {
  return this->operator()(TypeList<Ts..., Rs...>{}, Os{}...); 
}

template <typename... Ts>
constexpr TypeList<Ts...> concat_t::operator()(TypeList<Ts...>) const {
  return {}; 
}

} // end namespace aether

#endif
