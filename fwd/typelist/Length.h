#ifndef FWD_TYPELIST_LENGTH_H
#define FWD_TYPELIST_LENGTH_H

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct length_t {
  template <typename... Ts>
  constexpr std::size_t operator()(TypeList<Ts...>) const;

  template <typename... Ts>
  constexpr std::size_t operator()(TypeSet<Ts...>) const;
};

//constexpr length_t length{};

} // end namespace aether

#endif
