#ifndef FWD_TYPELIST_APPEND_H
#define FWD_TYPELIST_APPEND_H

#include "TypeList.h"

namespace aether {

struct append_t {
  template <typename... Ts, typename T>
  constexpr TypeList<Ts..., T> operator()(TypeList<Ts...>, T) const;
};

constexpr append_t append{};

} // end namespace aether

#endif
