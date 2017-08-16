#ifndef FWD_TYPELIST_CONCAT_H
#define FWD_TYPELIST_CONCAT_H

#include "TypeList.h"

namespace aether {

struct concat_t {
  template <typename... Ts, typename... Rs, typename... Os>
  constexpr auto operator()(TypeList<Ts...>, TypeList<Rs...>, Os...) const;

  template <typename... Ts>
  constexpr TypeList<Ts...> operator()(TypeList<Ts...>) const;
};

constexpr concat_t concat{};

} // end namespace aether

#endif
