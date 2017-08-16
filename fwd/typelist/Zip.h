#ifndef FWD_TYPELIST_ZIP_H
#define FWD_TYPELIST_ZIP_H

#include "TypeList.h"

namespace aether {

struct zip_t {
  template <typename... Rs, typename... Ts>
  constexpr auto operator()(TypeList<Rs...>, TypeList<Ts...>) const;
};

constexpr zip_t zip{};

} // end namespace aether

#endif
