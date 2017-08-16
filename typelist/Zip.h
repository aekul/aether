#ifndef TYPELIST_ZIP_H
#define TYPELIST_ZIP_H

#include "aether/fwd/typelist/Zip.h"

#include "aether/typelist/TypeList.h"

namespace aether {

template <typename... Rs, typename... Ts>
constexpr auto zip_t::operator()(TypeList<Rs...>, TypeList<Ts...>) const {
  static_assert(sizeof...(Rs) == sizeof...(Ts), "Zip requires lists of equal length");
  return List(List(Rs{}, Ts{})...);
}

} // end namespace aether

#endif
