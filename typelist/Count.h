#ifndef TYPELIST_COUNT_H
#define TYPELIST_COUNT_H

#include "aether/fwd/typelist/Count.h"

namespace aether {

template <typename Pred, typename... Ts>
constexpr int count_t::operator()(Pred pred, TypeList<Ts...>) const {
  bool pass[] = {pred(Ts{})...};
  int result = 0;
  for (bool p : pass) {
    if (p) {
      ++result;
    }
  }
  return result;
}


} // end namespace aether

#endif
