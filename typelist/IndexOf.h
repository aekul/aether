#ifndef TYPELIST_INDEX_OF_H
#define TYPELIST_INDEX_OF_H

#include "aether/fwd/typelist/IndexOf.h"

namespace aether {

template <typename R, typename... Ts>
constexpr std::size_t index_of_t::operator()(R, TypeList<Ts...>) const {
  constexpr bool same[] = {std::is_same<R, Ts>::value...};

  for (std::size_t i = 0; i < sizeof...(Ts); ++i) {
    if (same[i]) {
      return i;
    }
  }

  return -1;
}

template <typename R, typename... Ts>
constexpr std::size_t index_of_t::operator()(Type<R>, TypeList<Ts...>) const {
  return this->operator()<R>(TypeList<Ts...>{});
}

template <typename R, typename... Ts>
constexpr std::size_t index_of_t::operator()(R, TypeSet<Ts...>) const {
  return this->operator()(R{}, TypeList<Ts...>{});
}


template <typename R, typename... Ts>
constexpr std::size_t index_of_t::operator()(TypeList<Ts...>) const {
  constexpr bool same[] = {std::is_same<R, Ts>::value...};

  for (std::size_t i = 0; i < sizeof...(Ts); ++i) {
    if (same[i]) {
      return i;
    }
  }

  return -1;
}

template <typename R, typename... Ts>
constexpr std::size_t index_of_t::operator()(Type<R>, TypeSet<Ts...>) const {
  return this->operator()<R>(TypeList<Ts...>{});
}

template <typename R, typename... Ts>
constexpr std::size_t index_of_t::operator()(TypeSet<Ts...>) const {
  return this->operator()<R>(TypeList<Ts...>{});
}

template <typename R, typename... Ts>
constexpr auto index_of_all(R item, TypeList<Ts...>) {
  return List(_size_t<index_of(item, make_list(Ts{}))>{}...);
}

} // end namespace aether

#endif
