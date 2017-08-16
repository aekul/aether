#ifndef TYPELIST_FIND_H
#define TYPELIST_FIND_H

#include "aether/fwd/typelist/Find.h"

#include "aether/typelist/TypeList.h"

namespace aether {

template <typename Pred>
constexpr auto find_t::impl(Pred, TypeList<>) const {
  static_assert(!std::is_same<Pred, Pred>::value, "No elements passed.");
  return 0;
}

template <typename Pred, typename T, typename... Ts, EnableIf<!Pred{}(T{})>>
constexpr auto find_t::impl(Pred pred, TypeList<T, Ts...>) const {
  return impl(pred, TypeList<Ts...>{});
}

template <typename Pred, typename T, typename... Ts, EnableIf<Pred{}(T{})>>
constexpr T find_t::impl(Pred pred, TypeList<T, Ts...>) const {
  return {};
}

template <typename Pred, typename... Ts>
constexpr auto find_t::operator()(Pred pred, TypeList<Ts...>) const {
  return impl(pred, TypeList<Ts...>{});
}

template <typename Pred, typename... Ts>
constexpr auto find_t::operator()(Pred pred, TypeSet<Ts...>) const {
  return impl(pred, TypeList<Ts...>{});
}


} // end namespace aether

#endif
