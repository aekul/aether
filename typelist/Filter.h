#ifndef TYPELIST_FILTER_H
#define TYPELIST_FILTER_H

#include "aether/fwd/typelist/Filter.h"

#include "aether/typelist/TypeList.h"

namespace aether {

template <typename Pred>
constexpr auto filter_t::impl(Pred, TypeList<>) const {
  return List();
}

template <typename Pred, typename T, typename... Ts, EnableIf<!Pred{}(T{})>>
constexpr auto filter_t::impl(Pred pred, TypeList<T, Ts...>) const {
  return impl(pred, TypeList<Ts...>{});
}

template <typename Pred, typename T, typename... Ts, EnableIf<Pred{}(T{})>>
constexpr auto filter_t::impl(Pred pred, TypeList<T, Ts...>) const {
  return List(T{}).Concat(impl(pred, TypeList<Ts...>{}));
}

template <typename Pred, typename... Ts>
constexpr auto filter_t::operator()(Pred pred, TypeList<Ts...>) const {
  return impl(pred, TypeList<Ts...>{});
}

template <typename Pred, typename... Ts>
constexpr auto filter_t::operator()(Pred pred, TypeSet<Ts...>) const {
  return make_type_set(impl(pred, TypeList<Ts...>{}));
}


template <typename Pred>
constexpr auto remove_if_t::impl(Pred, TypeList<>) const {
  return List();
}

template <typename Pred, typename T, typename... Ts, EnableIf<Pred{}(T{})>>
constexpr auto remove_if_t::impl(Pred pred, TypeList<T, Ts...>) const {
  return impl(pred, TypeList<Ts...>{});
}

template <typename Pred, typename T, typename... Ts, EnableIf<!Pred{}(T{})>>
constexpr auto remove_if_t::impl(Pred pred, TypeList<T, Ts...>) const {
  return List(T{}).Concat(impl(pred, TypeList<Ts...>{}));
}

template <typename Pred, typename... Ts>
constexpr auto remove_if_t::operator()(Pred pred, TypeList<Ts...>) const {
  return impl(pred, TypeList<Ts...>{});
}

} // end namespace aether

#endif
