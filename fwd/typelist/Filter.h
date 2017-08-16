#ifndef FWD_TYPELIST_FILTER_H
#define FWD_TYPELIST_FILTER_H

#include "../TypeTraits.h"

#include "TypeList.h"
#include "TypeSet.h"

namespace aether {

struct filter_t {
  template <typename Pred>
  constexpr auto impl(Pred, TypeList<>) const;

  template <typename Pred, typename T, typename... Ts, EnableIf<!Pred{}(T{})> = 0>
  constexpr auto impl(Pred pred, TypeList<T, Ts...>) const;

  template <typename Pred, typename T, typename... Ts, EnableIf<Pred{}(T{})> = 0>
  constexpr auto impl(Pred pred, TypeList<T, Ts...>) const;

  template <typename Pred, typename... Ts>
  constexpr auto operator()(Pred pred, TypeList<Ts...>) const;

  template <typename Pred, typename... Ts>
  constexpr auto operator()(Pred pred, TypeSet<Ts...>) const;
};

constexpr filter_t filter{};

struct remove_if_t {
  template <typename Pred>
  constexpr auto impl(Pred, TypeList<>) const;

  template <typename Pred, typename T, typename... Ts, EnableIf<Pred{}(T{})> = 0>
  constexpr auto impl(Pred pred, TypeList<T, Ts...>) const;

  template <typename Pred, typename T, typename... Ts, EnableIf<!Pred{}(T{})> = 0>
  constexpr auto impl(Pred pred, TypeList<T, Ts...>) const;

  template <typename Pred, typename... Ts>
  constexpr auto operator()(Pred pred, TypeList<Ts...>) const;
};

constexpr remove_if_t remove_if{};

} // end namespace aether

#endif
