#ifndef TYPELIST_DROP_H
#define TYPELIST_DROP_H

#include "aether/fwd/typelist/Drop.h"

#include "aether/typelist/At.h"
#include "aether/typelist/Fill.h"

namespace aether {

template <typename Args, typename LeadingArgs>
struct drop_from_front_impl;

template <typename... Ts, typename... LeadingArgs>
struct drop_from_front_impl<TypeList<Ts...>, TypeList<LeadingArgs...>> {

  // http://pdimov.com/cpp2/simple_cxx11_metaprogramming_2.html
  template <typename... Rs>
  static constexpr TypeList<std::remove_pointer_t<Rs>...> fn(LeadingArgs..., Rs...) { return {}; };

  using type = decltype(fn(static_cast<Ts*>(0)...));
};

template <typename Args, typename LeadingArgs>
using drop_from_front_impl_t = typename drop_from_front_impl<Args, LeadingArgs>::type;

template <std::size_t N, typename... Ts, EnableIf<(N > 0)>>
constexpr auto drop_from_front_t::operator()(_size_t<N>, TypeList<Ts...>) const {
  static_assert(N <= sizeof...(Ts), "Out of bounds");
  using LeadingArgs = decltype(fill(_size_t<N>{}, (void*)nullptr));
  return drop_from_front_impl_t<TypeList<Ts...>, LeadingArgs>{};
};

template <std::size_t N, typename... Ts, EnableIf<(N <= 0)>>
constexpr TypeList<Ts...> drop_from_front_t::operator()(_size_t<N>, TypeList<Ts...>) const {
  return {};
};

template <typename... Ts>
constexpr TypeList<> drop_from_back_t::impl(_size_t<0>, TypeList<Ts...>) const {
  return {};
}

template <std::size_t N, typename T, typename... Ts, EnableIf<(N >= 1 && N < 2)>>
constexpr auto drop_from_back_t::impl(_size_t<N>, TypeList<T, Ts...>) const {
  return TypeList<T>{}.Concat(impl(_size_t<N - 1>{}, TypeList<Ts...>{}));
}

template <std::size_t N, typename T0, typename T1, typename... Ts, EnableIf<(N >= 2 && N < 4)>>
constexpr auto drop_from_back_t::impl(_size_t<N>, TypeList<T0, T1, Ts...>) const {
  return TypeList<T0, T1>{}.Concat(impl(_size_t<N - 2>{}, TypeList<Ts...>{}));
}

template <std::size_t N, typename T0, typename T1, typename T2, typename T3, typename... Ts, EnableIf<(N >= 4 && N < 8)>>
constexpr auto drop_from_back_t::impl(_size_t<N>, TypeList<T0, T1, T2, T3, Ts...>) const {
  return TypeList<T0, T1, T2, T3>{}.Concat(impl(_size_t<N - 4>{}, TypeList<Ts...>{}));
}

template <std::size_t N, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename... Ts, EnableIf<(N >= 8 && N < 16)>>
constexpr auto drop_from_back_t::impl(_size_t<N>, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, Ts...>) const {
  return TypeList<T0, T1, T2, T3, T4, T5, T6, T7>{}.Concat(impl(_size_t<N - 8>{}, TypeList<Ts...>{}));
}

template <std::size_t N, typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10, typename T11, typename T12, typename T13, typename T14, typename T15, typename... Ts, EnableIf<(N >= 16)>>
constexpr auto drop_from_back_t::impl(_size_t<N>, TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Ts...>) const {
  return TypeList<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15>{}.Concat(impl(_size_t<N - 16>{}, TypeList<Ts...>{}));
}

template <std::size_t N, typename... Ts, EnableIf<(N > 0)>>
constexpr auto drop_from_back_t::operator()(_size_t<N>, TypeList<Ts...> list) const {
  static_assert(N <= sizeof...(Ts), "Out of bounds");
  return impl(_size_t<sizeof...(Ts) - N>{}, list);
};

template <std::size_t N, typename... Ts, EnableIf<(N <= 0)>>
constexpr TypeList<Ts...> drop_from_back_t::operator()(_size_t<N>, TypeList<Ts...>) const {
  return {};
};

} // end namespace aether

#endif
