#ifndef TYPELIST_AT_H
#define TYPELIST_AT_H

#include "aether/fwd/typelist/At.h"
#include "aether/typelist/Fill.h"

namespace aether {

#if defined(AT_PAIRS)

template <typename K, typename V>
struct Pair {};

// http://pdimov.com/cpp2/simple_cxx11_metaprogramming_2.html
template <typename K, typename... Pairs>
struct at_impl {
  struct at_helper : Pairs... {};

  template <typename T>
  static constexpr T fn(Pair<K, T>*) { return T{}; }

  static constexpr void fn(...) {}

  using type = decltype(fn(static_cast<at_helper*>(0)));
};

template <typename K, typename... Pairs>
using at_impl_t = typename at_impl<K, Pairs...>::type;

template <std::size_t I, typename... Ts, std::size_t... Is>
constexpr auto at(_size_t<I>, TypeList<Ts...>, std::index_sequence<Is...>) {
  return at_impl_t<_size_t<I>, Pair<_size_t<Is>, Ts>...>{};
}

template <std::size_t I, typename... Ts, EnableIf<(I > 0)>>
constexpr auto at(_size_t<I> i, TypeList<Ts...> list) {
  static_assert(I < sizeof...(Ts), "Out of bounds");
  return at(i, list, std::index_sequence_for<Ts...>{});
}


#else


template <typename Args, typename LeadingArgs>
struct at_impl;

template <typename... Ts, typename... LeadingArgs>
struct at_impl<TypeList<Ts...>, TypeList<LeadingArgs...>> {

  // http://pdimov.com/cpp2/simple_cxx11_metaprogramming_2.html
  template <typename T>
  static constexpr T fn(LeadingArgs..., T, ...) { return T{}; };

  using type = std::remove_pointer_t<decltype(fn(static_cast<Ts*>(0)...))>;
};

template <typename Args, typename LeadingArgs>
using at_impl_t = typename at_impl<Args, LeadingArgs>::type;


template <std::size_t I, typename... Ts, EnableIf<(I > 0)>>
constexpr auto at(_size_t<I>, TypeList<Ts...>) {
  static_assert(I < sizeof...(Ts), "Out of bounds");
  using LeadingArgs = decltype(fill(_size_t<I>{}, (void*)nullptr));
  return at_impl_t<TypeList<Ts...>, LeadingArgs>{};
}

#endif

template <std::size_t I>
constexpr auto at(_size_t<I>, TypeList<>) {
  return type_not_found;
}

template <typename T, typename... Ts>
constexpr auto at(_size_t<0>, TypeList<T, Ts...>) {
  return T{};
}

template <std::size_t I, typename... Ts>
constexpr auto at(TypeList<Ts...> list) {
  return at(_size_t<I>{}, list);
}

template <std::size_t I, std::size_t J, typename... Ts>
constexpr auto at(TypeList<Ts...> list) {
  return at<J>(at(_size_t<I>{}, list));
}

template <std::size_t I, typename... Ts>
constexpr auto at(TypeSet<Ts...>) {
  return at(_size_t<I>{}, TypeList<Ts...>{});
}

template <std::size_t... Is, typename... Ts>
constexpr auto at_all(TypeList<Ts...>) {
  return List(at<Is>(TypeList<Ts...>{})...);
}

template <typename... Is, typename... Ts>
constexpr auto at_all(TypeList<Is...>, TypeList<Ts...>) {
  return List(at(Is{}, Ts{})...);
}


} // end namespace aether

#endif
