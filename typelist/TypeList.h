#ifndef TYPELIST_TYPELIST_H
#define TYPELIST_TYPELIST_H

#include "aether/fwd/TypeTraits.h"

#include "aether/fwd/typelist/TypeList.h"
#include "aether/typelist/Contains.h"

namespace aether {

template <typename... Ts>
template <typename... Rs>
constexpr TypeList<Ts..., Rs...> TypeList<Ts...>::Concat(const TypeList<Rs...>&) const {
  return TypeList<Ts..., Rs...>();
}

template <typename... Ts>
template <typename R>
constexpr TypeList<Ts..., R> TypeList<Ts...>::Concat(R) const {
  return TypeList<Ts..., R>{};
}

template <typename... Ts>
constexpr int TypeList<Ts...>::Size() const {
  return sizeof...(Ts);
}

template <typename... Ts>
template <typename... Rs>
constexpr bool TypeList<Ts...>::operator==(TypeList<Rs...>) const {
  return std::is_same<Self, TypeList<Rs...>>::value;
}

constexpr TypeList<> List(type_not_found_t) {
  return TypeList<>{};
}

template <typename... Ts>
constexpr TypeList<Ts...> List(Ts...) {
  return {};
}

template <typename... Ts>
constexpr bool is_list(TypeList<Ts...>) {
  return true;
}

template <typename T>
constexpr bool is_list(T) {
  return false;
}

template <typename... Ts>
constexpr TypeList<TypeList<Ts...>> ListOfLists(TypeList<Ts...>) {
  return {};
}



template <typename T>
constexpr auto flatten_impl(T) {
  return TypeList<T>{};
}

template <typename... Ts>
constexpr auto flatten_impl(TypeList<Ts...>) {
  return TypeList<Ts...>{};
}

template <typename T, typename... Ts>
constexpr auto flatten_impl(TypeList<T, Ts...>) {
  return flatten_impl(T{}).Concat(flatten_impl(TypeList<Ts...>{}));
}

template <typename... Ts>
constexpr auto flatten(Ts...) {
  return flatten_impl(TypeList<Ts...>{});
}





template <typename... Rs>
constexpr bool IsSubsetOf(TypeList<>, TypeList<Rs...>) {
  return true;
}

template <typename T, typename... Ts, typename... Rs, EnableIf<!contains(T{}, TypeList<Rs...>{})> = 0>
constexpr bool IsSubsetOf(TypeList<T, Ts...>, TypeList<Rs...>) {
  return false;
}

template <typename T, typename... Ts, typename... Rs, EnableIf<contains(T{}, TypeList<Rs...>{})> = 0>
constexpr bool IsSubsetOf(TypeList<T, Ts...>, TypeList<Rs...>) {
  return IsSubsetOf(TypeList<Ts...>(), TypeList<Rs...>{});
}

template <typename... Ts, typename... Rs>
constexpr bool AreEqual(TypeList<Ts...>, TypeList<Rs...>) {
  return IsSubsetOf(TypeList<Ts...>{}, TypeList<Rs...>{}) && IsSubsetOf(TypeList<Rs...>{}, TypeList<Ts...>{}); 
}

template <typename... Ts>
constexpr bool AreAllEqual(TypeList<Ts...>) {
  return true;
}

template <typename... Ts, typename... Rs, typename... Os>
constexpr bool AreAllEqual(TypeList<Ts...>, TypeList<Rs...>, Os...) {
  return AreEqual(TypeList<Ts...>{}, TypeList<Rs...>{}) && AreAllEqual(TypeList<Ts...>{}, Os{}...);
}

constexpr bool all_same_impl(TypeList<>) {
  return true;
}

template <typename A, typename... Ts>
constexpr bool all_same_impl(TypeList<A, Ts...>) {
  bool same[] = {std::is_same<A, Ts>::value...};
  for (bool s : same) {
    if (!s) {
      return false;
    }
  }
  return true;
}

template <typename... Ts>
constexpr bool all_same(TypeList<Ts...>) {
  return all_same_impl(TypeList<Ts...>{});
}

template <typename... Ts>
constexpr bool all_same(Ts...) {
  return all_same_impl(TypeList<Ts...>{});
}

} // end namespace aether

#endif
