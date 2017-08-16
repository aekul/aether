#ifndef TYPELIST_TYPEMAP_H
#define TYPELIST_TYPEMAP_H

#include "aether/fwd/typelist/TypeMap.h"
#include "aether/typelist/Remove.h"
#include "aether/typelist/Zip.h"
#include "aether/typelist/Append.h"

namespace aether {

template <typename Ks, typename Vs>
template <typename K, typename V, EnableIf<!contains(K{}, Ks{})>>
constexpr auto TypeMap<Ks, Vs>::Put(K, V) const {
  return make_type_map(append(Ks{}, K{}), append(Vs{}, V{}));
}

template <typename Ks, typename Vs>
template <typename K, typename V, EnableIf<contains(K{}, Ks{})>>
constexpr auto TypeMap<Ks, Vs>::Put(K, V) const {
  constexpr std::size_t I = index_of(K{}, Ks{});
  constexpr auto keys = remove_at(_size_t<I>{}, Ks{});
  constexpr auto values = remove_at(_size_t<I>{}, Vs{});
  return make_type_map(keys, values).Put(K{}, V{});
}

template <typename Ks, typename Vs>
template <typename K, EnableIf<contains(K{}, Ks{})>>
constexpr auto TypeMap<Ks, Vs>::Remove(K) const {
  constexpr std::size_t I = index_of(K{}, Ks{});
  constexpr auto keys = remove_at(_size_t<I>{}, Ks{});
  constexpr auto values = remove_at(_size_t<I>{}, Vs{});
  return make_type_map(keys, values);
}

template <typename Ks, typename Vs>
template <typename K, EnableIf<!contains(K{}, Ks{})>>
constexpr TypeMap<Ks, Vs> TypeMap<Ks, Vs>::Remove(K) const {
  return {};
}

template <typename Ks, typename Vs>
constexpr Ks TypeMap<Ks, Vs>::Keys() const {
  return Ks{};
}

template <typename Ks, typename Vs>
constexpr Vs TypeMap<Ks, Vs>::Values() const {
  return {};
}

template <typename Ks, typename Vs>
template <typename K>
constexpr bool TypeMap<Ks, Vs>::HasKey(K) const {
  return contains(K{}, Ks{});
}

template <typename Ks, typename Vs>
template <typename K>
constexpr auto TypeMap<Ks, Vs>::operator[](K) const {
  return at<index_of(K{}, Ks{})>(Vs{});
}

template <typename Ks, typename Vs>
template <typename K>
constexpr auto TypeMap<Ks, Vs>::Get(K) const {
  constexpr auto key = K{};
  return this->operator[](key);
}

template <typename Ks, typename Vs>
template <typename V>
constexpr auto TypeMap<Ks, Vs>::GetKeyForValue(V) const {
  return at<index_of(V{}, Vs{})>(Ks{});
}

template <typename Ks, typename Vs>
template <typename V>
constexpr bool TypeMap<Ks, Vs>::HasValue(V value) const {
  return contains(value, Vs{});
}

template <typename Ks, typename Vs>
constexpr TypeMap<Ks, Vs> TypeMap<Ks, Vs>::MergeImpl(TypeList<>) const {
  return {};
}

template <typename Ks, typename Vs>
template <typename K, typename V, typename... KVs>
constexpr auto TypeMap<Ks, Vs>::MergeImpl(TypeList<TypeList<K, V>, KVs...>) const {
  return Put(K{}, V{}).MergeImpl(TypeList<KVs...>{});
}

template <typename Ks, typename Vs>
template <typename K, typename V>
constexpr auto TypeMap<Ks, Vs>::Merge(TypeMap<K, V>) const {
  constexpr auto pairs = zip(K{}, V{});
  return MergeImpl(pairs);
}

template <typename Ks, typename Vs>
constexpr std::size_t TypeMap<Ks, Vs>::Size() const {
  return Ks{}.Size();
}

template <typename Ks, typename Vs>
constexpr bool TypeMap<Ks, Vs>::IsEmpty() const {
  return Size() == 0;
}

constexpr TypeMap<TypeList<>, TypeList<>> make_type_map() {
  return {};
}

template <typename... Ks, typename... Vs>
constexpr TypeMap<TypeList<Ks...>, TypeList<Vs...>> make_type_map(TypeList<Ks...>, TypeList<Vs...>) {
  static_assert(sizeof...(Ks) == sizeof...(Vs), "TypeMap requires the key list and value list to have the same length.");
  return {};
}


} // end namespace aether

#endif
