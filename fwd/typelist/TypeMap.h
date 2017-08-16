#ifndef FWD_TYPELIST_TYPEMAP_H
#define FWD_TYPELIST_TYPEMAP_H

#include "aether/fwd/TypeTraits.h"
#include "aether/fwd/typelist/TypeList.h"
#include "aether/typelist/Contains.h"

namespace aether {

template <typename Ks, typename Vs>
struct TypeMap {
  template <typename K, typename V, EnableIf<!contains(K{}, Ks{})> = 0>
  constexpr auto Put(K, V) const;

  template <typename K, typename V, EnableIf<contains(K{}, Ks{})> = 0>
  constexpr auto Put(K, V) const;

  template <typename K, EnableIf<contains(K{}, Ks{})> = 0>
  constexpr auto Remove(K) const;

  template <typename K, EnableIf<!contains(K{}, Ks{})> = 0>
  constexpr TypeMap<Ks, Vs> Remove(K) const;

  template <typename K>
  constexpr bool HasKey(K) const;

  constexpr Ks Keys() const;

  constexpr Vs Values() const;

  template <typename K>
  constexpr auto operator[](K) const;

  template <typename K>
  constexpr auto Get(K) const;

  template <typename V>
  constexpr auto GetKeyForValue(V) const;

  template <typename V>
  constexpr bool HasValue(V) const;

  constexpr TypeMap<Ks, Vs> MergeImpl(TypeList<>) const;

  template <typename K, typename V, typename... KVs>
  constexpr auto MergeImpl(TypeList<TypeList<K, V>, KVs...>) const;

  template <typename K, typename V>
  constexpr auto Merge(TypeMap<K, V>) const;

  constexpr std::size_t Size() const;

  constexpr bool IsEmpty() const;
};

} // end namespace aether

#endif
