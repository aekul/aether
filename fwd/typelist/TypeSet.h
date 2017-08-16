#ifndef FWD_TYPELIST_TYPESET_H
#define FWD_TYPELIST_TYPESET_H

#include "aether/fwd/TypeTraits.h"
#include "aether/fwd/typelist/TypeList.h"

namespace aether {

template <typename T>
struct TypeSetKey {};

template <typename S, typename T>
using has_key = std::is_base_of<TypeSetKey<T>, S>;

template <typename... Ts>
struct TypeSet : TypeSetKey<Ts>... {
  template <typename R, EnableIf<!has_key<TypeSet<Ts...>, R>::value> = 0>
  constexpr TypeSet<Ts..., R> Put(R) const;

  template <typename R, EnableIf<has_key<TypeSet<Ts...>, R>::value> = 0>
  constexpr TypeSet<Ts...> Put(R) const;

  template <typename T>
  constexpr bool Has() const;

  template <typename T>
  constexpr bool Has(T) const;

  template <typename R, EnableIf<!has_key<TypeSet<Ts...>, R>::value> = 0>
  constexpr TypeSet<Ts...> Remove(R) const;

  template <typename... Rs>
  constexpr TypeSet<Rs...> RemoveImpl(TypeList<Rs...>) const;

  template <typename R, EnableIf<has_key<TypeSet<Ts...>, R>::value> = 0>
  constexpr auto Remove(R) const;

  constexpr TypeSet<Ts...> Merge(TypeSet<>) const;

  template <typename R, typename... Rs>
  constexpr auto Merge(TypeSet<R, Rs...>) const;

  constexpr TypeSet<Ts...> Merge(TypeList<>) const;

  template <typename R, typename... Rs>
  constexpr auto Merge(TypeList<R, Rs...>) const;

  constexpr TypeSet<Ts...> Difference(TypeSet<>) const;

  template <typename R, typename... Rs>
  constexpr auto Difference(TypeSet<R, Rs...>) const;

  constexpr TypeSet<> Intersection(TypeSet<>) const;

  template <typename R, typename... Rs, EnableIf<!has_key<TypeSet<Ts...>, R>::value> = 0>
  constexpr auto Intersection(TypeSet<R, Rs...>) const;

  template <typename R, typename... Rs, EnableIf<has_key<TypeSet<Ts...>, R>::value> = 0>
  constexpr auto Intersection(TypeSet<R, Rs...>) const;

  template <typename... Rs>
  constexpr bool operator==(TypeSet<Rs...>) const;

  template <typename... Rs>
  constexpr bool operator!=(TypeSet<Rs...>) const;

  constexpr int Size() const;

  constexpr bool IsEmpty() const;
};

} // end namespace aether

#endif
