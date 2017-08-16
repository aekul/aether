#ifndef AETHER_NAMED_PARAMETER_H
#define AETHER_NAMED_PARAMETER_H

#include <utility>

#include <boost/hana.hpp>

#include "aether/typelist/TypeList.h"
#include "aether/typelist/Contains.h"
#include "aether/typelist/IndexOf.h"

namespace aether {
  
using namespace boost;

template <typename T>
struct named_parameter {
  using Name = T;

  template <typename V>
  constexpr auto operator=(V&&) const;
};

template <typename N, typename T>
struct bound_named_parameter {
  using Name = N;
  using result_type = T;
  T value;

  constexpr Name GetName() const {
    return {};
  }

  const T& Value() const {
    return value;
  }

  T& Value() {
    return value;
  }
};

template <typename N, typename T>
struct named_field {
  using Name = N;
  using Type = T;
};


template <typename Keys, typename Store>
struct named_struct {
  Store store{};

  using K = Keys;

  template <typename Name>
  constexpr const auto& operator[](Name) const {
    static_assert(contains(Name{}, Keys{}), "Field not found in named_struct");
    constexpr auto index = index_of(Name{}, Keys{});
    return hana::at_c<index>(store);
  }

  template <typename Name>
  auto& operator[](Name) {
    static_assert(contains(Name{}, Keys{}), "Field not found in named_struct");
    constexpr auto index = index_of(Name{}, Keys{});
    return hana::at_c<index>(store);
  }

  template <typename Name>
  constexpr bool HasKey(Name) const {
    return contains(Name{}, Keys{});
  }
};

using empty_named_struct = named_struct<TypeSet<>, hana::tuple<>>;

template <typename Name, typename T>
struct HasKey : std::false_type {};

template <typename Name, typename Keys, typename Store>
struct HasKey<Name, named_struct<Keys, Store>> {
  static constexpr bool value = contains(Name{}, Keys{});
};

template <typename Name, typename A, typename B, EnableIf<!HasKey<Name, A>::value || !HasKey<Name, B>::value> = 0>
constexpr bool TypesMatch() {
  return false;
};

template <typename Name, typename A, typename B, EnableIf<HasKey<Name, A>::value && HasKey<Name, B>::value> = 0>
constexpr bool TypesMatch() {
  return std::is_same<
    decltype(std::declval<typename A::Store>()[std::declval<Name>()])
    , decltype(std::declval<typename B::Store>()[std::declval<Name>()])
  >::value;
};



template <typename K, typename To, typename From, EnableIf<!TypesMatch<K, To, From>()> = 0>
void copy_fields_single(K, To&, const From&) {
}

template <typename K, typename To, typename From, EnableIf<TypesMatch<K, To, From>()> = 0>
void copy_fields_single(K, To& to, const From& from) {
  constexpr auto key = K{};
  to[key] = from[key];
}

template <typename... Ks, typename To, typename From>
void copy_fields_impl(TypeSet<Ks...>, To& to, const From& from) {
  using consume = int[];
  (void)consume{1, (copy_fields_single(Ks{}, to, from), 1)...};
}

template <typename To, typename From>
void copy_fields(To& to, const From& from) {
  copy_fields_impl(to.Keys(), to, from);
}

template <typename T, typename Name>
constexpr auto field(Name) {
  return named_field<Name, T>{};
}

template <typename... Vs>
constexpr auto make_named_struct(Vs...) {
  return named_struct<TypeSet<typename Vs::Name...>, hana::tuple<typename Vs::Type...>>{};
}

template <typename Name, typename T>
constexpr auto make_bound_named_parameter(Name, T&& t) {
  return bound_named_parameter<Name, std::decay_t<T>>{std::forward<T>(t)};
}

template <typename T>
template <typename V>
constexpr auto named_parameter<T>::operator=(V&& v) const {
  return bound_named_parameter<T, std::decay_t<V>>{std::forward<V>(v)};
}

template <typename Name, typename Tuple>
struct has_name_impl {
  static constexpr bool value = false;
};

template <typename Name, typename T, typename... Ts>
struct has_name_impl<Name, TypeList<bound_named_parameter<Name, T>, Ts...>> {
  static constexpr bool value = true;
};

template <typename Name, typename T, typename... Ts>
struct has_name_impl<Name, TypeList<T, Ts...>> {
  static constexpr bool value = has_name_impl<Name, TypeList<Ts...>>::value;
};

template <typename Name, typename... Ts>
struct has_name_impl<Name, hana::tuple<Ts...>> {
  static constexpr bool value = has_name_impl<Name, TypeList<Ts...>>::value;
};

template <typename Name, typename Tuple>
struct has_name {
  static constexpr bool value = has_name_impl<Name, Tuple>::value;
};


template <std::size_t I, typename Name, typename Tuple>
struct index_of_name_impl;

template <std::size_t I, typename Name>
struct index_of_name_impl<I, Name, TypeList<>> {
  static_assert(!std::is_same<Name, Name>::value, "Named parameter not found.");
};

template <std::size_t I, typename Name, typename T, typename... Ts>
struct index_of_name_impl<I, Name, TypeList<bound_named_parameter<Name, T>, Ts...>> {
  static constexpr std::size_t value = I;
};

template <std::size_t I, typename Name, typename T, typename... Ts>
struct index_of_name_impl<I, Name, TypeList<T, Ts...>> {
  static constexpr std::size_t value = index_of_name_impl<I + 1, Name, TypeList<Ts...>>::value;
};

template <typename Name, typename... Ts>
struct index_of_name_impl<0, Name, hana::tuple<Ts...>> {
  static constexpr std::size_t value = index_of_name_impl<0, Name, TypeList<Ts...>>::value;
};

template <typename Name, typename Tuple>
struct index_of_name {
  static constexpr std::size_t value = index_of_name_impl<0, Name, Tuple>::value;
};

template <std::size_t I, typename Name, typename Tuple>
struct type_of_name_impl;

template <std::size_t I, typename Name>
struct type_of_name_impl<I, Name, TypeList<>> {
  static_assert(!std::is_same<Name, Name>::value, "Named parameter not found.");
};

template <std::size_t I, typename Name, typename T, typename... Ts>
struct type_of_name_impl<I, Name, TypeList<bound_named_parameter<Name, T>, Ts...>> {
  using type = T;
};

template <std::size_t I, typename Name, typename T, typename... Ts>
struct type_of_name_impl<I, Name, TypeList<T, Ts...>> {
  using type = typename type_of_name_impl<I + 1, Name, TypeList<Ts...>>::type;
};

template <typename Name, typename... Ts>
struct type_of_name_impl<0, Name, hana::tuple<Ts...>> {
  using type = typename type_of_name_impl<0, Name, TypeList<Ts...>>::type;
};

template <typename Name, typename Tuple>
struct type_of_name {
  using type = typename type_of_name_impl<0, Name, Tuple>::type;
};

template <typename Name, typename Tuple>
using type_of_name_t = typename type_of_name<Name, Tuple>::type;


#define NAMED_PARAM(name) struct name##_t {}; constexpr named_parameter<name##_t> name##_{};

} // end namespace aether

#endif
