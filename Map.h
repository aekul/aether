#ifndef AETHER_MAP
#define AETHER_MAP

#include "aether/fwd/TypeTraits.h"
#include "aether/fwd/Math.h"

#include "aether/typelist/Contains.h"
#include "aether/typelist/Intersection.h"

namespace aether {

template <typename... Ks>
struct Map {
  using keys_t = TypeSet<Ks...>;

  std::array<Real, sizeof...(Ks)> values;
  std::array<bool, sizeof...(Ks)> valid;

  constexpr TypeSet<Ks...> Keys() const {
    return {};
  }
};

using empty_map_t = Map<>;

template <typename Key, typename... Ks>
constexpr bool has(const Map<Ks...>&) {
  return contains(Key{}, TypeSet<Ks...>{});
}

template <typename Key, typename... Ks>
constexpr bool valid(const Map<Ks...>& map) {
  static_assert(contains(Key{}, TypeSet<Ks...>{}), "Key not found in map.");
  constexpr auto index = index_of(Key{}, TypeSet<Ks...>{});
  return map.valid[index];
}

template <typename Key, typename... Ks>
constexpr bool valid(Key, const Map<Ks...>& map) {
  static_assert(contains(Key{}, TypeSet<Ks...>{}), "Key not found in map.");
  constexpr auto index = index_of(Key{}, TypeSet<Ks...>{});
  return map.valid[index];
}

template <typename Key, typename... Ks>
constexpr auto get(const Map<Ks...>& map) {
  static_assert(contains(Key{}, TypeSet<Ks...>{}), "Key not found in map.");
  constexpr auto index = index_of(Key{}, TypeSet<Ks...>{});
  return map.values[index];
}

template <typename Key, typename... Ks>
constexpr auto get(Key, const Map<Ks...>& map) {
  static_assert(contains(Key{}, TypeSet<Ks...>{}), "Key not found in map.");
  constexpr auto index = index_of(Key{}, TypeSet<Ks...>{});
  return map.values[index];
}

template <typename... Keys, typename... Ks>
constexpr auto get_all(TypeSet<Keys...> keys, const Map<Ks...>& map) {
  return make_map(
    keys
    , std::array<Real, sizeof...(Keys)>{{get(Keys{}, map)...}}
    , std::array<bool, sizeof...(Keys)>{{valid(Keys{}, map)...}}
  );
}

template <typename... Ts, typename... Bs, std::size_t N>
constexpr auto make_map_impl(TypeSet<Ts...>, TypeList<Bs...>, const std::array<Real, N>& vs) {
  return Map<Ts...>{
    vs
    , std::array<bool, sizeof...(Ts)>{{Bs::value...}}
  };
}

template <typename... Ts, typename... Vs>
constexpr auto make_map(TypeSet<Ts...> keys, const Vs&... vs) {
  return make_map_impl(keys, fill(_size_t<sizeof...(Ts)>{}, bool_t<true>{}), std::array<Real, sizeof...(Ts)>{{vs...}}); 
}

template <typename... Ts, std::size_t N>
constexpr auto make_map(TypeSet<Ts...> keys, const std::array<Real, N>& vs) {
  return make_map_impl(keys, fill(_size_t<sizeof...(Ts)>{}, bool_t<true>{}), vs); 
}

template <typename... Ts, std::size_t N>
constexpr auto make_map(TypeSet<Ts...>, const std::array<Real, N>& values, const std::array<bool, N>& valid) {
  return Map<Ts...>{values, valid};
}

template <typename Key, typename V, typename... Ks, EnableIf<!contains(Key{}, TypeSet<Ks...>{})> = 0>
constexpr void set(Key, const V&, Map<Ks...>&) {
}

template <typename Key, typename V, typename... Ks, EnableIf<contains(Key{}, TypeSet<Ks...>{})> = 0>
constexpr void set(Key, const V& value, Map<Ks...>& map) {
  static_assert(contains(Key{}, TypeSet<Ks...>{}), "Key not found in map.");
  constexpr auto index = index_of(Key{}, TypeSet<Ks...>{});
  map.values[index] = value;
  map.valid[index] = true;
}

template <typename... Keys, std::size_t N, typename... Ks, std::size_t... Is>
constexpr void set_all_impl(TypeSet<Keys...>, const std::array<Real, N>& values, Map<Ks...>& map, std::index_sequence<Is...>) {
  using consume = int[];
  (void)consume{1, (set(Keys{}, values[Is], map), 1)...};
};

template <typename... Keys, std::size_t N, typename... Ks>
constexpr void set_all(TypeSet<Keys...> keys, const std::array<Real, N>& values, Map<Ks...>& map) {
  set_all_impl(keys, values, map, std::make_index_sequence<N>{});
}

template <typename Key, typename V, typename... Ks, EnableIf<contains(Key{}, TypeSet<Ks...>{})> = 0>
constexpr auto put(Key, const V&, const Map<Ks...>& map) {
  return map;
}

template <typename Key, typename V, typename... Ks, std::size_t... Is>
constexpr auto put_impl(Key, const V& value, const Map<Ks...>& map, std::index_sequence<Is...>) {
  std::array<Real, sizeof...(Ks) + 1> values{{map.values[Is]..., value}};
  std::array<bool, sizeof...(Ks) + 1> valid{{map.valid[Is]..., true}};
  return Map<Ks..., Key>{values, valid};
}

template <typename Key, typename V, typename... Ks, EnableIf<!contains(Key{}, TypeSet<Ks...>{})> = 0>
constexpr auto put(Key, const V& value, const Map<Ks...>& map) {
  return put_impl(Key{}, value, map, std::index_sequence_for<Ks...>{});
}

template <std::size_t N, std::size_t I, typename... Ks>
constexpr auto put_all_impl(TypeSet<>, const std::array<Real, N>&, const Map<Ks...>& map, _size_t<I>) {
  return map;
}

template <typename Key, typename... Keys, std::size_t N, std::size_t I, typename... Ks>
constexpr auto put_all_impl(TypeSet<Key, Keys...>, const std::array<Real, N>& values, const Map<Ks...>& map, _size_t<I>) {
  return put_all_impl(put_impl(Key{}, values[I], map, _size_t<I + 1>{}));
}

template <typename Key, typename... Keys, std::size_t N, typename... Ks>
constexpr auto put_all(TypeSet<Key, Keys...>, const std::array<Real, N>& values, const Map<Ks...>& map) {
  return put_all_impl(Key{}, values, map, _size_t<0>{});
}

template <typename Key, typename... Ts, typename... Rs, EnableIf<contains(Key{}, TypeSet<Ts...>{}) && contains(Key{}, TypeSet<Rs...>{})> = 0>
constexpr auto merge_single_value(Key, const Map<Ts...>& a, const Map<Rs...>& b) {
  if (valid(Key{}, a) && !valid(Key{}, b)) {
    return get(Key{}, a);
  }

  if (!valid(Key{}, a) && valid(Key{}, b)) {
    return get(Key{}, b);
  }

  return get(Key{}, a);
}

template <typename Key, typename... Ts, typename... Rs, EnableIf<contains(Key{}, TypeSet<Ts...>{}) && !contains(Key{}, TypeSet<Rs...>{})> = 0>
constexpr auto merge_single_value(Key, const Map<Ts...>& a, const Map<Rs...>&) {
  return get(Key{}, a);
}

template <typename Key, typename... Ts, typename... Rs, EnableIf<!contains(Key{}, TypeSet<Ts...>{}) && contains(Key{}, TypeSet<Rs...>{})> = 0>
constexpr auto merge_single_value(Key, const Map<Ts...>&, const Map<Rs...>& b) {
  return get(Key{}, b);
}

template <typename Key, typename... Ts, typename... Rs, EnableIf<contains(Key{}, TypeSet<Ts...>{}) && contains(Key{}, TypeSet<Rs...>{})> = 0>
constexpr auto merge_single_valid(Key, const Map<Ts...>& a, const Map<Rs...>& b) {
  return valid(Key{}, a) || valid(Key{}, b);
}

template <typename Key, typename... Ts, typename... Rs, EnableIf<contains(Key{}, TypeSet<Ts...>{}) && !contains(Key{}, TypeSet<Rs...>{})> = 0>
constexpr bool merge_single_valid(Key, const Map<Ts...>& a, const Map<Rs...>&) {
  return valid(Key{}, a);
}

template <typename Key, typename... Ts, typename... Rs, EnableIf<!contains(Key{}, TypeSet<Ts...>{}) && contains(Key{}, TypeSet<Rs...>{})> = 0>
constexpr bool merge_single_valid(Key, const Map<Ts...>&, const Map<Rs...>& b) {
  return valid(Key{}, b);
}

template <typename... Ts, typename... Rs, typename... Ks>
constexpr auto merge_impl(const Map<Ts...>& a, const Map<Rs...>& b, TypeSet<Ks...> keys) {
  std::array<Real, sizeof...(Ks)> values{{merge_single_value(Ks{}, a, b)...}};
  std::array<bool, sizeof...(Ks)> valid{{merge_single_valid(Ks{}, a, b)...}};
  return make_map(keys, values, valid);
}

template <typename... Ts, typename... Ks, EnableIf<(sizeof...(Ts) > sizeof...(Ks))> = 0>
constexpr auto merge(const Map<Ts...>& a, const Map<Ks...>& b) {
  constexpr auto merged_keys = TypeSet<Ts...>{}.Merge(TypeSet<Ks...>{});
  return merge_impl(a, b, merged_keys);
}

template <typename... Ts, typename... Ks, EnableIf<(sizeof...(Ts) <= sizeof...(Ks))> = 0>
constexpr auto merge(const Map<Ts...>& a, const Map<Ks...>& b) {
  constexpr auto merged_keys = TypeSet<Ts...>{}.Merge(TypeSet<Ks...>{});
  return merge_impl(a, b, merged_keys);
}

constexpr Map<> merge_all() {
  return {};
}

template <typename... Ts>
constexpr auto merge_all(const Map<Ts...>& a) {
  return a;
}

template <typename... Ts, typename... Ks, typename... Os>
constexpr auto merge_all(const Map<Ts...>& a, const Map<Ks...>& b, const Os&... others) {
  return merge_all(merge(a, b), others...);
}

template <typename Key, typename... Ts, typename... Ks>
constexpr void copy_single(Key, Map<Ts...>& to, const Map<Ks...>& from) {
  static_assert(contains(Key{}, TypeSet<Ks...>{}), "Key not found in map.");
  constexpr auto index_to = index_of(Key{}, TypeSet<Ts...>{});
  constexpr auto index_from = index_of(Key{}, TypeSet<Ks...>{});
  to.values[index_to] = from.values[index_from];
  to.valid[index_to] = from.valid[index_from];
}

template <typename... Ts>
constexpr auto copy(Map<Ts...>&, const Map<>&) {
}

template <typename... Ts, typename... Ks, EnableIf<(sizeof...(Ks) > 0)> = 0>
constexpr auto copy(Map<Ts...>& to, const Map<Ks...>& from) {
  using consume = int[];
  (void)consume{1, (copy_single(Ks{}, to, from), 1)...};
}

template <typename... Ts, typename Replacements>
constexpr auto update_seq_indices(const Map<Ts...>& a, Replacements r) {
  return make_map(make_type_set(replace_from_map(Ts{}, r)...), a.values, a.valid);
}

template <typename... Ts, typename Replacements>
constexpr auto update_value_ids(const Map<Ts...>& a, Replacements r) {
  return make_map(make_type_set(replace_from_map(Ts{}, r)...), a.values, a.valid);
}

template <typename... Ks, typename... Ts>
constexpr auto filter_missing_keys_impl(TypeSet<Ks...> keys, const Map<Ts...>& a) {
  return make_map(
    keys
    , std::array<Real, sizeof...(Ks)>{{get(Ks{}, a)...}}
    , std::array<bool, sizeof...(Ks)>{{valid(Ks{}, a)...}}
  );
}

template <typename T, typename... Ts>
constexpr auto filter_missing_keys(baseexpr<T>, const Map<Ts...>& a) {
  constexpr auto vs = make_list(values(T{}));
  constexpr auto va = make_list(vars(T{}));
  constexpr auto ps = make_list(params(T{}));
  constexpr auto ns = make_list(named_params(T{}));
  constexpr auto rs = make_list(refs(T{}));
  constexpr auto keys = make_type_set(concat(vs, va, ps, ns, rs));
  return filter_missing_keys_impl(keys.Intersection(a.Keys()), a);
}


} // end namespace aether

#endif
