#ifndef REPLACE_H
#define REPLACE_H

#include "Expr.h"
#include "typelist/TypeMap.h"

namespace aether {

template <typename T, typename Map, EnableIf<is_leaf(T{}) && !Map{}.HasKey(T{})> = 0>
constexpr T replace_from_map(baseexpr<T>, Map) {
  return {};
}

template <typename... Ts, typename Map>
constexpr auto replace_from_map(TypeList<Ts...>, Map) {
  return List(replace_from_map(Ts{}, Map{})...);
}

template <typename T, typename Map, EnableIf<!Map{}.HasKey(varsubexpr<T>{})> = 0>
constexpr auto replace_from_map(varsubexpr<T>, Map) {
  return make_expr(var_subexpr_tag, replace_from_map(T{}, Map{}));
}

template <typename T, typename Map, EnableIf<Map{}.HasKey(varsubexpr<T>{})> = 0>
constexpr auto replace_from_map(varsubexpr<T>, Map) {
  constexpr auto key = T{};
  return make_expr(var_subexpr_tag, replace_from_map(Map{}[key], Map{}));
}

template <typename T, typename Map, EnableIf<Map{}.HasKey(T{})> = 0>
constexpr auto replace_from_map(baseexpr<T>, Map m) {
  constexpr auto key = T{};
  return replace_from_map_key_found(T{}, m[key], m);
}

template <typename Value, typename Map>
constexpr Value replace_from_map_key_found(baseexpr<Value>, baseexpr<Value>, Map) {
  return {};
}

template <typename Key, typename Value, typename Map>
constexpr auto replace_from_map_key_found(baseexpr<Key>, baseexpr<Value>, Map m) {
  return replace_from_map(Value{}, m);
}

template <typename Key, typename C, typename V, typename Map>
constexpr auto replace_from_map_key_found(baseexpr<Key>, branchcaseexpr<C, V>, Map m) {
  return make_expr(
    branch_case_tag
    , replace_from_map(C{}, m.Put(Key{}, V{}))
    , replace_from_map(V{}, m)
  );
}

template <typename Key, typename... Ts, typename Map>
constexpr auto replace_from_map_key_found(baseexpr<Key>, branchexpr<Ts...>, Map m) {
  return make_branch_expr(
    List(replace_from_map(get_condition(Ts{}), m.Put(Key{}, get_value(Ts{})))...)
    , List(replace_from_map(get_value(Ts{}), m)...)
  );
}

template <typename T, typename Map, EnableIf<!is_leaf(T{}) && !Map{}.HasKey(T{})> = 0>
constexpr auto replace_from_map(baseexpr<T>, Map) {
  return make_expr(tag_of(T{}), replace_from_map(operands(T{}), Map{}));
}

template <typename T, typename Map>
constexpr T replace_from_map(coordinate_system<T>, Map) {
  return {};
}

template <std::size_t I, typename Map>
constexpr _size_t<I> replace_from_map(_size_t<I> i, Map) {
  return i;
}

template <typename... Ts, typename Map, EnableIf<Map{}.HasValue(branchexpr<Ts...>{})> = 0>
constexpr auto replace_from_map(branchexpr<Ts...> br, Map m) {
  constexpr auto key = m.GetKeyForValue(br);
  return make_branch_expr(
    List(replace_from_map(get_condition(Ts{}), m.Put(key, get_value(Ts{})))...)
    , List(replace_from_map(get_value(Ts{}), m)...)
  );
}

template <typename C, typename V, typename Map, EnableIf<Map{}.HasValue(branchcaseexpr<C, V>{})> = 0>
constexpr auto replace_from_map_key_found(branchcaseexpr<C, V> br, Map m) {
  constexpr auto key = m.GetKeyForValue(br);
  return make_expr(
    branch_case_tag
    , replace_from_map(C{}, m.Put(key, V{}))
    , replace_from_map(V{}, m)
  );
}

template <typename T>
constexpr T replace_from_map(baseexpr<T>, TypeMap<TypeList<>, TypeList<>>) {
  return {};
}


} // end namespace aether

#endif
