#ifndef TYPELIST_TYPESET_H
#define TYPELIST_TYPESET_H

#include "aether/fwd/typelist/TypeSet.h"
#include "aether/typelist/Intersection.h"
#include "aether/typelist/Append.h"

namespace aether {

template <typename... Ts>
template <typename R, EnableIf<!has_key<TypeSet<Ts...>, R>::value>>
constexpr TypeSet<Ts..., R> TypeSet<Ts...>::Put(R) const {
  return {};
}

template <typename... Ts>
template <typename R, EnableIf<has_key<TypeSet<Ts...>, R>::value>>
constexpr TypeSet<Ts...> TypeSet<Ts...>::Put(R) const {
  return {};
}

template <typename... Ts>
template <typename T>
constexpr bool TypeSet<Ts...>::Has() const {
  return std::is_base_of<TypeSetKey<T>, TypeSet<Ts...>>::value;
}

template <typename... Ts>
template <typename T>
constexpr bool TypeSet<Ts...>::Has(T) const {
  return std::is_base_of<TypeSetKey<T>, TypeSet<Ts...>>::value;
}

template <typename... Ts>
template <typename R, EnableIf<!has_key<TypeSet<Ts...>, R>::value>>
constexpr TypeSet<Ts...> TypeSet<Ts...>::Remove(R) const {
  return {};
}

template <typename... Ts>
template <typename... Rs>
constexpr TypeSet<Rs...> TypeSet<Ts...>::RemoveImpl(TypeList<Rs...>) const {
  return {};
}

template <typename... Ts>
template <typename R, EnableIf<has_key<TypeSet<Ts...>, R>::value>>
constexpr auto TypeSet<Ts...>::Remove(R) const {
  return RemoveImpl(remove_first(R{}, TypeList<Ts...>{}));
}

template <typename... Ts>
constexpr TypeSet<Ts...> TypeSet<Ts...>::Merge(TypeSet<>) const {
  return TypeSet<Ts...>();
}

template <typename... Ts>
template <typename R, typename... Rs>
constexpr auto TypeSet<Ts...>::Merge(TypeSet<R, Rs...>) const {
  return Put(R{}).Merge(TypeSet<Rs...>());
}

template <typename... Ts>
constexpr TypeSet<Ts...> TypeSet<Ts...>::Merge(TypeList<>) const {
  return TypeSet<Ts...>();
}

template <typename... Ts>
template <typename R, typename... Rs>
constexpr auto TypeSet<Ts...>::Merge(TypeList<R, Rs...>) const {
  return Put(R{}).Merge(TypeList<Rs...>{});
}

template <typename... Ts>
constexpr TypeSet<Ts...> TypeSet<Ts...>::Difference(TypeSet<>) const {
  return {};
}

template <typename... Ts>
template <typename R, typename... Rs>
constexpr auto TypeSet<Ts...>::Difference(TypeSet<R, Rs...>) const {
  return Remove(R{}).Difference(TypeSet<Rs...>{});
}

template <typename... Ts>
constexpr TypeSet<> TypeSet<Ts...>::Intersection(TypeSet<>) const {
  return {};
}

template <typename... Ts>
template <typename R, typename... Rs, EnableIf<!has_key<TypeSet<Ts...>, R>::value>>
constexpr auto TypeSet<Ts...>::Intersection(TypeSet<R, Rs...>) const {
  return Intersection(TypeSet<Rs...>{});
}

template <typename... Ts>
template <typename R, typename... Rs, EnableIf<has_key<TypeSet<Ts...>, R>::value>>
constexpr auto TypeSet<Ts...>::Intersection(TypeSet<R, Rs...>) const {
  return make_type_set(List(R{}).Concat(make_list(Intersection(TypeSet<Rs...>{}))));
}

template <typename... Ts>
template <typename... Rs>
constexpr bool TypeSet<Ts...>::operator==(TypeSet<Rs...>) const {
  return (contains(Rs{}, TypeList<Ts...>{}) && ... && true)
    && (contains(Ts{}, TypeList<Rs...>{}) && ... && true);
}

template <typename... Ts>
template <typename... Rs>
constexpr bool TypeSet<Ts...>::operator!=(TypeSet<Rs...>) const {
  return !this->operator==(TypeSet<Rs...>{});
}

template <typename... Ts>
constexpr int TypeSet<Ts...>::Size() const {
  return sizeof...(Ts);
}

template <typename... Ts>
constexpr bool TypeSet<Ts...>::IsEmpty() const {
  return Size() == 0;
}

template <typename... Ts>
constexpr TypeSet<Ts...> make_type_set(Ts...) {
  return {};
}

template <typename... Ts>
constexpr TypeSet<Ts...> make_type_set(TypeList<Ts...>) {
  return {};
}

template <typename... Ts>
constexpr TypeSet<Ts...> make_type_set(TypeSet<Ts...>) {
  return {};
}

template <typename... Ts>
constexpr auto make_list(TypeSet<Ts...>) {
  return TypeList<Ts...>{};
}

constexpr TypeSet<> MergeAll() {
  return {};
}

template <typename... Ts>
constexpr auto MergeAll(const TypeSet<Ts...>& A) {
  return A;
}

template <typename... Ts, typename... Rs, typename... Others>
constexpr auto MergeAll(const TypeSet<Ts...>& A, const TypeSet<Rs...>& B, const Others&...) {
  return MergeAll(A.Merge(B), Others()...);
}

template <typename... Ts>
constexpr auto merge_list_of_sets(TypeList<Ts...>) {
  return MergeAll(Ts{}...);
}

template <typename... Bs>
constexpr TypeList<> difference_all_impl(TypeList<Bs...>, TypeList<>) {
  return {};
}

template <typename... Bs, typename T, typename... Ts>
constexpr auto difference_all_impl(TypeList<Bs...> done, TypeList<T, Ts...>) {
  constexpr auto unique = T{}.Difference(MergeAll(Bs{}..., Ts{}...));
  return List(unique).Concat(difference_all_impl(append(done, T{}), TypeList<Ts...>{}));
}

template <typename... Ts>
constexpr auto difference_all(TypeList<Ts...> sets) {
  return difference_all_impl(TypeList<>{}, sets);
}

template <typename... Ts>
constexpr auto make_type_set_all(TypeList<Ts...>) {
  return List(make_type_set(Ts{})...);
}

template <std::size_t I, typename Set>
struct type_at;

template <typename T, typename... Ts>
struct type_at<0, TypeSet<T, Ts...>> {
  using type = T;
};

template <std::size_t I, typename T, typename... Ts>
struct type_at<I, TypeSet<T, Ts...>> : type_at<I - 1, TypeSet<Ts...>> {
};

template <typename T, typename... Ts>
struct type_at<0, TypeList<T, Ts...>> {
  using type = T;
};

template <std::size_t I, typename T, typename... Ts>
struct type_at<I, TypeList<T, Ts...>> : type_at<I - 1, TypeList<Ts...>> {
};

template <std::size_t I, typename Set>
using type_at_t = typename type_at<I, Set>::type;


template <typename Container>
struct back_type;

template <typename... Ts>
struct back_type<TypeSet<Ts...>> : type_at<TypeSet<Ts...>{}.Size() - 1, TypeSet<Ts...>> {};

template <typename Set>
using back_type_t = typename back_type<Set>::type;


} // end namespace aether

#endif
