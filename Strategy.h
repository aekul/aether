#ifndef AETHER_STRATEGY_H
#define AETHER_STRATEGY_H

#include <boost/hana.hpp>
#include <boost/optional.hpp>

using namespace boost;

#include "aether/fwd/TypeTraits.h"
#include "aether/fwd/RandomSequence.h"
#include "aether/Sampler.h"

namespace aether {

template <std::size_t I>
struct ignore {
  template <typename T>
  constexpr ignore(T&&) {}
};

template <typename Indices>
struct extract_impl;

template <std::size_t... Is>
struct extract_impl<std::index_sequence<Is...>> {
  template <typename T, typename... Ts>
  static decltype(auto) apply(ignore<Is>..., T&& t, Ts&&...) {
    return std::forward<T>(t);
  }
};

template <std::size_t I, typename... Ts>
decltype(auto) extract(Ts&&... ts) {
  return extract_impl<std::make_index_sequence<I>>::apply(std::forward<Ts>(ts)...);
}

template <typename T>
struct node_traits_t;


template <typename T>
struct Node;


template <typename T>
struct node_traits_t {
  using args_t = fn_args_t<T>;
  Ty<args_t> s{};

  template <typename Args>
  struct unpack_args;

  template <typename... Args>
  struct unpack_args<TypeList<Args...>> {
    using return_t = decltype(std::declval<T>()(std::declval<Args>()...));
  };

  using return_t = typename unpack_args<args_t>::return_t;
};

} // end namespace aether

#endif
