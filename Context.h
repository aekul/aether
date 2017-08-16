#ifndef AETHER_CONTEXT_H
#define AETHER_CONTEXT_H

#include "aether/RandomVar.h"
#include "aether/DynamicRandomVar.h"

#include <boost/optional.hpp>

namespace aether {

template <typename T>
struct Context {
  T& Derived() {
    return static_cast<T&>(*this);
  }

  const T& Derived() const {
    return static_cast<const T&>(*this);
  }

  template <typename Fn, typename... Args>
  constexpr auto constant_call(Fn&& fn, Args&&... args);

  template <typename V, typename... Args>
  auto Sample(const discrete_random_var_dynamic<V>& rv, Args&&... args);

  template <typename Sampler>
  Real Uniform1D(Sampler& sampler);

  template <typename Sampler>
  std::array<Real, 2> Uniform2D(Sampler& sampler);

  template <typename Sampler>
  std::array<Real, 2> PiecewiseConstant2D(Sampler& sampler, const Distribution2D *distribution);

  // Should only be used when the target is RandomSequence
  inline bool MatchTargetSize(const std::size_t size);

protected:
  Context() = default;

private:
  Context& operator=(const Context&) = delete;
};

template <typename T>
template <typename Fn, typename... Args>
constexpr auto Context<T>::constant_call(Fn&& fn, Args&&... args) {
  return Derived().constant_call(std::forward<Fn>(fn), std::forward<Args>(args)...);
}

template <typename T>
template <typename Sampler>
Real Context<T>::Uniform1D(Sampler& sampler) {
  return Derived().Uniform1D(sampler);
}

template <typename T>
template <typename Sampler>
std::array<Real, 2> Context<T>::Uniform2D(Sampler& sampler) {
  return Derived().Uniform2D(sampler);
}

template <typename T>
template <typename Sampler>
std::array<Real, 2> Context<T>::PiecewiseConstant2D(Sampler& sampler, const Distribution2D *distribution) {
  return Derived().PiecewiseConstant2D(sampler, distribution);
}

template <typename T>
template <typename V, typename... Args>
auto Context<T>::Sample(const discrete_random_var_dynamic<V>& rv, Args&&... args) {
  return Derived().Sample(rv, std::forward<Args>(args)...);
}

template <typename T>
inline bool Context<T>::MatchTargetSize(const std::size_t size) {
  return Derived().MatchTargetSize(size);
}

} // end namespace aether

#endif
