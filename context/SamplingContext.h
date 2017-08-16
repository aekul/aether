#ifndef AETHER_CONTEXT_SAMPLING_CONTEXT_H
#define AETHER_CONTEXT_SAMPLING_CONTEXT_H

#include "aether/Context.h"
#include "aether/typelist/IndexOf.h"

namespace aether {

struct SamplingContext : Context<SamplingContext> {
  template <typename Fn, typename... Args>
  constexpr auto constant_call(Fn&& fn, Args&&... args);

  template <typename T, typename... Args>
  auto Sample(const discrete_random_var_dynamic<T>& rv, Args&&... args);

  template <typename Sampler>
  Real Uniform1D(Sampler& sampler);

  template <typename Sampler>
  std::array<Real, 2> Uniform2D(Sampler& sampler);

  template <typename Sampler>
  std::array<Real, 2> PiecewiseConstant2D(Sampler& sampler, const Distribution2D *distribution);

  inline bool MatchTargetSize(const std::size_t size);
};

inline SamplingContext make_sampling_context() {
  return {};
}


template <typename Fn, typename... Args>
constexpr auto SamplingContext::constant_call(Fn&& fn, Args&&... args) {
  return std::forward<Fn>(fn)(std::forward<Args>(args)...);
}

template <typename Sampler>
Real SamplingContext::Uniform1D(Sampler& sampler) {
  return sampler.Uniform1D();
}

template <typename Sampler>
std::array<Real, 2> SamplingContext::Uniform2D(Sampler& sampler) {
  return sampler.Uniform2D();
}

template <typename Sampler>
std::array<Real, 2> SamplingContext::PiecewiseConstant2D(Sampler& sampler, const Distribution2D *distribution) {
  return distribution->Sample(sampler.Uniform2D());
}

template <typename T, typename... Args>
auto SamplingContext::Sample(const discrete_random_var_dynamic<T>& rv, Args&&... args) {
  return rv.Sample(std::forward<Args>(args)...);
}

inline bool SamplingContext::MatchTargetSize(const std::size_t size) {
  return true;
}

} // end namespace aether

#endif
