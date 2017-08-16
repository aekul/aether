#ifndef SAMPLER_H
#define SAMPLER_H

#include <array>
#include "fwd/Math.h"

namespace aether {

struct BaseSampler {
  virtual Real operator()() = 0;

  virtual Real Uniform1D(int) = 0;

  virtual Real Uniform1D() = 0;

  virtual std::array<Real, 2> Uniform2D(int) = 0;

  virtual std::array<Real, 2> Uniform2D() = 0;
};

}

#endif
