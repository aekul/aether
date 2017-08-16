#ifndef AETHER_UNIFORM_TRIANGLE_H
#define AETHER_UNIFORM_TRIANGLE_H

#include "fwd/Shapes.h"
#include "fwd/Math.h"
#include "Expr.h"
#include "Literal.h"

namespace aether {

struct uniform_triangle_t {
  auto operator()(const Vector3& tri_v0, const Vector3& tri_v1, const Vector3& tri_v2) const {
    auto v0 = constant(tri_v0);
    auto v1 = constant(tri_v1);
    auto v2 = constant(tri_v2);

    constexpr auto a = sqrt(u1);
    constexpr auto b1 = one - a;
    constexpr auto b2 = u2 * a;

    return b1 * v0 + b2 * v1 + (one - b1 - b2) * v2;
  }

  template <typename V0, typename V1, typename V2>
  auto operator()(const V0& v0, const V1& v1, const V2& v2) const {
    constexpr auto a = sqrt(u1);
    constexpr auto b1 = one - a;
    constexpr auto b2 = u2 * a;

    return b1 * v0 + b2 * v1 + (one - b1 - b2) * v2;
  }

  constexpr auto operator()() const {
    constexpr auto a = sqrt(u1);
    return make_random_vector(make_random_var(one - a), make_random_var(u2 * a));
  }
};

constexpr uniform_triangle_t uniform_triangle{};



}

#endif
