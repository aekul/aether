#ifndef AETHER_SHAPES_H
#define AETHER_SHAPES_H

#include "fwd/Shapes.h"
#include "CompositeRandomVar.h"
#include "RandomVar.h"
#include "Expr.h"
#include "Literal.h"
#include "Param.h"
#include "Variable.h"
#include "NamedParameter.h"

namespace aether {

NAMED_PARAM(f);

struct uniform_hemisphere_ : Function<uniform_hemisphere_> {
  uniform_hemisphere_() {}
  uniform_hemisphere_(const Vector3& r) : R(r) {}
  uniform_hemisphere_(Real r, Real g, Real b) : uniform_hemisphere_(Vector3{r, g, b}) {}

  constexpr auto RV() const {
    constexpr auto pi = literal<245850922, 78256779>{};
    constexpr auto r = sqrt(one - sq(u1));
    constexpr auto phi = two * pi * u2;
    constexpr auto x = r * cos(phi);
    constexpr auto y = r * sin(phi);
    constexpr auto z = u1;
    return make_random_vector(make_random_var(x), make_random_var(y), make_random_var(z));
  }

  auto Sample(Real u, Real v) const {
    return RV().Sample(u, v);
  }

  template <typename LocalToWorld>
  auto Sample(const LocalToWorld& local_to_world, Real u, Real v) const {
    auto wi = (local_to_world * RV()).Sample(u, v);
    return wi;
  }

  template <typename Wo, typename Normal, typename LocalToWorld>
  auto Sample(const Wo&, const Normal& N, const LocalToWorld& local_to_world, Real u, Real v) const {
    auto wi = (local_to_world * RV()).Sample(u, v);
    return wi;
  }

  auto impl(Real u, Real v) const {
    return RV().Sample(u, v);
  }

  template <typename LocalToWorld>
  auto impl(const LocalToWorld& local_to_world, Real u, Real v) const {
    auto wi = (local_to_world * RV()).Sample(u, v);
    return wi;
  }

  template <typename Wo, typename Normal, typename LocalToWorld>
  auto impl(const Wo&, const Normal& N, const LocalToWorld& local_to_world, Real u, Real v) const {
    auto wi = (local_to_world * RV()).Sample(u, v);
    return wi;
  }

  Vector3 R;
};


template <>
struct F_<uniform_hemisphere_> {
  template <int I, typename Cond>
  auto operator()(const uniform_hemisphere_& hemi, _int<I>, Cond, const Vector3& normal, const Vector3& wo, const Vector3& wi, const Vector2&, Real) const {
    Vector3 result = (dot(wi, normal) > 0 && dot(wo, normal) > 0) ? Vector3(hemi.R / pi<Real>) : Vector3(0, 0, 0);
    return value(result).Sample();
  }
};

template <>
struct SampleCall<uniform_hemisphere_> {
  template <int I, typename Index>
  auto operator()(const uniform_hemisphere_& hemi, _int<I>, Index, Real u, Real v, Real) const {
    return hemi.Sample(u, v);
  }

  template <int I, typename Index, typename Wo, typename Normal, typename LocalToWorld>
  auto operator()(const uniform_hemisphere_& hemi, _int<I>, Index, const Wo& wo, const Normal& N, const LocalToWorld& local_to_world, Real u, Real v, Real) const {
    return hemi.Sample(wo, N, local_to_world, u, v);
  }
};

struct uniform_hemisphere2 {
  uniform_hemisphere2() {}
  uniform_hemisphere2(const Vector3& r) : R(r) {}

  constexpr auto RV() const {
    constexpr auto pi = literal<245850922, 78256779>{};
    constexpr auto r = sqrt(one - sq(u1));
    constexpr auto phi = two * pi * u2;
    constexpr auto x = r * cos(phi);
    constexpr auto y = r * sin(phi);
    constexpr auto z = u1;
    return make_random_vector(make_random_var(x), make_random_var(y), make_random_var(z));
  }

  auto Sample(Real u, Real v) const {
    return RV().Sample(u, v);
  }

  template <typename Wo, typename Normal, typename LocalToWorld>
  auto Sample(const Wo&, const Normal&, const LocalToWorld& local_to_world, Real u, Real v) const {
    return (local_to_world * RV()).Sample(u, v);
  }

  Vector3 R;
};

template <>
struct F_<uniform_hemisphere2> {
  template <int I, typename Cond>
  auto operator()(const uniform_hemisphere2& hemi, _int<I>, Cond, const Vector3& normal, const Vector3& wo, const Vector3& wi, const Vector2&, Real) const {
    Vector3 result = (dot(wi, normal) > Real(0)) ? Vector3(hemi.R / pi<Real>) : Vector3(0, 0, 0);
    return value(result).Sample();
  }
};


template <>
struct SampleCall<uniform_hemisphere2> {
  template <int I, typename Index>
  auto operator()(const uniform_hemisphere2& hemi, _int<I>, Index, Real u, Real v, Real) const {
    return hemi.Sample(u, v);
  }

  template <int I, typename Index, typename Wo, typename Normal, typename LocalToWorld>
  auto operator()(const uniform_hemisphere2& hemi, _int<I>, Index, const Wo& wo, const Normal& N, const LocalToWorld& local_to_world, Real u, Real v, Real) const {
    return hemi.Sample(wo, N, local_to_world, u, v);
  }
};


NAMED_PARAM(v0);
NAMED_PARAM(v1);
NAMED_PARAM(v2);

template <int ID>
struct intersect_triangle_t : Function<intersect_triangle_t<ID>> {
  template <typename Org, typename Dir>
  auto impl(const Vector3& tri_v0, const Vector3& tri_v1, const Vector3& tri_v2, const Org& org, const Dir& dir) const {
    auto v0 = named_constant<ID>(v0_, tri_v0);
    auto v1 = named_constant<ID>(v1_, tri_v1);
    auto v2 = named_constant<ID>(v2_, tri_v2);

    auto e1 = v0 - v2;
    auto e2 = v1 - v2;
    auto N = cross(e1, e2); 

    auto t = dot(v0 - org, N) / dot(dir, N);

    return org + t * dir;
  }

  template <typename Org, typename Dir>
  auto impl(const std::array<Real, 3>& tri_v0, const std::array<Real, 3>& tri_v1, const std::array<Real, 3>& tri_v2, const Org& org, const Dir& dir) const {
    return impl(to_vector3(tri_v0), to_vector3(tri_v1), to_vector3(tri_v2), org, dir);
  }
};

template <int ID>
constexpr intersect_triangle_t<ID> intersect_triangle{};


}

#endif
