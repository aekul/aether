#ifndef AETHER_FWD_MATH_H
#define AETHER_FWD_MATH_H

#include <Eigen/Dense>

namespace aether {

template <typename T>
constexpr T epsilon = T(0.000001);

template <typename T>
constexpr T pi = T(EIGEN_PI);

template <typename T>
constexpr T one_over_pi = T(1) / pi<T>;

template <typename T>
constexpr T one_over_two_pi = T(1) / (T(2) * pi<T>);

template <typename T>
constexpr T two_pi = T(2) * pi<T>;


//using Real = float;
using Real = double;
using Vector2 = Eigen::Matrix<Real, 2, 1>;
using Vector3 = Eigen::Matrix<Real, 3, 1>;
using Matrix3x2 = Eigen::Matrix<Real, 3, 2>;
using Matrix3x3 = Eigen::Matrix<Real, 3, 3>;

//inline Real det(const Matrix3x2& m);

inline Real dot(const Vector3& a, const Vector3& b);

inline Vector3 cross(const Vector3& a, const Vector3& b);

inline Vector3 normalize(const Vector3& a);

inline Vector3 to_vector3(const std::array<Real, 3>& a);

inline Real fmax(const Vector3& a);

inline Real length(const Vector3& a);

inline Real rcp(const Real& a);

inline Vector3 rcp(const Vector3& a);

inline Real abs(const Real& a);

} // end namespace aether

#endif
