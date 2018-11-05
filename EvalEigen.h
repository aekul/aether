#ifndef AETHER_EVAL_EIGEN_H
#define AETHER_EVAL_EIGEN_H

#include "Add.h"
#include "Literal.h"
#include "Param.h"
#include "Map.h"
#include "Pow.h"
#include "Replace.h"
#include "Product.h"
#include "Trig.h"
#include <boost/hana.hpp>
#include "fwd/typelist/TypeList.h"
#include "Variable.h"
#include "Vector.h"
#include "fwd/Math.h"

static bool debug_eigen = false;

namespace aether {
using namespace Eigen;

template <typename T>
using result_t = typename std::conditional<
  dimensions_of(T{}).IsScalar()
  , Real
  , typename std::conditional<
    is_vector(T{})
    , typename std::conditional<
      dimensions_of(T{}).Rows() == 2
      , Vector2
      , Vector3
    >::type
    , typename std::conditional<
      dimensions_of(T{}).Cols() == 3
      , Matrix3x3
      , Matrix3x2
    >::type
  >::type
>::type;

//inline std::ostream& operator<<(std::ostream& out, const Vector3& v) {
  //return out << "[" << v[0] << ", " << v[1] << ", " << v[2] << "]";
//}

template <int Rows, int Cols>
inline Real det(const Eigen::Matrix<Real, Rows, Cols>& m) {
  //std::cerr << m << std::endl;
  return std::sqrt((m.transpose() * m).determinant());
}

inline Real dot(const Vector3& a, const Vector3& b) {
  return a.dot(b);
}

inline Vector3 cross(const Vector3& a, const Vector3& b) {
  return a.cross(b);
}

inline Vector3 normalize(const Vector3& a) {
  return a.normalized();
}

inline Vector3 Value(const Vector3& a) {
  return a;
}

inline Vector3 to_vector3(const std::array<Real, 3>& a) {
  return Vector3(a[0], a[1], a[2]);
}

inline Real fmax(const Vector3& a) {
  return std::fmax(std::fmax(a.x(), a.y()), a.z());
}

inline Real length(const Vector3& a) {
  return a.norm();
}

inline Real rcp(const Real& a) {
  return Real(1) / a;
}

inline Vector3 rcp(const Vector3& a) {
  return a.cwiseInverse();
}

inline Real abs(const Real& a) {
  return std::fabs(a);
}

inline Real clamp(const Real& a) {
  return std::max(Real(0), std::min(a, Real(1)));
}

template <int A, int B>
inline Eigen::Matrix<Real, A + B, 1> concatenate_vectors(const Eigen::Matrix<Real, A, 1>& a, const Eigen::Matrix<Real, B, 1>& b) {
  Eigen::Matrix<Real, A + B, 1> result;
  result << a, b;
  return result;
}

template <int ID, int SeqIndex, typename Vars>
inline result_t<variable<ID, SeqIndex>> eval_eigen(variable<ID, SeqIndex>, const Vars& vars) {
  return get<variable<ID, SeqIndex>>(vars);
}

template <int ID, typename Vars>
inline result_t<inverse_variable<ID>> eval_eigen(inverse_variable<ID>, const Vars& vars) {
  return get<inverse_variable<ID>>(vars);
}

template <int ID, std::size_t N, std::size_t I, int SeqIndex, typename Vars>
inline result_t<param<ID, N, I, SeqIndex>> eval_eigen(param<ID, N, I, SeqIndex>, const Vars& vars) {
  return get<param<ID, N, I, SeqIndex>>(vars);
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex, typename Vars>
inline result_t<named_param<T, ID, N, I, SeqIndex>> eval_eigen(named_param<T, ID, N, I, SeqIndex>, const Vars& vars) {
  return get<named_param<T, ID, N, I, SeqIndex>>(vars);
}

template <int ID, std::size_t N, std::size_t I, typename Vars>
inline result_t<valueexpr<ID, N, I>> eval_eigen(valueexpr<ID, N, I>, const Vars& vars) {
  return get<valueexpr<ID, N, I>>(vars);
}

template <std::intmax_t N, std::intmax_t D, typename Vars>
inline result_t<literal<N, D>> eval_eigen(literal<N, D>, const Vars&) {
  return Real(N) / Real(D);
}


template <typename... Ts, typename Vars>
inline result_t<productexpr<Ts...>> eval_eigen(productexpr<Ts...>, const Vars& vars) {
  return result_t<productexpr<Ts...>>((... * eval_eigen(Ts{}, vars)));
}


template <typename... Ts, typename Vars>
inline result_t<addexpr<Ts...>> eval_eigen(addexpr<Ts...>, const Vars& vars) {
  return result_t<addexpr<Ts...>>((... + eval_eigen(Ts{}, vars)));
}

template <std::size_t I, typename T, typename Vars>
inline result_t<indexaccessexpr<I, T>> eval_eigen(indexaccessexpr<I, T>, const Vars& vars) {
  auto result = eval_eigen(T{}, vars);
  return result[I];
}


template <typename T, typename Vars>
inline result_t<T> eval_eigen(matrixinverseexpr<T>, const Vars& vars) {
  auto m = eval_eigen(T{}, vars);
  auto result = m.inverse();
  return result;
}


template <typename S, typename D, typename Vars>
inline result_t<crossproduct<S, D>> eval_eigen(crossproduct<S, D>, const Vars& vars) {
  auto a = eval_eigen(S{}, vars);
  auto b = eval_eigen(D{}, vars);
  return cross(a, b);
}

template <typename S, typename D, typename Vars>
inline result_t<dotproduct<S, D>> eval_eigen(dotproduct<S, D>, const Vars& vars) {
  auto a = eval_eigen(S{}, vars);
  auto b = eval_eigen(D{}, vars);
  return dot(a, b);
}

template <typename T, typename Vars>
inline result_t<vectorlength<T>> eval_eigen(vectorlength<T>, const Vars& vars) {
  auto vec = eval_eigen(T{}, vars);

  return length(vec);
}

template <typename T, std::size_t... Is, typename... Ts>
inline void eval_eigen_unpack_matrix(T& m, std::index_sequence<Is...>, Ts&&... ts) {
  using consume = int[];
  (void)consume{1, (m.col(Is) = std::forward<Ts>(ts), 1)...};
}

template <typename T, std::size_t... Is, typename... Ts>
inline void eval_eigen_unpack_vector(T& m, std::index_sequence<Is...>, Ts&&... ts) {
  using consume = int[];
  (void)consume{1, (m(Is) = std::forward<Ts>(ts), 1)...};
}


template <typename... Ts, typename Vars>
inline result_t<rotationmatrixexpr<Ts...>> eval_eigen(rotationmatrixexpr<Ts...>, const Vars& vars) {
  result_t<rotationmatrixexpr<Ts...>> result;
  eval_eigen_unpack_matrix(result, std::index_sequence_for<Ts...>{}, eval_eigen(Ts{}, vars)...);
  return result;
}

template <typename T, typename Vars>
inline result_t<determinantexpr<T>> eval_eigen(determinantexpr<T>, const Vars& vars) {
  result_t<T> result(eval_eigen(T{}, vars));
  return result.det();
}

template <typename C, typename... Vs, typename Vars, EnableIf<(is_vector(Vs{}) && ... && true)> = 0>
inline auto concatenate_vectors(vectorexpr<C, Vs...>, const Vars& vars) {
  return concatenate_vectors(eval_eigen(Vs{}, vars)...);
}

template <typename C, typename... Vs, typename... Ts, typename Vars, EnableIf<(is_vector(Vs{}) && ... && true)> = 0>
inline auto eval_eigen(matrixexpr<vectorexpr<C, Vs...>, Ts...>, const Vars& vars) {
  constexpr auto num_rows = dimensions_of(vectorexpr<C, Vs...>{}).Rows() * dimensions_of(at<0>(vectorexpr<C, Vs...>{})).Rows();

  Eigen::Matrix<Real, num_rows, 1 + sizeof...(Ts)> result;

  eval_eigen_unpack_matrix(
    result
    , std::make_index_sequence<1 + sizeof...(Ts)>{}
    , concatenate_vectors(vectorexpr<C, Vs...>{}, vars), concatenate_vectors(Ts{}, vars)...
  );
  return result;
}

template <typename... Ts, typename Vars>
inline result_t<matrixexpr<Ts...>> eval_eigen(matrixexpr<Ts...>, const Vars& vars) {
  result_t<matrixexpr<Ts...>> result;
  eval_eigen_unpack_matrix(result, std::index_sequence_for<Ts...>{}, eval_eigen(Ts{}, vars)...);
  return result;
}

template <typename C, typename... Ts, typename Vars>
inline result_t<vectorexpr<C, Ts...>> eval_eigen(vectorexpr<C, Ts...>, const Vars& vars) {
  result_t<vectorexpr<C, Ts...>> result;
  eval_eigen_unpack_vector(result, std::index_sequence_for<Ts...>{}, eval_eigen(Ts{}, vars)...);
  return result;
}




template <typename T, typename E, typename Vars>
inline result_t<powerexpr<T, E>> eval_eigen(powerexpr<T, E>, const Vars& vars) {
  auto base = eval_eigen(T{}, vars);
  auto exponent = eval_eigen(E{}, vars);
  return std::pow(base, exponent);
}

template <typename T, typename Vars>
inline result_t<indicatorexpr<T>> eval_eigen(indicatorexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return arg;
}

template <typename T, typename Vars>
inline result_t<absexpr<T>> eval_eigen(absexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return std::abs(arg);
}

template <typename T, typename Vars>
inline result_t<expexpr<T>> eval_eigen(expexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return std::exp(arg);
}

template <typename T, typename Vars>
inline result_t<logexpr<T>> eval_eigen(logexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return std::log(arg);
}

template <typename T, typename Vars>
inline result_t<sinexpr<T>> eval_eigen(sinexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return std::sin(arg);
}

template <typename T, typename Vars>
inline result_t<arcsinexpr<T>> eval_eigen(arcsinexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return std::asin(arg);
}

template <typename T, typename Vars>
inline result_t<cosexpr<T>> eval_eigen(cosexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return std::cos(arg);
}

template <typename T, typename Vars>
inline result_t<arccosexpr<T>> eval_eigen(arccosexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return std::acos(arg);
}

template <typename T, typename Vars>
inline result_t<arctanexpr<T>> eval_eigen(arctanexpr<T>, const Vars& vars) {
  auto arg = eval_eigen(T{}, vars);
  return std::atan(arg);
}

template <typename... Ts, typename Vars>
inline result_t<arctanexpr<productexpr<Ts...>>> eval_eigen(arctanexpr<productexpr<Ts...>>, const Vars& vars) {
  auto num = eval_eigen(numerator(productexpr<Ts...>{}), vars);
  auto denom = eval_eigen(denominator(productexpr<Ts...>{}), vars);
  Real result = std::atan2(num, denom);

  if (result < 0) {
    result += two_pi<Real>;
  }

  return result;
}


template <typename A, typename B, typename Vars>
inline result_t<arctan2expr<A, B>> eval_eigen(arctan2expr<A, B>, const Vars& vars) {
  auto a = eval_eigen(A{}, vars);
  auto b = eval_eigen(B{}, vars);
  Real result = std::atan2(a, b);

  if (result < 0) {
    result += two_pi<Real>;
  }

  return result;
}

inline bool eval_eigen_condition_less_than_equal(Real lhs, Real rhs) {
  return lhs <= rhs;
}

inline bool eval_eigen_condition_less_than(Real lhs, Real rhs) {
  return lhs < rhs;
}

inline bool eval_eigen_condition(Real lhs, Real rhs, less_than_tag_t) {
  return eval_eigen_condition_less_than(lhs, rhs);
}

inline bool eval_eigen_condition(Real lhs, Real rhs, less_than_equal_tag_t) {
  return eval_eigen_condition_less_than_equal(lhs, rhs);
}

inline bool eval_eigen_condition(Real lhs, Real rhs, greater_than_tag_t) {
  return !eval_eigen_condition_less_than_equal(lhs, rhs);
}

inline bool eval_eigen_condition(Real lhs, Real rhs, greater_than_equal_tag_t) {
  return !eval_eigen_condition_less_than(lhs, rhs);
}


inline bool eval_eigen_condition_equal(Real lhs, Real rhs) {
  return std::abs(lhs - rhs) < epsilon<Real>;
}

inline bool eval_eigen_condition(Real lhs, Real rhs, equal_tag_t) {
  return eval_eigen_condition_equal(lhs, rhs);
}

inline bool eval_eigen_condition(Real lhs, Real rhs, not_equal_tag_t) {
  return !eval_eigen_condition_equal(lhs, rhs);
}

inline bool eval_eigen_condition(bool lhs, bool rhs, logical_and_tag_t) {
  return lhs && rhs;
}

inline bool eval_eigen_condition(bool lhs, bool rhs, logical_or_tag_t) {
  return lhs || rhs;
}

inline bool eval_eigen_condition(bool lhs, bool rhs, logical_not_tag_t) {
  return !lhs;
}

template <typename Lhs, typename Rhs, typename... Ts, typename Vars>
inline bool eval_eigen(notequalexpr<Lhs, Rhs>, const Vars& vars) {
  auto lhs = eval_eigen(Lhs{}, vars);
  auto rhs = eval_eigen(Rhs{}, vars);
  return lhs != rhs;
}

template <typename Lhs, typename Rhs, typename... Ts, typename Vars>
inline bool eval_eigen(equalexpr<Lhs, Rhs>, const Vars& vars) {
  auto lhs = eval_eigen(Lhs{}, vars);
  auto rhs = eval_eigen(Rhs{}, vars);
  return lhs == rhs;
}

template <typename Lhs, typename Rhs, typename... Ts, typename Vars>
inline bool eval_eigen(greaterthanexpr<Lhs, Rhs>, const Vars& vars) {
  auto lhs = eval_eigen(Lhs{}, vars);
  auto rhs = eval_eigen(Rhs{}, vars);
  return lhs > rhs;
}

template <typename Lhs, typename Rhs, typename... Ts, typename Vars>
inline bool eval_eigen(lessthanexpr<Lhs, Rhs>, const Vars& vars) {
  auto lhs = eval_eigen(Lhs{}, vars);
  auto rhs = eval_eigen(Rhs{}, vars);
  return lhs < rhs;
}

template <typename Lhs, typename Rhs, typename... Ts, typename Vars>
inline bool eval_eigen(lessthanequalexpr<Lhs, Rhs>, const Vars& vars) {
  auto lhs = eval_eigen(Lhs{}, vars);
  auto rhs = eval_eigen(Rhs{}, vars);
  return lhs <= rhs;
}

template <typename T, typename Vars>
inline bool eval_eigen(logicalnotexpr<T>, const Vars& vars) {
  return !eval_eigen(T{}, vars);
}

template <typename... Ts, typename Vars>
inline bool eval_eigen(logicalandexpr<Ts...>, const Vars& vars) {
  return (eval_eigen(Ts{}, vars) && ... && true);
}

template <typename Vars>
inline bool eval_eigen(trueexpr, const Vars& vars) {
  return true;
}

template <typename Vars>
inline bool eval_eigen(falseexpr, const Vars& vars) {
  return false;
}

template <typename V, typename Vars>
inline result_t<V> eval_eigen_branch(TypeList<branchcaseexpr<trueexpr, V>>, const Vars& vars) {
  return eval_eigen(V{}, vars);
}

template <typename C, typename V, typename... Ts, typename Vars>
inline auto eval_eigen_branch(TypeList<branchcaseexpr<C, V>, Ts...>, const Vars& vars) {
  auto result = eval_eigen(C{}, vars);
  if (result) {
    return eval_eigen(V{}, vars);
  }

  return eval_eigen_branch(TypeList<Ts...>{}, vars);
}


template <typename... Ts, typename Vars>
inline result_t<branchexpr<Ts...>> eval_eigen(branchexpr<Ts...>, const Vars& vars) {
  return eval_eigen_branch(TypeList<Ts...>{}, vars);
}

template <typename... Ts, typename Vars>
inline result_t<branchexpr<Ts...>> eval_eigen(balanceexpr<branchexpr<Ts...>>, const Vars& vars) {
  return eval_eigen_branch(TypeList<Ts...>{}, vars);
}

template <typename C, typename T, typename Vars>
inline result_t<T> eval_eigen(balanceexpr<branchcaseexpr<C, T>>, const Vars& vars) {
  return eval_eigen(T{}, vars);
}


template <typename T, typename Vars>
inline result_t<T> eval_eigen(constantsubexpr<T>, const Vars& vars) {
  return eval_eigen(T{}, vars);
}




template <typename T>
struct Conditional {
  Conditional() : valid{false} {}

  template <typename... Vs>
  Conditional(const Vs&... values) : valid{true}, result{values...} {}

  template <typename... Vs>
  Conditional(bool v, const Vs&... values) : valid{v}, result{values...} {}

  operator bool() const {
    return valid;
  }

  const T& Value() const {
    return result;
  }

  bool valid;
  T result;
};

template <typename T>
using result_conditional_t = Conditional<result_t<T>>;



template <typename T, typename Vars>
inline result_conditional_t<T> eval_eigen_conditional(baseexpr<T>, const Vars& vars) {
  auto result = eval_eigen(T{}, vars);
  return result_conditional_t<T>(true, result);
}

template <typename C, typename V, typename Vars>
inline result_conditional_t<branchcaseexpr<C, V>> eval_eigen_conditional(branchcaseexpr<C, V>, const Vars& vars) {
  auto cond = eval_eigen(C{}, vars);
  if (!cond) {
    return result_conditional_t<branchcaseexpr<C, V>>{};
  }
  auto result = eval_eigen(V{}, vars);
  return result_conditional_t<branchcaseexpr<C, V>>(result);
}

template <typename T, typename Vars>
inline result_conditional_t<T> eval_eigen_conditional(constantsubexpr<T>, const Vars&) {
  return eval_eigen_conditional(T{}, vars);
}

template <typename T, typename... Vs>
auto evaluate(baseexpr<T>, const Map<Vs...>& t) {
  auto result = eval_eigen(T{}, t);
  return result;
}

template <typename T, typename... Vs>
auto evaluate_conditional(baseexpr<T>, const Map<Vs...>& t) {
  auto result = eval_eigen_conditional(T{}, t);
  return result;
}



} // end namespace aether

#endif
