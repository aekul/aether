#ifndef DYNAMIC_RANDOM_VAR_H
#define DYNAMIC_RANDOM_VAR_H

#include <vector>
#include <numeric>

#include "aether/fwd/Math.h"

namespace aether {

template <typename T>
struct pdf_value_pair {
  Real pdf;
  T value;
};

template <typename T>
auto make_pdf_value_pair(Real pdf, T&& value) {
  return pdf_value_pair<std::decay_t<T>>{pdf, std::forward<T>(value)};
}

template <typename T>
struct discrete_random_var_dynamic {
  using value_t = T;
  struct discrete_array_struct {
      T value;
      Real pdf;
      Real cdf;
  };

  discrete_random_var_dynamic(int N)
    : data(N)
  {
    for (int i = 0; i < N; i++) {
      data[i].value = i;
      data[i].pdf = Real(1.0) / N;
      data[i].cdf = Real(i + 1) / N;
    }
  }

  discrete_random_var_dynamic(const T *values, const Real *weights, const std::size_t N)
    : data(N)
  {
    if (N >= 1) {
      data[0].value = values[0];
      data[0].pdf = data[0].cdf = weights[0];
      for (size_t i = 1; i < N; i++) {
        data[i].value = values[i];
        data[i].pdf = weights[i];
        data[i].cdf = data[i - 1].cdf + weights[i];
      }

      Real invNorm = Real(1) / data[N - 1].cdf;
      for (size_t i = 0; i < N; i++) {
        data[i].pdf *= invNorm;
        data[i].cdf *= invNorm;
      }
    }
  }

  discrete_random_var_dynamic(const T *values, const std::size_t N) 
    : data(N)
  {
    for (int i = 0; i < N; i++) {
      data[i].value = values[i];
      data[i].pdf = Real(1.0) / N;
      data[i].cdf = Real(i + 1) / N;
    }
  }

  discrete_random_var_dynamic(const pdf_value_pair<T> *pairs, const std::size_t N)
    : data(N)
  {
    if (N >= 1) {
      data[0].value = pairs[0].value;
      data[0].pdf = data[0].cdf = pairs[0].pdf;
      for (size_t i = 1; i < N; i++) {
        data[i].value = pairs[i].value;
        data[i].pdf = pairs[i].pdf;
        data[i].cdf = data[i - 1].cdf + pairs[i].pdf;
      }
      Real invNorm = Real(1) / data[N - 1].cdf;
      for (size_t i = 0; i < N; i++) {
        data[i].pdf *= invNorm;
        data[i].cdf *= invNorm;
      }
    }
  }

  std::size_t Size() const {
    return data.size();
  }

  value_t Value(std::size_t i) const {
    return data[i].value;
  }

  value_t Sample(Real u) const {
    std::size_t N = data.size();

    for (std::size_t i = 0; i < N; ++i) {
      if (u < data[i].cdf) {
        return data[i].value;
      }
    }

    return data[N - 1].value;
  }

  Real Pdf(const value_t& value) const {
    std::size_t N = data.size();
    Real ret(0);
    for (std::size_t i = 0; i < N; ++i) {
      if (data[i].value == value) {
        ret += data[i].pdf;
      }
    }
    return ret;
  }

  std::vector<discrete_array_struct> data;
};


template <typename T>
constexpr auto discrete_dynamic(const std::vector<T>& values) {
  return discrete_random_var_dynamic<T>{values.data(), values.size()};
}

template <typename T>
constexpr auto discrete_dynamic(const T *values, const std::size_t N) {
  return discrete_random_var_dynamic<T>{values, N};
}

template <typename T>
constexpr auto discrete_dynamic(const std::vector<T>& values, const std::vector<Real>& weights) {
  return discrete_random_var_dynamic<T>{values.data(), weights.data(), values.size()};
}

template <typename T>
constexpr auto discrete_dynamic(const T *values, const Real *weights, const std::size_t N) {
  return discrete_random_var_dynamic<T>{values, weights, N};
}

inline auto discrete_dynamic(int N) {
  return discrete_random_var_dynamic<int>{N};
}

template <typename T>
struct discrete_random_var_2d {
  using value_t = T;
  template <typename C>
  discrete_random_var_2d(const C& grid) : dist(Init(grid)) {
  }

  template <typename C>
  auto Init(const C& grid) {
    auto num_rows = grid.size();

    std::vector<Real> rows(num_rows);
    std::vector<pdf_value_pair<discrete_random_var_dynamic<T>>> dists;

    Real total = Real(0);
    for (int i = 0; i < num_rows; ++i) {
      rows[i] = Sum(grid[i]);
      total += rows[i];
    }

    for (int i = 0; i < num_rows; ++i) {
      rows[i] /= total;
      dists.push_back(make_pdf_value_pair(rows[i], discrete_dynamic<T>(grid[i])));
    }

    return discrete_dynamic<discrete_random_var_dynamic<T>>(dists);
  }

  template <typename C>
  Real Sum(const C& c) {
    Real sum = Real(0);
    for (auto v : c) {
      sum += v;
    }
    return sum;
  }

  value_t Sample(Real u, Real v) const {
    return (dist.Sample(u)).Sample(v);
  }

  discrete_random_var_dynamic<discrete_random_var_dynamic<T>> dist;
};

template <typename A, typename B>
constexpr auto discrete_2d(A&& a, B&& b) {
  return discrete_random_var_2d<std::decay_t<A>>{std::forward<A>(a), std::forward<B>(b)};
}

template <typename T, typename A>
constexpr auto discrete_dynamic_2d(A&& a) {
  return discrete_random_var_2d<T>{std::forward<A>(a)};
}


template <typename A, typename B>
constexpr auto discrete(A&& a, B&& b) {
  return discrete_2d(discrete(std::forward<A>(a)), discrete(std::forward<B>(b)));
}


} // end namespace aether

#endif
