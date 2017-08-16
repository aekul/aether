#ifndef RANDOM_VAR_H
#define RANDOM_VAR_H

#include "fwd/RandomVar.h"
#include "Expr.h"
#include "Condition.h"
#include "Eval.h"
#include "Invert.h"
#include "Product.h"
#include "NamedParameter.h"
#include "Vector.h"
#include "Map.h"
#include "Distribution2D.h"
#include "typelist/Back.h"
#include "typelist/Prepend.h"
#include "typelist/Max.h"
#include "typelist/Min.h"
#include "typelist/RemoveBack.h"

#include <boost/optional.hpp>
#include <boost/hana.hpp>
#include <utility>
#include <cmath>

namespace aether {

template <typename T>
struct SampleCall;

template <typename T>
struct Function {
  template <typename... Args>
  auto operator()(Args&&... args) {
    return Derived().impl(std::forward<Args>(args)...);
  }

  template <typename... Args>
  auto operator()(Args&&... args) const {
    return Derived().impl(std::forward<Args>(args)...);
  }

  T& Derived() {
    return static_cast<T&>(*this);
  }

  const T& Derived() const {
    return static_cast<const T&>(*this);
  }
};


using namespace boost;

struct Argument {
  struct Record {
    uint8_t isRecord;
    uint8_t distID;
    uint8_t maxDimension;
    uint8_t dimension;
  };
  union {
    Real value;
    Record record;
  };
};

struct SourceDistributionHelper {
  void reset(const std::vector<const Distribution2D*> *dist2Ds) {
    distributions = dist2Ds;
    buffer.resize(distributions->size(), std::array<Real, 2>{{-1, -1}});
  }

  const std::vector<const Distribution2D*> *distributions = nullptr;
  std::vector<std::array<Real, 2>> buffer;
};

template <std::size_t I, typename T, typename Tuple>
struct index_of_type_impl;

template <std::size_t I, typename T, typename... Ts>
struct index_of_type_impl<I, T, TypeList<T, Ts...>> {
  static constexpr std::size_t value = I;
};

template <std::size_t I, typename T, typename R, typename... Ts>
struct index_of_type_impl<I, T, TypeList<R, Ts...>> {
  static constexpr std::size_t value = index_of_type_impl<I + 1, T, TypeList<Ts...>>::value;
};

template <typename T, typename... Ts>
struct index_of_type_impl<0, T, hana::tuple<Ts...>> {
  static constexpr std::size_t value = index_of_type_impl<0, T, TypeList<Ts...>>::value;
};

template <typename T, typename Tuple>
struct index_of_type {
  static constexpr std::size_t value = index_of_type_impl<0, T, Tuple>::value;
};

template <typename T>
struct scalar {
  using type = typename std::conditional<
    dimensions_of(T{}).IsScalar()
    , Real
    , std::array<Real, 3>
  >::type;
};

template <typename T>
using scalar_t = result_t<T>;


/*template <bool Computed, typename E, typename Map, typename Data = empty_tuple_t>
struct random_var;*/

template <typename C, typename... Ts>
constexpr int output_dimensions(vectorexpr<C, Ts...>) {
  return (output_dimensions(Ts{}) + ... + 0);
}

template <typename E>
constexpr int output_dimensions(baseexpr<E>) {
  return dimensions_of(E{}).Rows(); 
}

template <typename E, typename Map, typename Data>
struct random_var<false, E, Map, Data> {
  using expr_t = E;
  using map_t = Map;
  using self_t = random_var<false, expr_t, map_t, Data>;
  using result_t = scalar_t<expr_t>;
  using data_t = Data;

  constexpr E expr() const;
  
  constexpr static int N = vars(E{}).Size();
  constexpr static int M = dimensions_of(E{}).Rows();

  auto Value() const;

  bool Valid() const;
 
  auto Invert() const;

  template <typename... Args>
  constexpr auto Sample(Real arg, const Args&... args) const;

  template <typename P = void>
  constexpr auto Sample(const std::array<Real, N>& args) const;

  template <typename... Args>
  constexpr auto Sample(Argument &arg, Args&... args) const;

  constexpr auto Sample() const;

  template <typename T>
  auto bind_inverse_vars(const T& arg, std::index_sequence<0>) const;

  template <typename T, std::size_t... I>
  auto bind_inverse_vars(const T& arg, std::index_sequence<I...>) const;

  template <typename... Inverted, typename VarsAndParams>
  auto get_uv_impl(TypeList<Inverted...>, const VarsAndParams& vars_and_params) const;

  template <typename... Ts, typename T>
  auto get_uv(TypeList<Ts...>, const T& arg) const;

  template <typename T>
  auto get_uv(const T& arg) const;

  template <std::size_t... Is>
  auto to_array_impl(const std::array<Conditional<Real>, N>& values, std::index_sequence<Is...>) const;

  auto to_array(const std::array<Conditional<Real>, N>& values) const;

  bool all_valid(const std::array<Conditional<Real>, N>& values) const;

  bool all_refs_valid(const std::array<Conditional<Real>, N>& values) const;

  auto SymbolicPdf() const; 

  template <typename T>
  Real PdfImpl(const T& t) const;

  template <typename T>
  Real PdfImpl(const T& t, SourceDistributionHelper &source_distribution_helper) const;

  template <typename T>
  Real Pdf(const T& t) const;

  template <typename T>
  Real Pdf(const T& t, SourceDistributionHelper &source_distribution_helper) const;

  Real Pdf(const Vector3& v) const; 

  Real Pdf(const Vector3& v, SourceDistributionHelper &source_distribution_helper) const; 

  template <typename Name>
  constexpr const auto& Get(named_parameter<Name>) const;

  template <typename Name>
  constexpr const auto& operator[](named_parameter<Name>) const;

  template <typename Name, typename T, EnableIf<has_name<Name, Data>::value> = 0>
  constexpr const auto& GetOr(named_parameter<Name>, T) const;

  template <typename Name, typename T, EnableIf<!has_name<Name, Data>::value> = 0>
  constexpr auto GetOr(named_parameter<Name>, T) const;

  template <typename Name>
  constexpr bool Has(named_parameter<Name>) const;

  map_t values;
  bool valid;
  Data data;
};

template <typename E, typename Map, typename Data>
constexpr E random_var<false, E, Map, Data>::expr() const {
  return {};
}

template <typename E, typename Map, typename Data>
auto random_var<false, E, Map, Data>::Value() const {
  return Sample().Value();
}

template <typename E, typename Map, typename Data>
bool random_var<false, E, Map, Data>::Valid() const {
  return valid;
}

template <typename E, typename Map, typename Data>
auto random_var<false, E, Map, Data>::Invert() const {
  return invert(E{});
}

template <typename E, typename Map, typename Data>
template <typename T>
auto random_var<false, E, Map, Data>::bind_inverse_vars(const T& arg, std::index_sequence<0>) const {
  return make_map(make_type_set(inverse_variable<0>{}), arg);
}

template <typename E, typename Map, typename Data>
template <typename T, std::size_t... I>
auto random_var<false, E, Map, Data>::bind_inverse_vars(const T& arg, std::index_sequence<I...>) const {
  return make_map(make_type_set(inverse_variable<I>{}...), arg[I]...);
}

template <typename E, typename Map, typename Data>
template <typename... Inverted, typename VarsAndParams>
auto random_var<false, E, Map, Data>::get_uv_impl(TypeList<Inverted...>, const VarsAndParams& vars_and_params) const {
  return std::array<Conditional<Real>, N>{{evaluate_conditional(Inverted{}, vars_and_params)...}};
}

template <typename E, typename Map, typename Data>
template <typename... Ts, typename T>
auto random_var<false, E, Map, Data>::get_uv(TypeList<Ts...>, const T& arg) const {
  auto vars_and_params = merge(
    values
    , bind_inverse_vars(arg, std::make_index_sequence<output_dimensions(E{})>{})
  );

  return std::array<std::array<Conditional<Real>, N>, sizeof...(Ts)>{{get_uv_impl(Ts{}.Values(), vars_and_params)...}};
}

template <typename E, typename Map, typename Data>
template <typename T>
auto random_var<false, E, Map, Data>::get_uv(const T& arg) const {
  constexpr auto inverted = invert(expr_t{});
  return get_uv(inverted, arg);
}

template <typename E, typename Map, typename Data>
template <std::size_t... Is>
auto random_var<false, E, Map, Data>::to_array_impl(const std::array<Conditional<Real>, N>& values, std::index_sequence<Is...>) const {
  return std::array<Real, N>{{values[Is].Value()...}};
}

template <typename E, typename Map, typename Data>
auto random_var<false, E, Map, Data>::to_array(const std::array<Conditional<Real>, N>& values) const {
  return to_array_impl(values, std::make_index_sequence<N>{});
}

template <typename E, typename Map, typename Data>
bool random_var<false, E, Map, Data>::all_valid(const std::array<Conditional<Real>, N>& values) const {
  return std::all_of(values.cbegin(), values.cend(), [](const Conditional<Real>& v) {
    return bool(v) && v.Value() > 0.0f && v.Value() <= 1.0f;
  });
}

template <typename E, typename Map, typename Data>
bool random_var<false, E, Map, Data>::all_refs_valid(const std::array<Conditional<Real>, N>& values) const {
  return std::all_of(values.cbegin(), values.cend(), [](const Conditional<Real>& v) {
    return bool(v);
  });
}

template <typename E, typename Map, typename Data>
template <typename T>
Real random_var<false, E, Map, Data>::PdfImpl(const T& t) const { 
  constexpr auto var_list = vars(E{});
  auto uv_list = get_uv(t);

  Real result = 0;
  for (auto uv : uv_list) {
    if (!all_valid(uv)) {
      continue;
    }
    
    map_t updated_values = values;
    set_all(var_list, to_array(uv), updated_values);
    result += determinant(E{}, updated_values);
  }

  return result;
}

template <typename Key, typename... Ks, EnableIf<!contains(Key{}, TypeSet<Ks...>{})> = 0>
constexpr void fill_src_dist_buffer_impl(Key, const Map<Ks...>& values,
                                        const Map<Ks...>& updated_values, std::vector<std::array<Real, 2>> &src_dist_buffer) {
}

template <typename Key, typename... Ks, EnableIf<contains(Key{}, TypeSet<Ks...>{})> = 0>
constexpr void fill_src_dist_buffer_impl(Key, const Map<Ks...>& values,
                                        const Map<Ks...>& updated_values, std::vector<std::array<Real, 2>> &src_dist_buffer) {
  static_assert(contains(Key{}, TypeSet<Ks...>{}), "Key not found in map.");
  constexpr auto index = index_of(Key{}, TypeSet<Ks...>{});
  if (values.valid[index] && updated_values.valid[index]) {
    if (values.values[index] != 0) {
      Argument argument;
      argument.value = values.values[index];
      if (argument.record.distID < src_dist_buffer.size()) {
        src_dist_buffer[argument.record.distID][argument.record.dimension] = updated_values.values[index];
      }
    }
  }
};

template <typename... Keys, typename... Ks>
void fill_src_dist_buffer(TypeSet<Keys...> keys, const Map<Ks...>& values,
                         const Map<Ks...>& updated_values, std::vector<std::array<Real, 2>> &src_dist_buffer) {
  using consume = int[];
  (void)consume{1, (fill_src_dist_buffer_impl(Keys{}, values, updated_values, src_dist_buffer), 1)...};
}

template <typename E, typename Map, typename Data>
template <typename T>
Real random_var<false, E, Map, Data>::PdfImpl(const T& t,
    SourceDistributionHelper &source_distribution_helper) const { 
  constexpr auto var_list = vars(E{});
  auto uv_list = get_uv(t);
  std::vector<std::array<Real, 2>>& source_distribution_buffer = source_distribution_helper.buffer;
  const std::vector<const Distribution2D*> *source_distributions = source_distribution_helper.distributions;

  Real result = 0;
  for (auto uv : uv_list) {
    if (!all_valid(uv)) {
      continue;
    }
    
    map_t updated_values = values;
    set_all(var_list, to_array(uv), updated_values);
    fill_src_dist_buffer(var_list, values, updated_values, source_distribution_buffer);
    Real source_pdf = Real(1);
    for (size_t i = 0; i < source_distribution_buffer.size(); i++) {
      source_pdf *= source_distributions->at(i)->Pdf(source_distribution_buffer[i]);
    }

    result += source_pdf * determinant(E{}, updated_values);
  }

  return result;
}

template <typename E, typename Map, typename Data>
template <typename T>
Real random_var<false, E, Map, Data>::Pdf(const T& t) const {
  return p(*this, E{}, values, t);
}

template <typename E, typename Map, typename Data>
Real random_var<false, E, Map, Data>::Pdf(const Vector3& v) const { 
  return p(*this, E{}, values, v);
}

template <typename E, typename Map, typename Data>
template <typename T>
Real random_var<false, E, Map, Data>::Pdf(const T& t, SourceDistributionHelper &source_distribution_helper) const { 
  return p(*this, E{}, values, t, source_distribution_helper);
}

template <typename E, typename Map, typename Data>
Real random_var<false, E, Map, Data>::Pdf(const Vector3& v, SourceDistributionHelper &source_distribution_helper) const { 
  return p(*this, E{}, values, v, source_distribution_helper);
}

template <typename E, typename Map, typename Data>
struct random_var<true, E, Map, Data> {
  using expr_t = E;
  using map_t = Map;
  using self_t = random_var<true, expr_t, map_t, Data>;
  using result_t = scalar_t<expr_t>;
  using data_t = Data;

  constexpr E expr() const;
  
  constexpr static int N = vars(E{}).Size();
  constexpr static int M = dimensions_of(E{}).Rows();

  bool Valid() const;
 
  auto Invert() const;

  auto Value() const;

  template <typename T>
  auto bind_inverse_vars(const T& arg, std::index_sequence<0>) const;

  template <typename T, std::size_t... I>
  auto bind_inverse_vars(const T& arg, std::index_sequence<I...>) const;

  template <typename... Inverted, typename VarsAndParams>
  auto get_uv_impl(TypeList<Inverted...>, const VarsAndParams& vars_and_params) const;

  template <typename... Ts, typename T>
  auto get_uv(TypeList<Ts...>, const T& arg) const;

  template <typename T>
  auto get_uv(const T& arg) const;

  template <std::size_t... Is>
  auto to_array_impl(const std::array<Conditional<Real>, N>& values, std::index_sequence<Is...>) const;

  auto to_array(const std::array<Conditional<Real>, N>& values) const;

  bool all_valid(const std::array<Conditional<Real>, N>& values) const;

  bool all_refs_valid(const std::array<Conditional<Real>, N>& values) const;

  auto SymbolicPdf() const;

  template <typename T>
  Real PdfImpl(const T& t) const; 

  template <typename T>
  Real PdfImpl(const T& t, SourceDistributionHelper &source_distribution_helper) const; 

  template <typename T>
  Real Pdf(const T& t) const; 

  template <typename T>
  Real Pdf(const T& t, SourceDistributionHelper &source_distribution_helper) const; 

  Real Pdf(const Vector3& v) const; 

  Real Pdf(const Vector3& v, SourceDistributionHelper &source_distribution_helper) const; 

  template <typename T>
  auto g(const T& t) const; 

  Real Pdf() const; 

  boost::optional<Real> OptionalPdf() const; 

  template <typename Name>
  constexpr const auto& Get(named_parameter<Name>) const;

  template <typename Name, typename T, EnableIf<has_name<Name, Data>::value> = 0>
  constexpr const auto& GetOr(named_parameter<Name>, T) const;

  template <typename Name, typename T, EnableIf<!has_name<Name, Data>::value> = 0>
  constexpr auto GetOr(named_parameter<Name>, T) const;

  template <typename Name>
  constexpr const auto& operator[](named_parameter<Name>) const;

  template <typename Name>
  constexpr bool Has(named_parameter<Name>) const;

  template <int ID>
  constexpr auto Cast() const;

  constexpr auto Cast() const;
  
  bool valid;
  map_t values;
  Data data;
  result_t result;
};



template <typename E, typename Map, typename Data>
constexpr E random_var<true, E, Map, Data>::expr() const {
  return {};
}

template <typename E, typename Map, typename Data>
bool random_var<true, E, Map, Data>::Valid() const {
  return valid;
}

template <typename E, typename Map, typename Data>
auto random_var<true, E, Map, Data>::Invert() const {
  return invert(E{});
}

template <typename E, typename Map, typename Data>
auto random_var<true, E, Map, Data>::Value() const {
  return result;
}

template <typename E, typename Map, typename Data>
template <typename T>
auto random_var<true, E, Map, Data>::bind_inverse_vars(const T& arg, std::index_sequence<0>) const {
  return make_map(make_type_set(inverse_variable<0>{}), arg);
}

template <typename E, typename Map, typename Data>
template <typename T, std::size_t... I>
auto random_var<true, E, Map, Data>::bind_inverse_vars(const T& arg, std::index_sequence<I...>) const {
  return make_map(make_type_set(inverse_variable<I>{}...), arg[I]...);
}

template <typename E, typename Map, typename Data>
template <typename... Inverted, typename VarsAndParams>
auto random_var<true, E, Map, Data>::get_uv_impl(TypeList<Inverted...>, const VarsAndParams& vars_and_params) const {
  return std::array<Conditional<Real>, N>{{evaluate_conditional(Inverted{}, vars_and_params)...}};
}

template <typename E, typename Map, typename Data>
template <typename... Ts, typename T>
auto random_var<true, E, Map, Data>::get_uv(TypeList<Ts...>, const T& arg) const {
  auto vars_and_params = merge(
    values
    , bind_inverse_vars(arg, std::make_index_sequence<output_dimensions(E{})>{})
  );

  return std::array<std::array<Conditional<Real>, N>, sizeof...(Ts)>{{get_uv_impl(Ts{}.Values(), vars_and_params)...}};
}

template <typename E, typename Map, typename Data>
template <typename T>
auto random_var<true, E, Map, Data>::get_uv(const T& arg) const {
  constexpr auto inverted = invert(expr_t{});
  return get_uv(inverted, arg);
}

template <typename E, typename Map, typename Data>
template <std::size_t... Is>
auto random_var<true, E, Map, Data>::to_array_impl(const std::array<Conditional<Real>, N>& values, std::index_sequence<Is...>) const {
  return std::array<Real, N>{{values[Is].Value()...}};
}

template <typename E, typename Map, typename Data>
auto random_var<true, E, Map, Data>::to_array(const std::array<Conditional<Real>, N>& values) const {
  return to_array_impl(values, std::make_index_sequence<N>{});
}

template <typename E, typename Map, typename Data>
bool random_var<true, E, Map, Data>::all_valid(const std::array<Conditional<Real>, N>& values) const {
  return std::all_of(values.cbegin(), values.cend(), [](const Conditional<Real>& v) {
    return bool(v) && v.Value() > 0.0f && v.Value() <= 1.0f;
  });
}

template <typename E, typename Map, typename Data>
bool random_var<true, E, Map, Data>::all_refs_valid(const std::array<Conditional<Real>, N>& values) const {
  return std::all_of(values.cbegin(), values.cend(), [](const Conditional<Real>& v) {
    return bool(v);
  });
}

template <typename E, typename Map, typename Data>
template <typename T>
Real random_var<true, E, Map, Data>::PdfImpl(const T& t) const { 
  constexpr auto var_list = vars(E{});
  auto uv_list = get_uv(t);

  Real result = 0;
  for (auto uv : uv_list) {
    if (!all_valid(uv)) {
      continue;
    }

    map_t updated_values = values;
    set_all(var_list, to_array(uv), updated_values);

    auto p = determinant(E{}, updated_values);
    if (std::isnan(p)) {
      continue;
    }

    result += p;
  }

  return result;
}

template <typename E, typename Map, typename Data>
template <typename T>
Real random_var<true, E, Map, Data>::PdfImpl(const T& t,
    SourceDistributionHelper &source_distribution_helper) const { 
  constexpr auto var_list = vars(E{});
  auto uv_list = get_uv(t);
  std::vector<std::array<Real, 2>>& source_distribution_buffer = source_distribution_helper.buffer;
  const std::vector<const Distribution2D*>* source_distributions = source_distribution_helper.distributions;

  Real result = 0;
  for (auto uv : uv_list) {
    if (!all_valid(uv)) {
      continue;
    }

    map_t updated_values = values;
    set_all(var_list, to_array(uv), updated_values);
    fill_src_dist_buffer(var_list, values, updated_values, source_distribution_buffer);
    Real source_pdf = Real(1);
    for (size_t i = 0; i < source_distribution_buffer.size(); i++) {
      source_pdf *= source_distributions->at(i)->Pdf(source_distribution_buffer[i]);
    }

    auto p = determinant(E{}, updated_values);
    if (std::isnan(p)) {
      continue;
    }

    result += source_pdf * p;
  }

  return result;
}

template <typename E, typename Map, typename Data>
template <typename T>
Real random_var<true, E, Map, Data>::Pdf(const T& t) const { 
  return p(*this, E{}, values, t);
}

template <typename E, typename Map, typename Data>
Real random_var<true, E, Map, Data>::Pdf(const Vector3& v) const { 
  return p(*this, E{}, values, v);
}

template <typename E, typename Map, typename Data>
template <typename T>
Real random_var<true, E, Map, Data>::Pdf(const T& t, SourceDistributionHelper &source_distribution_helper) const { 
  return p(*this, E{}, values, t, source_distribution_helper);
}

template <typename E, typename Map, typename Data>
Real random_var<true, E, Map, Data>::Pdf(const Vector3& v, SourceDistributionHelper &source_distribution_helper) const { 
  return p(*this, E{}, values, v, source_distribution_helper);
}

template <typename E, typename Map, typename Data>
template <typename T>
auto random_var<true, E, Map, Data>::g(const T& t) const { 
  return get_uv(t);
}

template <typename E, typename Map, typename Data>
Real random_var<true, E, Map, Data>::Pdf() const { 
  return p(*this, E{}, values);
}

template <typename E, typename Map, typename Data>
boost::optional<Real> random_var<true, E, Map, Data>::OptionalPdf() const { 
  return boost::optional<Real>(valid, valid ? Pdf() : 0);
}

template <typename... Ts, bool Computed, typename E, typename M, typename Data>
auto get_data_as_map_impl(TypeSet<Ts...>, const random_var<Computed, E, M, Data>& rv) {
  return merge_all(named_constant<0>(Ts{}, rv.Get(Ts{})).values...);
}

template <typename... Ts, bool Computed, typename E, typename M, typename Data>
auto get_data_as_map(TypeSet<Ts...> keys, const random_var<Computed, E, M, Data>& rv) {
  return get_data_as_map_impl(MergeAll(make_type_set(get_name(Ts{}))...), rv);
}

//template <typename... Ts, typename Sample>
//auto get_data_as_map_impl(TypeSet<Ts...> keys, const std::vector<Sample>& samples) {
  ////Ty<decltype(keys)> s{};
  //return get_data_as_map_impl(MergeAll(make_type_set(get_name(Ts{}))...), rv);
//}

template <typename T, int ID, int SeqIndex, typename Sample>
auto get_data_as_map_impl(named_param<T, ID, 1, 0, SeqIndex>, const std::vector<Sample>& samples) {
  return samples[ID].Get(T{});
}

template <typename T, int ID, std::size_t N, std::size_t I, int SeqIndex, typename Sample>
auto get_data_as_map_impl(named_param<T, ID, N, I, SeqIndex>, const std::vector<Sample>& samples) {
  return samples[ID].Get(T{})[I];
}

template <typename... Ts, typename Sample>
auto get_data_as_map(TypeSet<Ts...> keys, const std::vector<Sample>& samples) {
  return make_map(keys, get_data_as_map_impl(Ts{}, samples)...);

    //named_constant<get_id(Ts{})>(get_name(Ts{}), samples[get_id(Ts{})].Get(get_name(Ts{})))
  //Ty<decltype(keys)> s{};
  //return get_data_as_map_impl(MergeAll(make_type_set(get_name(Ts{}))...), rv);
}

template <typename T>
struct get_data_keys;

template <bool Computed, typename E, typename M, typename... Ds>
struct get_data_keys<random_var<Computed, E, M, hana::tuple<Ds...>>> {
  using type = TypeSet<named_parameter<typename Ds::Name>...>;
};

template <typename T>
using get_data_keys_t = typename get_data_keys<T>::type;


template <bool Computed, typename E, typename M, typename Data>
constexpr auto named_params(const random_var<Computed, E, M, Data>& rv) {
  return named_params(E{});
}

template <bool Computed, typename E, typename M, typename Data>
auto get_value(const random_var<Computed, E, M, Data>& rv) {
  return rv;
}

template <std::size_t N, bool Computed, typename E, typename Map, typename Data>
auto get_sample_value(const random_var<Computed, E, Map, Data>& rv) {
  return rv.Value();
}

template <typename T>
auto get_value(const boost::optional<T>& rv) {
  return rv.get();
}

template <bool Computed, typename E, typename M, typename Data>
bool is_valid(const random_var<Computed, E, M, Data>& rv) {
  return rv.valid;
}

template <typename T>
bool is_valid(const boost::optional<T>& rv) {
  return bool(rv);
}

template <typename T>
constexpr bool is_refexpr(baseexpr<T>) {
  constexpr auto v = vars(T{});
  return any(is_refvar, make_list(v));
}

template <bool Computed, typename E, typename M, typename Data, typename T, typename... Ts, typename R, EnableIf<(vars(E{}).Size() > 0 && is_refexpr(E{}))> = 0>
Real p(const random_var<Computed, E, M, Data>& rv, baseexpr<T>, const Map<Ts...>& map, const R& result) {
  return rv.PdfRefImpl(result);
}

template <typename Cond, bool Computed, typename T, typename M, typename Data, typename... Ts, typename R>
Real p_ref_impl_helper(baseexpr<Cond>, const random_var<Computed, T, M, Data>& rv, const Map<Ts...>& map, const R& result) {
  if (!evaluate(Cond{}, map)) {
    return 0;
  }

  return p(rv, T{}, map, result);
}

template <typename... Conds, bool Computed, typename... Es, typename M, typename Data, typename T, typename... Ts, typename R>
Real p_ref_impl(TypeList<Conds...>, const random_var<Computed, branchexpr<Es...>, M, Data>& rv, baseexpr<T>, const Map<Ts...>& map, const R& result) {
  return (p_ref_impl_helper(Conds{}, make_random_var(rv.valid, get_value(Es{}), rv.values, rv.data), map, result) + ... + 0);
}

template <bool Computed, typename... Es, typename M, typename Data, typename T, typename... Ts, typename R, EnableIf<(vars(branchexpr<Es...>{}).Size() > 0 && is_refexpr(branchexpr<Es...>{}))> = 0>
Real p(const random_var<Computed, branchexpr<Es...>, M, Data>& rv, baseexpr<T>, const Map<Ts...>& map, const R& result) {
  constexpr auto conds = and_conditions(List(get_condition(Es{})...));
  return p_ref_impl(conds, rv, T{}, map, result);
}

template <bool Computed, typename E, typename M, typename Data, typename T, typename... Ts, typename R, EnableIf<(vars(E{}).Size() > 0 && !is_refexpr(E{}))> = 0>
Real p(const random_var<Computed, E, M, Data>& rv, baseexpr<T>, const Map<Ts...>& map, const R& result) {
  return rv.PdfImpl(result);
}

template <bool Computed, typename E, typename M, typename Data, typename T, typename... Ts, typename R, EnableIf<(vars(E{}).Size() > 0 && !is_refexpr(E{}))> = 0>
Real p(const random_var<Computed, E, M, Data>& rv, baseexpr<T>, const Map<Ts...>& map, const R& result, SourceDistributionHelper &source_distribution_helper) {
  return rv.PdfImpl(result, source_distribution_helper);
}

template <bool Computed, typename E, typename M, typename Data, typename T, typename... Ts, typename R, EnableIf<(vars(E{}).Size() == 0)> = 0>
Real p(const random_var<Computed, E, M, Data>& rv, baseexpr<T>, const Map<Ts...>&, const R& result) {
  return 1;
}

template <bool Computed, typename E, typename M, typename Data, typename T, typename... Ts, typename R, EnableIf<(vars(E{}).Size() == 0)> = 0>
Real p(const random_var<Computed, E, M, Data>& rv, baseexpr<T>, const Map<Ts...>&, const R& result, SourceDistributionHelper &source_distribution_helper) {
  return 1;
}

template <bool Computed, typename E, typename M, typename Data, typename T, typename... Ts, EnableIf<(vars(E{}).Size() > 0 && is_refexpr(E{}))> = 0>
Real p(const random_var<Computed, E, M, Data>& rv, baseexpr<T>, const Map<Ts...>& map) {
  return rv.PdfRefImpl(rv.result);
}

template <bool Computed, typename E, typename M, typename Data, typename T, typename... Ts, EnableIf<(vars(E{}).Size() > 0 && !is_refexpr(E{}))> = 0>
Real p(const random_var<Computed, E, M, Data>& rv, baseexpr<T>, const Map<Ts...>& map) {
  return rv.PdfImpl(rv.result);
}

template <bool Computed, typename... Es, typename M, typename Data, typename T, typename... Ts, EnableIf<(vars(balanceexpr<branchexpr<Es...>>{}).Size() > 0)> = 0>
Real p(const random_var<Computed, balanceexpr<branchexpr<Es...>>, M, Data>& rv, baseexpr<T>, const Map<Ts...>& map) {
  return (make_random_var(get_value(Es{}), rv.values).Pdf(rv.Value()) + ...) / Real(sizeof...(Es));
}

template <bool Computed, typename E, typename M, typename Data, typename T, typename... Ts, EnableIf<(vars(T{}).Size() == 0)> = 0>
Real p(const random_var<Computed, E, M, Data>& rv, baseexpr<T>, const Map<Ts...>&) {
  return Real(1);
}

template <typename A>
constexpr auto merge_data(const A& a) {
  return a;
}

template <typename... As, typename... Bs>
constexpr auto merge_data_impl(_size_t<sizeof...(Bs)>, const hana::tuple<As...>& a, const hana::tuple<Bs...>& b) {
  return a;
}


template <std::size_t I, typename... As, typename... Bs, EnableIf<!contains(at<I>(TypeList<typename Bs::Name...>{}), TypeList<typename As::Name...>{})> = 0>
constexpr auto merge_data_impl(_size_t<I>, const hana::tuple<As...>& a, const hana::tuple<Bs...>& b) {
  return merge_data_dispatch(_size_t<I + 1>{}, hana::append(a, hana::at_c<I>(b)), b);
}

template <std::size_t I, typename... As, typename... Bs, EnableIf<contains(at<I>(TypeList<typename Bs::Name...>{}), TypeList<typename As::Name...>{})> = 0>
constexpr auto merge_data_impl(_size_t<I>, const hana::tuple<As...>& a, const hana::tuple<Bs...>& b) {
  return merge_data_dispatch(_size_t<I + 1>{}, a, b);
}

template <std::size_t I, typename... As, typename... Bs, EnableIf<(I >= sizeof...(Bs))> = 0>
constexpr auto merge_data_dispatch(_size_t<I>, const hana::tuple<As...>& a, const hana::tuple<Bs...>& b) {
  return a;
}

template <std::size_t I, typename... As, typename... Bs, EnableIf<(I < sizeof...(Bs))> = 0>
constexpr auto merge_data_dispatch(_size_t<I>, const hana::tuple<As...>& a, const hana::tuple<Bs...>& b) {
  return merge_data_impl(_size_t<I>{}, a, b);
}

template <typename... As, typename... Bs, typename... Ts>
constexpr auto merge_data(const hana::tuple<As...>& a, const hana::tuple<Bs...>& b, const Ts&... ts) {
  return merge_data(merge_data_dispatch(_size_t<0>{}, a, b), ts...);
}

template <typename E, typename Tuple, typename Data>
template <typename Name>
constexpr const auto& random_var<false, E, Tuple, Data>::Get(named_parameter<Name>) const {
  return hana::at_c<index_of_name<Name, Data>::value>(data).Value();
}

template <typename E, typename Tuple, typename Data>
template <typename Name, typename T, EnableIf<has_name<Name, Data>::value>>
constexpr const auto& random_var<false, E, Tuple, Data>::GetOr(named_parameter<Name>, T) const {
  return Get(Name{});
}

template <typename E, typename Tuple, typename Data>
template <typename Name, typename T, EnableIf<!has_name<Name, Data>::value>>
constexpr auto random_var<false, E, Tuple, Data>::GetOr(named_parameter<Name>, T value) const {
  return value;
}

template <typename E, typename Tuple, typename Data>
template <typename Name>
constexpr const auto& random_var<false, E, Tuple, Data>::operator[](named_parameter<Name>) const {
  return hana::at_c<index_of_name<Name, Data>::value>(data).Value();
}

template <typename E, typename Tuple, typename Data>
template <typename Name>
constexpr bool random_var<false, E, Tuple, Data>::Has(named_parameter<Name>) const {
  return has_name<Name, Data>::value;
}

template <typename E, typename Tuple, typename Data>
template <typename Name>
constexpr const auto& random_var<true, E, Tuple, Data>::Get(named_parameter<Name>) const {
  return hana::at_c<index_of_name<Name, Data>::value>(data).Value();
}

template <typename E, typename Tuple, typename Data>
template <typename Name, typename T, EnableIf<has_name<Name, Data>::value>>
constexpr const auto& random_var<true, E, Tuple, Data>::GetOr(named_parameter<Name>, T) const {
  return Get(Name{});
}

template <typename E, typename Tuple, typename Data>
template <typename Name, typename T, EnableIf<!has_name<Name, Data>::value>>
constexpr auto random_var<true, E, Tuple, Data>::GetOr(named_parameter<Name>, T value) const {
  return value;
}

template <typename E, typename Tuple, typename Data>
template <typename Name>
constexpr const auto& random_var<true, E, Tuple, Data>::operator[](named_parameter<Name>) const {
  return hana::at_c<index_of_name<Name, Data>::value>(data).Value();
}

template <typename E, typename Tuple, typename Data>
template <typename Name>
constexpr bool random_var<true, E, Tuple, Data>::Has(named_parameter<Name>) const {
  return has_name<Name, Data>::value;
}

template <typename T, typename... Ts, typename Data = empty_tuple_t>
constexpr auto make_random_var_impl(bool valid, baseexpr<T>, const Map<Ts...>& values, const Data& data = {}) {
  return random_var<false, T, Map<Ts...>, Data>{values, true, data};
}

template <typename T, typename... Ts>
constexpr auto make_random_var(baseexpr<T>, const Map<Ts...>& values) {
  return make_random_var_impl(true, T{}, filter_missing_keys(T{}, values));
}

template <typename T, typename... Ts, typename Data>
constexpr auto make_random_var_with_data(baseexpr<T>, const Map<Ts...>& values, const Data& data) {
  return make_random_var_impl(true, T{}, filter_missing_keys(T{}, values), data);
}

template <typename T, typename... Ts, typename Data>
constexpr auto make_random_var(bool valid, baseexpr<T>, const Map<Ts...>& values, const Data& data) {
  return make_random_var_impl(valid, T{}, filter_missing_keys(T{}, values), data);
}

template <typename E, typename Tuple, typename Data>
auto random_var<true, E, Tuple, Data>::SymbolicPdf() const {
  return make_random_var(valid, det(E{}), values, empty_tuple_t{});
}


template <typename A, typename TupleA, typename B, typename TupleB>
constexpr auto copy_computed_value_into_random_var(const random_var<false, A, TupleA>&, const random_var<true, B, TupleB>& from) {
  return random_var<true, A, TupleB>{true, from.values, {}, from.value}; 
}

template <typename T, typename MapA, typename DataA>
constexpr auto make_computed_random_var_without_value(bool valid, const random_var<false, T, MapA, DataA>& rv) {
  return random_var<true, T, MapA, DataA>{valid, {}, rv.data, {}};
}


template <typename T, typename... Ts, typename Data = empty_tuple_t>
constexpr auto make_computed_random_var(bool valid, baseexpr<T>, const Map<Ts...>& values, const Data& data = empty_tuple_t{}) {
  return random_var<true, T, Map<Ts...>, Data>{valid, values, data, evaluate(T{}, values)};
}

template <typename T, typename... Ts, typename Data>
constexpr auto make_computed_random_var(baseexpr<T>, const Map<Ts...>& values, const Data& data) {
  return make_computed_random_var(true, T{}, values, data);
}

template <typename T, typename... Ts>
constexpr auto make_computed_random_var(baseexpr<T>, const Map<Ts...>& values) {
  return make_computed_random_var(true, T{}, values);
}

template <typename T, typename... Ts>
constexpr auto make_random_var_with_data(baseexpr<T>, const Ts&... data) {
  return random_var<false, T, Map<>, hana::tuple<Ts...>>{{}, true, hana::make_tuple(data...)};
}

template <typename T, typename Map, typename... Ts>
constexpr auto make_random_var_with_data(const random_var<false, T, Map>& rv, const Ts&... data) {
  return random_var<false, T, Map, hana::tuple<Ts...>>{rv.values, true, hana::make_tuple(data...)};
}

template <typename T>
constexpr auto make_random_var(baseexpr<T>) {
  return random_var<false, T, Map<>>{{}, true, {}};
}

template <typename... Ts>
constexpr auto make_random_vector(Ts&&... ts) {
  return make_random_var((ts.valid && ... && true), make_vector_expr(ts.expr()...), merge_all(ts.values...), merge_data(ts.data...));
}

template <typename... Ts>
constexpr auto make_random_matrix(Ts&&... ts) {
  return make_random_var(make_matrix_expr(ts.expr()...), merge_all(ts.values...));
}

template <typename... Ts>
constexpr auto make_random_rotation_matrix(Ts&&... ts) {
  return make_random_var(make_rotation_matrix_expr(ts.expr()...), merge_all(ts.values...));
}

template <bool ComputedA, typename A, typename MapA, typename DataA, bool ComputedB, typename B, typename MapB, typename DataB>
constexpr auto dot(const random_var<ComputedA, A, MapA, DataA>& a, const random_var<ComputedB, B, MapB, DataB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = dot(A{}, updated_expr_b);
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool ComputedA, typename A, typename MapA, bool ComputedB, typename B, typename MapB>
constexpr auto cross(const random_var<ComputedA, A, MapA>& a, const random_var<ComputedB, B, MapB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = cross(A{}, updated_expr_b);
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool ComputedA, typename A, typename MapA, bool ComputedB, typename B, typename MapB>
constexpr auto operator/(const random_var<ComputedA, A, MapA>& a, const random_var<ComputedB, B, MapB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} / updated_expr_b;
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool Computed, typename A, typename MapA, typename B>
constexpr auto operator/(const random_var<Computed, A, MapA>& a, baseexpr<B>) {
  return a / make_random_var(B{});
}

template <bool Computed, typename A, typename B, typename MapB>
constexpr auto operator/(baseexpr<A>, const random_var<Computed, B, MapB>& b) {
  return make_random_var(A{}) / b;
}

template <bool ComputedA, typename A, typename MapA, bool ComputedB, typename B, typename MapB>
constexpr auto pow(const random_var<ComputedA, A, MapA>& a, const random_var<ComputedB, B, MapB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = pow(A{}, updated_expr_b);
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool Computed, typename E, typename Map>
constexpr auto rcp(const random_var<Computed, E, Map>& rv) {
  return make_random_var(rcp(E{}), rv.values);
}

template <bool Computed, typename E, typename Map>
constexpr auto sq(const random_var<Computed, E, Map>& rv) {
  return make_random_var(sq(E{}), rv.values);
}

template <bool Computed, typename E, typename Map>
constexpr auto sqrt(const random_var<Computed, E, Map>& rv) {
  return make_random_var(sqrt(E{}), rv.values);
}

template <bool ComputedA, typename A, typename MapA, typename DataA, bool ComputedB, typename B, typename MapB, typename DataB>
constexpr auto operator-(const random_var<ComputedA, A, MapA, DataA>& a, const random_var<ComputedB, B, MapB, DataB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} - updated_expr_b;
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool Computed, typename A, typename B, typename MapB>
constexpr auto operator-(baseexpr<A>, const random_var<Computed, B, MapB>& b) {
  return make_random_var(A{}) - b;
}

template <bool ComputedA, typename A, typename MapA, typename DataA, bool ComputedB, typename B, typename MapB, typename DataB>
constexpr auto operator*(const random_var<ComputedA, A, MapA, DataA>& a, const random_var<ComputedB, B, MapB, DataB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} * updated_expr_b;
  return make_random_var(a.valid && b.valid, E, merge(a.values, updated_map_b), merge_data(a.data, b.data));
}

template <bool Computed, typename A, typename MapA, typename DataA, typename B>
constexpr auto operator*(const random_var<Computed, A, MapA, DataA>& a, baseexpr<B>) {
  return a * make_random_var(B{});
}

template <bool Computed, typename A, typename B, typename MapB, typename DataB>
constexpr auto operator*(baseexpr<A>, const random_var<Computed, B, MapB, DataB>& b) {
  return make_random_var(A{}) * b;
}

template <bool ComputedA, typename A, typename MapA, typename DataA, bool ComputedB, typename B, typename MapB, typename DataB>
constexpr auto operator+(const random_var<ComputedA, A, MapA, DataA>& a, const random_var<ComputedB, B, MapB, DataB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} + updated_expr_b;
  return make_random_var(a.valid && b.valid, E, merge(a.values, updated_map_b), merge_data(a.data, b.data));
}

template <bool Computed, typename A, typename MapA, typename Data>
constexpr auto operator-(const random_var<Computed, A, MapA, Data>& a) {
  return make_random_var(a.valid, -a.expr(), a.values, a.data);
}

template <bool ComputedA, typename A, typename MapA, bool ComputedB, typename B, typename MapB>
constexpr auto operator<(const random_var<ComputedA, A, MapA>& a, const random_var<ComputedB, B, MapB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} < updated_expr_b;
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool ComputedA, typename A, typename MapA, bool ComputedB, typename B, typename MapB>
constexpr auto operator<=(const random_var<ComputedA, A, MapA>& a, const random_var<ComputedB, B, MapB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} <= updated_expr_b;
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool ComputedA, typename A, typename MapA, bool ComputedB, typename B, typename MapB>
constexpr auto operator>(const random_var<ComputedA, A, MapA>& a, const random_var<ComputedB, B, MapB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} > updated_expr_b;
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool ComputedA, typename A, typename MapA, bool ComputedB, typename B, typename MapB>
constexpr auto operator>=(const random_var<ComputedA, A, MapA>& a, const random_var<ComputedB, B, MapB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} >= updated_expr_b;
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool ComputedA, typename A, typename MapA, bool ComputedB, typename B, typename MapB>
constexpr auto operator==(const random_var<ComputedA, A, MapA>& a, const random_var<ComputedB, B, MapB>& b) {
  constexpr auto replacements = get_value_id_replacements(A{}, B{});
  constexpr auto updated_expr_b = replace_from_map(B{}, replacements);
  auto updated_map_b = update_value_ids(b.values, replacements);
  constexpr auto E = A{} == updated_expr_b;
  return make_random_var(E, merge(a.values, updated_map_b));
}

template <bool Computed, typename A, typename Map, typename Data>
constexpr auto exp(const random_var<Computed, A, Map, Data>& a) {
  return make_random_var(a.valid, exp(a.expr()), a.values, a.data);
}

template <bool Computed, typename A, typename Map, typename Data>
constexpr auto log(const random_var<Computed, A, Map, Data>& a) {
  return make_random_var(a.valid, log(a.expr()), a.values, a.data);
}

template <bool Computed, typename A, typename Map, typename Data>
constexpr auto sin(const random_var<Computed, A, Map, Data>& a) {
  return make_random_var(a.valid, sin(a.expr()), a.values, a.data);
}

template <bool Computed, typename A, typename Map, typename Data>
constexpr auto cos(const random_var<Computed, A, Map, Data>& a) {
  return make_random_var(a.valid, cos(a.expr()), a.values, a.data);
}

template <bool Computed, typename A, typename Map, typename Data>
constexpr auto tan(const random_var<Computed, A, Map, Data>& a) {
  return make_random_var(a.valid, tan(a.expr()), a.values, a.data);
}

template <bool Computed, typename A, typename Map, typename Data>
constexpr auto normalize(const random_var<Computed, A, Map, Data>& a) {
  constexpr auto e = normalize(A{});
  return make_random_var(a.valid, e, a.values, a.data);
}

template <bool Computed, typename A, typename Map, typename Data>
constexpr auto length(const random_var<Computed, A, Map, Data>& a) {
  constexpr auto e = length(A{});
  return make_random_var(a.valid, e, a.values, a.data);
}

template <typename C, bool Computed, typename A, typename Map>
constexpr auto get_value_for_condition(baseexpr<C>, const random_var<Computed, A, Map>& a) {
  constexpr auto e = get_value_for_condition(C{}, A{});
  return make_random_var(e, a.values);
}

template <typename T, EnableIf<is_guaranteed_non_negative(T{})> = 0>
constexpr T abs(baseexpr<T>) {
  return {};
}

template <typename T, EnableIf<!is_guaranteed_non_negative(T{})> = 0>
constexpr auto abs(baseexpr<T>) {
  return make_expr(abs_tag, T{});
}

template <bool Computed, typename A, typename Map>
constexpr auto abs(const random_var<Computed, A, Map>& a) {
  return make_random_var(abs(A{}), a.values);
}


template <std::size_t I, bool Computed, typename T, typename Map, typename Data>
constexpr auto at(const random_var<Computed, T, Map, Data>& a) {
  return make_random_var(a.valid, at<I>(T{}), a.values, a.data);
}

template <bool Computed, typename T, typename Map, typename Data>
constexpr auto make_array(const random_var<Computed, T, Map, Data>& a) {
  return make_random_var(a.valid, make_array(T{}), a.values, a.data);
}

template <typename T>
struct tuple_types;

template <typename... Ts>
struct tuple_types<hana::tuple<Ts...>> {
  using type = TypeList<Ts...>;
};

template <typename T>
using tuple_types_t = typename tuple_types<T>::type;

template <typename To, typename From>
constexpr void copy_data_impl(To&, const From&, TypeList<>) {
}

template <typename To, typename From, typename T, typename... Ts>
constexpr void copy_data_impl(To& to, const From& from, TypeList<T, Ts...>) {
  hana::at(to, hana::size_c<index_of_type<T, To>::value>) = hana::at(from, hana::size_c<index_of_type<T, From>::value>);
  copy_data_impl(to, from, TypeList<Ts...>{});
}

template <typename To, typename From>
constexpr void copy_data(To& to, const From& from) {
  copy_data_impl(to, from, tuple_types_t<From>{});
}


template <typename... Cases>
constexpr auto make_pattern(const Cases&... cases) {
  return make_random_var_with_data(
    make_branch_expr(List(make_expr(branch_case_tag, hana::at_c<0>(cases).expr(), hana::at_c<1>(cases).expr())...))
    , merge_all(merge(hana::at_c<0>(cases).values, hana::at_c<1>(cases).values)...)
    , merge_data(merge_data(hana::at_c<0>(cases).data, hana::at_c<1>(cases).data)...)
  );
}

template <typename C, typename V, typename... Cases>
constexpr auto pattern(const hana::tuple<C, V>& c, const Cases&... cases) {
  return make_pattern(c, cases...);
}


template <bool ComputedB, typename B, typename TupleB, typename TableB>
constexpr auto when(bool cond, const random_var<ComputedB, B, TupleB, TableB>& value) {
  return hana::make_tuple(make_value(cond), value);
}

template <bool ComputedA, typename A, typename TupleA, typename TableA, bool ComputedB, typename B, typename TupleB, typename TableB>
constexpr auto when(const random_var<ComputedA, A, TupleA, TableA>& cond, const random_var<ComputedB, B, TupleB, TableB>& value) {
  return hana::make_tuple(cond, value);
}

template <bool Computed, typename T, typename Tuple, typename Table>
constexpr auto otherwise(const random_var<Computed, T, Tuple, Table>& value) {
  return hana::make_tuple(make_random_var(trueexpr{}), value);
}

template <bool Computed, typename T, typename Map, typename Table>
constexpr auto balance(const random_var<Computed, T, Map, Table>& rv) {
  return make_random_var(rv.valid, make_balance_expr(T{}), rv.values, rv.data);
}

template <bool Computed, typename T, typename Map>
constexpr auto coordinate_basis(const random_var<Computed, T, Map>& N) {
  static_assert(is_vector(T{}), "coordinate_basis() requires a vector argument.");
  auto a = make_random_vector(make_random_var(zero), -at<2>(N), at<1>(N));
  auto b = make_random_vector(at<2>(N), make_random_var(zero), -at<0>(N));

  auto t = normalize(pattern(
    when(dot(a, a) - dot(b, b) > make_random_var(zero), a)
    , otherwise(b)
  ));

  return make_random_rotation_matrix(t, cross(N, t), N);
}

template <bool Computed, typename T, typename Tuple, typename Data, typename... Vs>
auto optional_sample(bool valid, const random_var<Computed, T, Tuple, Data>& b, Vs&&... vs) {
  return make_computed_random_var(valid, T{}, b.values, hana::concat(b.data, hana::make_tuple(std::forward<Vs>(vs)...)));
}

template <typename... Ts>
auto sample_tuple(Ts&&... ts) {
  return hana::make_tuple(std::forward<Ts>(ts)...);
}


//template <bool Computed, typename T, typename Tuple, typename Data, typename... Vs>
//auto make_optional_sample_(bool valid, const random_var<Computed, T, Tuple, Data>& b, Vs&&... vs) {
  //return make_computed_random_var(valid, T{}, b.values, hana::concat(b.data, hana::make_tuple(std::forward<Vs>(vs)...)));
//}

//template <typename T, typename Data>
//constexpr auto make_computed_random_var2(bool valid, const T& result, const Data& data) {
  //auto rv = constant(result);
  //return make_computed_random_var(valid, rv.expr(), rv.values, data);
//}

template <bool Computed, typename T, typename Tuple, typename Data, typename... Vs>
auto make_optional_sample_(bool valid, const random_var<Computed, T, Tuple, Data>& rv, Vs&&... vs) {
  return make_computed_random_var(valid, rv.expr(), rv.values, hana::concat(rv.data, hana::make_tuple(std::forward<Vs>(vs)...)));
}


template <bool Computed, typename T, typename Tuple, typename Data, typename... Vs>
auto make_computed_optional_sample_impl(bool valid, const random_var<Computed, T, Tuple, Data>& b, const hana::tuple<Vs...>& vs) {
  return make_computed_random_var(valid, T{}, b.values, hana::concat(b.data, vs));
}

template <typename RV, typename Data>
auto make_computed_optional_sample(bool valid, RV&& rv, Data&& data) {
  return make_computed_optional_sample_impl(valid, std::forward<RV>(rv), std::forward<Data>(data));
}

template <bool Computed, typename T, typename Tuple, typename Data, typename... Vs>
auto make_computed_sample_impl(const random_var<Computed, T, Tuple, Data>& b, const hana::tuple<Vs...>& vs) {
  return make_computed_random_var(true, T{}, b.values, hana::concat(b.data, vs));
}

template <typename RV, typename Data>
auto make_computed_sample(RV&& rv, Data&& data) {
  return make_computed_sample_impl(std::forward<RV>(rv), std::forward<Data>(data));
}


template <bool Computed, typename T, typename Tuple, typename Data, typename... Vs>
auto sample_(const random_var<Computed, T, Tuple, Data>& b, Vs&&... vs) {
  return make_computed_random_var(true, T{}, b.values, hana::concat(b.data, hana::make_tuple(std::forward<Vs>(vs)...)));
}

template <bool Computed, typename T, typename Tuple, typename Data, typename... Vs>
auto make_sample_(const random_var<Computed, T, Tuple, Data>& b, Vs&&... vs) {
  return make_computed_random_var(true, T{}, b.values, hana::concat(b.data, hana::make_tuple(std::forward<Vs>(vs)...)));
}


inline auto sample() {
  return make_random_var(one);
}




template <typename E, typename Tuple, typename Data>
constexpr auto random_var<false, E, Tuple, Data>::Sample() const {
  return make_computed_random_var(valid, E{}, values, data);
}

template <typename E, typename Tuple, typename Data>
template <typename... Args>
constexpr auto random_var<false, E, Tuple, Data>::Sample(Real arg, const Args&... args) const {
  constexpr auto v = vars(E{}).Difference(typename Tuple::keys_t{});
  constexpr auto N = v.Size();
  static_assert(sizeof...(Args) + 1 >= N, "Too few arguments.");
  return make_computed_random_var(E{}, merge(values, make_map(v, arg, args...)));
}

template <typename E, typename Tuple, typename Data>
template <typename P>
constexpr auto random_var<false, E, Tuple, Data>::Sample(const std::array<Real, N>& args) const {
  constexpr auto v = vars(E{});
  return make_computed_random_var(E{}, merge(values, make_map(v, args)));
}

template <int ID, typename T>
constexpr auto cast(const T& value) { 
  return make_value<ID>(value);
}

template <typename E, typename Tuple, typename Data>
template <int ID>
constexpr auto random_var<true, E, Tuple, Data>::Cast() const {
  auto c = cast<ID>(result);
  return make_computed_random_var(valid, c.expr(), c.values, data);
}

template <typename E, typename Tuple, typename Data>
constexpr auto random_var<true, E, Tuple, Data>::Cast() const {
  return Cast<0>();
}

template <typename T, int SeqIndex>
constexpr auto update_seq_indices(const T& data, _int<SeqIndex>) {
  return data;
}

template <typename N, typename T, int SeqIndex>
constexpr auto update_seq_indices_impl(const bound_named_parameter<N, T>& data, _int<SeqIndex> si) {
  return make_bound_named_parameter(N{}, update_seq_indices(data.value, si));
}

template <typename... Ts, int SeqIndex, std::size_t... Is>
constexpr auto update_seq_indices_impl(const hana::tuple<Ts...>& data, _int<SeqIndex> si, std::index_sequence<Is...>) {
  return hana::make_tuple(update_seq_indices_impl(hana::at_c<Is>(data), si)...);
}

template <typename... Ts, int SeqIndex>
constexpr auto update_seq_indices(const hana::tuple<Ts...>& data, _int<SeqIndex> si) {
  return update_seq_indices_impl(data, si, std::index_sequence_for<Ts...>{});
}

template <bool Computed, typename T, typename Map, typename Data, int SeqIndex>
constexpr auto update_seq_indices(const random_var<Computed, T, Map, Data>& rv, _int<SeqIndex> si) {
  constexpr auto replacements = get_constant_seq_index_replacements(T{}, si);
  constexpr auto updated_expr = replace_from_map(T{}, replacements);
  auto updated_map = update_seq_indices(rv.values, replacements);
  return make_computed_random_var(rv.valid, updated_expr, updated_map, update_seq_indices(rv.data, si));
}

template <bool Computed, typename T, typename Map, typename Data, int OldSeqIndex, int NewSeqIndex>
constexpr auto update_seq_indices(const random_var<Computed, T, Map, Data>& rv, _int<OldSeqIndex> oi, _int<NewSeqIndex> ni) {
  constexpr auto replacements = get_constant_seq_index_replacements(T{}, oi, ni);
  constexpr auto updated_expr = replace_from_map(T{}, replacements);
  auto updated_map = update_seq_indices(rv.values, replacements);
  return make_computed_random_var(rv.valid, updated_expr, updated_map, rv.data);
}

template <bool Computed, typename T, typename Map, typename Data, typename Adjustments>
constexpr auto adjust_seq_indices(const random_var<Computed, T, Map, Data>& rv, Adjustments replacements) {
  constexpr auto updated_expr = replace_from_map(T{}, replacements);
  auto updated_map = update_seq_indices(rv.values, replacements);
  return make_computed_random_var(rv.valid, updated_expr, updated_map, rv.data);
}

template <bool Computed, typename T, typename Map, typename Data, typename Adjustments>
constexpr auto adjust_seq_indices(const boost::optional<const random_var<Computed, T, Map, Data>>& rv, Adjustments replacements) {
  using result_t = boost::optional<const decltype(adjust_seq_indices(std::declval<random_var<Computed, T, Map, Data>>(), replacements))>;
  return bool(rv) ? adjust_seq_indices(rv.get(), replacements) : result_t{};
}

constexpr auto zero_vector3() {
  return make_random_vector(make_random_var(zero), make_random_var(zero), make_random_var(zero));
}

constexpr auto one_vector3() {
  return make_random_vector(make_random_var(one), make_random_var(one), make_random_var(one));
}

template <typename T, typename Map, typename Data, EnableIf<(random_var<false, T, Map, Data>::N > 0)> = 0>
std::ostream& operator<<(std::ostream& out, const random_var<false, T, Map, Data>& rv) {
  static_assert(!std::is_same<T, T>::value, "Must call Sample() before printing a random var");
  return out << rv.Value();
}

template <typename T, typename Map, typename Data, EnableIf<(random_var<false, T, Map, Data>::N == 0)> = 0>
std::ostream& operator<<(std::ostream& out, const random_var<false, T, Map, Data>& rv) {
  return out << rv.Value();
}

template <bool Computed, typename T, typename Map, typename Data>
std::ostream& operator<<(std::ostream& out, const random_var<true, T, Map, Data>& rv) {
  return out << rv.Value();
}


} // end namespace aether

#endif
