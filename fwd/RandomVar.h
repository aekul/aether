#ifndef FWD_RANDOM_VAR_H
#define FWD_RANDOM_VAR_H

#include <boost/hana.hpp>

namespace aether {

using namespace boost;

using empty_tuple_t = hana::tuple<>;

template <bool Computed, typename E, typename Map, typename Data = empty_tuple_t>
struct random_var;

template <typename T>
struct is_random_var : std::false_type {};

template <bool Computed, typename E, typename Map, typename Data>
struct is_random_var<random_var<Computed, E, Map, Data>> : std::true_type {};

template <typename T>
constexpr bool is_random_var_v = is_random_var<T>::value;

} // end namespace aether

#endif
