#ifndef FWD_RANDOM_SEQUENCE_H
#define FWD_RANDOM_SEQUENCE_H

#include <type_traits>

namespace aether {

template <typename T>
struct RandomSequence;

template <typename T>
struct is_random_sequence : std::false_type {};

template <typename T>
struct is_random_sequence<RandomSequence<T>> : std::true_type {};

template <typename T>
constexpr bool is_random_sequence_v = is_random_sequence<std::decay_t<T>>::value;


template <typename T>
struct RandomSequenceView;


} // end namespace aether

#endif
