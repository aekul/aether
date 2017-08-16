#ifndef FWD_LITERAL_H
#define FWD_LITERAL_H

#include <ratio>

namespace aether {

template <std::intmax_t N, std::intmax_t D>
struct literal;

template <std::intmax_t N, std::intmax_t D>
constexpr auto canonicalize(literal<N, D>);

  
} // end namespace aether


#endif
