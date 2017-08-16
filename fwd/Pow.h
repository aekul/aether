#ifndef FWD_POW_H
#define FWD_POW_H

namespace aether {

template <typename T, typename E>
struct powerexpr;

template <typename B, typename E>
constexpr auto canonicalize(powerexpr<B, E>);

} // end namespace aether

#endif
