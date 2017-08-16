#ifndef FWD_CANONICAL_H
#define FWD_CANONICAL_H

#include "Expr.h"

namespace aether {

template <typename T>
constexpr auto canonicalize(baseexpr<T>);

} // end namespace aether

#endif
