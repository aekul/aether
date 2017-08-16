#ifndef FWD_TYPELIST_PICK_H
#define FWD_TYPELIST_PICK_H

#include "aether/fwd/TypeTraits.h"
#include "TypeList.h"

namespace aether {

template <std::size_t I, typename... Ts>
constexpr auto pick(TypeList<Ts...>);

} // end namespace aether


#endif
