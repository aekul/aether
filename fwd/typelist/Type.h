#ifndef FWD_TYPELIST_TYPE_H
#define FWD_TYPELIST_TYPE_H

namespace aether {

template <typename T>
struct Type {
  using type = T;
};

} // end namespace aether

#endif
