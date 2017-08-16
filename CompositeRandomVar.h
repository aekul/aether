#ifndef COMPOSITE_RANDOM_VAR_H
#define COMPOSITE_RANDOM_VAR_H

#include <boost/variant.hpp>

#include "RandomVar.h"

namespace aether {

template <typename... Ts>
struct CompositeRandomVar {
  using Types = TypeSet<Ts...>;

  static constexpr std::size_t N = sizeof...(Ts);

  CompositeRandomVar() {}

  template <typename T>
  CompositeRandomVar(const T& v) : value{v} {
    static_assert(Types{}.template Has<T>(), "Type not found in variant.");

    constexpr auto index = index_of.operator()<T>(Types{});
    which = index;
    valid = true;
  }

  int which{};
  bool valid{};
  mutable boost::variant<Ts...> value;

  template <typename T>
  bool IsA() const {
    constexpr auto index = index_of.operator()<T>(Types{});
    return which == index;
  }

  explicit operator bool() const {
    return valid;
  }

  bool operator==(const CompositeRandomVar& other) const {
    if (which != other.which) {
      return false;
    }

    return value == other.value;
  }

  template <template <typename> class F, int I>
  struct CompositeRandomVarImpl : Function<CompositeRandomVarImpl<F, I>> {
    CompositeRandomVarImpl(int w, bool v, boost::variant<Ts...>& val)
      : which(w)
      , valid(v)
      , value(val)
    {}

    int which{};
    bool valid{};
    boost::variant<Ts...>& value;

    template <typename T, typename Cond, typename... Args>
    struct copy_ : boost::static_visitor<void> {
      copy_(T& t, Cond index, Args&&... args) : to(t), index(index), args(std::forward_as_tuple(args...)) {}

      T& to;
      Cond index;
      std::tuple<Args&...> args;

      template <typename RV, std::size_t... Is>
      void impl(const RV& rv, std::index_sequence<Is...>) {
        auto sample = F<RV>{}(rv, _int<I>{}, index, std::get<Is>(args)...);
        copy(to.values, sample.values);
        to.result = sample.result;
        to.valid = sample.valid;
        copy_data(to.data, sample.data);
      }

      template <typename RV>
      void operator()(const RV& rv) {
        impl(rv, std::make_index_sequence<sizeof...(Args)>{});
      }
    };

    template <typename B, typename Cond, typename... Args>
    auto make_copy_visitor(B& branch_rv, Cond index, Args&&... args) const {
      return copy_<B, Cond, Args...>{branch_rv, index, std::forward<Args>(args)...};
    }

    // each branch must return a random variable
    template <typename... Is, typename... Rs, typename Back, typename... Args>
    auto make_impl(TypeSet<Rs...>, Type<Back>, TypeList<Is...>, Args&&... args) const {
      auto index = update_seq_indices(constant(which), _int<I>{});

      auto branch_rv = pattern(
        when(index == make_random_var(Is{}), decltype(std::declval<F<Rs>>()(std::declval<Rs>(), _int<I>{}, index, std::forward<Args>(args)...)){})...
        , otherwise(decltype(std::declval<F<Back>>()(std::declval<Back>(), _int<I>{}, index, std::forward<Args>(args)...)){}) 
      );

      auto result_rv = make_computed_random_var_without_value(valid, branch_rv);

      if (!valid) {
        return result_rv;
      }

      auto c = make_copy_visitor(result_rv, index, std::forward<Args>(args)...);
      boost::apply_visitor(c, value);

      set(index.expr(), which, result_rv.values);
      return result_rv;
    }

    template <typename... Args>
    auto impl(Args&&... args) const {
      return make_impl(
        remove_back(Types{})
        , Type<back_type_t<Types>>{}
        , make_literal_sequence<N - 1>()
        , std::forward<Args>(args)...
      );
    }
  };

  template <template <typename> class F, int I, typename... Args>
  auto operator()(Args&&... args) const {
    auto f = CompositeRandomVarImpl<F, I>{which, valid, value};
    return f.impl(std::forward<Args>(args)...);
  }

  template <typename... Args>
  auto Sample(Args&&... args) const {
    auto f = CompositeRandomVarImpl<SampleCall, -1>{which, valid, value};
    return f.impl(std::forward<Args>(args)...);
  }
};

template <template <typename> class F, typename T, typename... Args>
auto invoke(const T& t, Args&&... args) {
  return t.template operator()<F, -1>(std::forward<Args>(args)...);
}

template <template <typename> class F, int I, typename T, typename... Args>
auto invoke(const T& t, Args&&... args) {
  return t.template operator()<F, I>(std::forward<Args>(args)...);
}


} // end namespace aether

#endif
