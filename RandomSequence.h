#ifndef AETHER_RANDOM_SEQUENCE_H
#define AETHER_RANDOM_SEQUENCE_H

#include <random>
#include <cmath>

#include "aether/RandomVar.h"
#include "aether/Object.h"
#include "aether/context/SamplingContext.h"
#include "aether/context/PdfContext.h"
#include "aether/Strategy.h"
#include "aether/CachedFunction.h"

namespace aether {

template <typename T>
struct Node : T {
  template <typename... Ts>
  Node(Ts&&... ts)
    : T{std::forward<Ts>(ts)...}
  {}

  T& Derived() {
    return static_cast<T&>(*this);
  }

  template <typename... Args>
  auto operator()(Args&&... args) {
    return Derived()(std::forward<Args>(args)...);
  }

  template <typename... Args>
  auto Sample(Args&&... args) {
    auto context = make_sampling_context();
    return Derived()(context, std::forward<Args>(args)...);
  }
};

template <typename T>
struct output_length_t;

template <>
struct output_length_t<Vector3> {
  static constexpr std::size_t N = 3;
};

template <typename T>
constexpr std::size_t output_length = output_length_t<T>::N;

template <typename T, typename D = empty_named_struct>
struct Element : Object {
  using scalar_t = T;
  using data_t = D;
  using named_param_keys_t = typename D::K;
  using Store = D;
  static constexpr std::size_t N = output_length<T>;

  Element() = default;

  template <typename... Vs>
  Element(bool valid, const scalar_t& value, Vs&&... vs) 
    : value{value}
    , valid{valid}
  {
    using consume = int[];
    (void)consume{1, (Set(vs), 1)...};
  }

  template <typename... Vs>
  Element(const scalar_t& value, Vs&&... vs) 
    : Element(true, value, vs...)
  {
  }

  scalar_t value{};
  data_t data;
  bool valid{};

  template <typename Name>
  constexpr bool HasKey(Name) const {
    return data.HasKey(Name{}); 
  }

  template <typename Name>
  constexpr auto& Get(named_parameter<Name>) const {
    constexpr auto key = named_parameter<Name>{};
    return data[key];
  }

  template <typename Name>
  auto& Get(named_parameter<Name>) {
    constexpr auto key = named_parameter<Name>{};
    return data[key];
  }

  template <typename Name>
  constexpr auto& operator[](named_parameter<Name>) const {
    constexpr auto key = named_parameter<Name>{};
    return data[key];
  }

  template <typename Name>
  auto& operator[](named_parameter<Name>) {
    constexpr auto key = named_parameter<Name>{};
    return data[key];
  }

  const auto& Value() const {
    return value;
  }

  template <typename Name, typename V>
  void Set(const bound_named_parameter<Name, V>& bound) {
    constexpr auto key = named_parameter<Name>{};
    data[key] = bound.Value();
  }

  template <typename Name, bool Computed, typename E, typename Map, typename Data>
  void Set(const bound_named_parameter<Name, random_var<Computed, E, Map, Data>>& bound) {
    constexpr auto key = named_parameter<Name>{};
    data[key] = bound.Value().Value();
  }

  bool Valid() const {
    return valid;
  }
};

template <typename T, typename D, typename Name>
decltype(auto) get_by_key(const Element<T, D>& elm, Name name) {
  return elm[name];
}

template <std::size_t N, typename T, typename D>
auto get_sample_value(const Element<T, D>& elm) {
  return elm.Value();
}

template <std::size_t N, typename T, typename D>
auto get_sample_value(const std::vector<Element<T, D>>& elm) {
  std::array<Real, N> result;
  constexpr std::size_t element_size = Element<T, D>::N;
  constexpr std::size_t num_elements = N / element_size;

  std::size_t k = 0;
  for (std::size_t i = 0; i < num_elements; ++i) {
    for (std::size_t j = 0; j < element_size; ++j) {
      result[k++] = elm[i].Value()[j];
    }
  }

  return result;
}

template <typename T, typename D>
struct pdf_traits<Element<T, D>> {
  using data_keys_t = typename Element<T, D>::named_param_keys_t;
};

template <typename T>
struct pdf_traits<boost::optional<T>> : pdf_traits<T> {};

template <typename T>
struct pdf_traits<std::vector<T>> : pdf_traits<T> {};

template <typename T, typename D>
bool operator==(const Element<T, D>& a, const Element<T, D>& b) {
  return a.value == b.value;
}

template <typename T, typename D>
bool operator!=(const Element<T, D>& a, const Element<T, D>& b) {
  return !(a.value == b.value);
}

template <typename Name, typename T, typename Store>
struct HasKey<Name, Element<T, Store>> : HasKey<Name, Store> {};


template <typename... Ks, typename Data>
auto named_struct_to_tuple(const named_struct<TypeSet<Ks...>, Data>& ns) {
  return hana::make_tuple(Ks{} = ns[Ks{}]...);
}

template <typename T, typename Data>
auto make_random_var(const Element<T, Data>& element) {
  auto rv = value_(element.value);
  return make_random_var(element.valid, rv.expr(), rv.values, named_struct_to_tuple(element.data));
}

template <typename E, typename... Vs>
constexpr E make_optional_elm_impl(bool valid, const typename E::scalar_t& t, Vs&&... vs) {
  return E{valid, t, vs...};
}

template <typename E, typename... Vs>
constexpr E make_elm_impl(const typename E::scalar_t& t, Vs&&... vs) {
  return E{t, vs...};
}


template <typename E, typename... Vs, std::size_t... Is>
constexpr E make_elm_helper(bool valid, const typename E::scalar_t& value, const hana::tuple<Vs...>& vs, std::index_sequence<Is...>) {
  return make_optional_elm_impl<E>(valid, value, hana::at_c<Is>(vs)...);
}

template <typename E, bool Computed, typename T, typename Map, typename Data, typename... Vs>
constexpr E make_elm_from_rv_and_tuple(const random_var<Computed, T, Map, Data>& rv, const hana::tuple<Vs...>& vs) {
  return make_elm_helper<E>(rv.valid, rv.Value(), vs, std::index_sequence_for<Vs...>{});
}

template <typename E, bool Computed, typename T, typename Map, typename Data>
constexpr E make_elm(const random_var<Computed, T, Map, Data>& rv) {
  return make_elm_from_rv_and_tuple<E>(rv, rv.data);
}

template <typename E, bool Computed, typename T, typename Map, typename Data>
constexpr E make_elm(const boost::optional<random_var<Computed, T, Map, Data>>& rv) {
  if (!rv) {
    return {};
  }
  return make_elm_from_rv_and_tuple<E>(rv.get(), rv.get().data);
}

template <typename E, typename... Vs>
constexpr E make_elm(const typename E::scalar_t& t, Vs&&... vs) {
  return make_elm_impl<E>(t, std::forward<Vs>(vs)...);
}

template <typename Heuristic, typename Store>
struct CombineCall {
  CombineCall(const Store& values) : store(values) {}

  Store store;
};


template <typename... Ts>
struct back_type<hana::tuple<Ts...>> {
  using type = decltype(back(List(Ts{}...)));
};


template <typename... Ts>
struct size_of_tuple_t<hana::tuple<Ts...>> {
  static constexpr std::size_t value = sizeof...(Ts);
};

template <typename T, typename Data>
constexpr auto named_params(const Element<T, Data>& elm) {
  return named_params(elm.value);
}


struct balance_heuristic {
  template <std::size_t N>
  Real operator()(int index, const std::array<Real, N>& pdfs) {
    Real denom = Real(0);
    for (auto p : pdfs) {
      denom += p;
    }

    if (denom == Real(0)) {
      return Real(0);
    }

    return pdfs[index] / denom;
  }
};

  
template <typename T>
struct SequenceElement;

template <typename T>
struct Strategy;

template <typename S>
struct StrategySequence {
  using strategy_t = Strategy<S>;

  template <typename T, typename... Args>
  void Append(Node<T>& node, Args&&... args);

  template <typename E, typename Tuple, typename Data>
  void Append(const random_var<true, E, Tuple, Data>& rv);

  void Append(std::shared_ptr<strategy_t> strategy);

  void RemoveBack();

  const auto& operator[](std::size_t i) const;
  auto& operator[](std::size_t i);

  const auto& Back() const;
  auto& Back();

  std::size_t Size() const;
  auto begin() const;
  auto end() const;

  StrategySequence<S> Slice(std::size_t I, std::size_t N) const;

  std::vector<std::shared_ptr<strategy_t>> store{};
};

template <typename S>
struct RandomSequence {
  using element_t = S;
  using sample_t = SequenceElement<element_t>;

  auto Concat(const RandomSequence<S>& other) const;

  template <typename T, typename... Args>
  void Append(Node<T>& node, Args&&... args);

  void Append(std::shared_ptr<Strategy<S>> strategy);

  void Append(const Strategy<S>& strategy);

  template <typename Fn>
  void Append(std::size_t I, const Fn& fn);

  template <typename E, typename Tuple, typename Data>
  void Append(random_var<true, E, Tuple, Data> rv);

  void RemoveBack();

  template <typename T, typename... Args>
  RandomSequence<S> Mutate(Node<T>& node, Args&&... args) const;

  template <typename Fn>
  RandomSequence<S> Mutate(const Fn& fn) const;

  template <typename I, typename N>
  auto Slice(I&& start_index, N&& length) const {
    return slice_(std::forward<I>(start_index), std::forward<N>(length), *this);
  }

  auto Size() const {
    return store.size();
  }

  auto Back() const {
    return store.back().get().rv.get();
  }

  auto operator[](std::size_t i) const {
    return store.at(i).get().rv.get();
  }

  bool IsEmpty() const {
    return store.empty();
  }

  bool AllInvalid() const {
    return std::all_of(store.begin(), store.end(), [](const auto& element) {
      return !element.rv || !element.rv.get().Valid();
    });
  }

  bool AllValid() const {
    return std::all_of(store.cbegin(), store.cend(), [](const auto& element) {
      return element.get().rv && element.get().rv.get().Valid();
    });
  }

  template <typename Seq>
  Real Pdf(const Seq& query_seq) const {
    SourceDistributionHelper source_distribution_helper;
    return Pdf(query_seq, source_distribution_helper);
  }

  template <typename Seq>
  Real Pdf(const Seq& query_seq, SourceDistributionHelper &source_distribution_helper) const {
    if (!query_seq.AllValid()) {
      return Real(0);
    }

    std::size_t total_strategy_output_size = 0;
    for (const auto& strategy : strategies) {
      total_strategy_output_size += strategy->output_size;
    }

    if (query_seq.Size() != total_strategy_output_size) {
      return Real(0);
    }

    Real p = 1;
    using T = typename Seq::sample_t::optional_element_t;

    std::size_t sample_index = 0;
    for (std::size_t strategy_index = 0, N = strategies.Size(); strategy_index < N; ++strategy_index) {
      const auto& strategy = strategies[strategy_index];
      std::vector<T> query_samples;
      query_samples.reserve(strategy->output_size);
      for (std::size_t i = 0; i < strategy->output_size; ++i) {
        query_samples.push_back(query_seq.store.at(sample_index + i).get().rv);
      }

      auto pdf = strategy->Pdf(query_seq, query_samples, sample_index,
                               reversed[sample_index], source_distribution_helper);
      if (pdf == 0 || std::isnan(pdf)) {
        return 0;
      }
      p *= pdf;
      sample_index += strategy->output_size;
    }

    return p;
  }

  Real Pdf() const {
    return Pdf(*this);
  }

  template <typename T>
  friend std::size_t hash_value(const RandomSequence<T>& seq) {
    std::size_t seed{0};
    for (std::size_t i = 0, N = seq.Size(); i < N; ++i) {
      boost::hash_combine(seed, seq[i]);
    }
    return seed;
  }

  void Sample(std::size_t strategy_index, std::size_t sample_index);
  void Sample();

  std::vector<boost::optional<sample_t>> store{};
  StrategySequence<S> strategies;
  std::vector<bool> reversed{};
};


template <typename Heuristic, typename S>
struct CombinedSample {
  using sample_t = S;

  struct WeightedSample {
    sample_t& sequence;
    Real weight;
  };

  std::vector<WeightedSample> weighted_samples;

  CombinedSample(std::vector<S>& samples) {
    for (int i = 0, N = samples.size(); i < N; ++i) {
      weighted_samples.push_back(WeightedSample{samples[i], Weight(samples, i)});
    }
  }

  CombinedSample(const std::vector<std::reference_wrapper<S>>& samples) {
    for (int i = 0, N = samples.size(); i < N; ++i) {
      weighted_samples.push_back(WeightedSample{samples[i].get(), Weight(samples, i)});
    }
  }
 
  auto begin() {
    return weighted_samples.begin();
  }

  auto begin() const {
    return weighted_samples.cbegin();
  }

  auto end() {
    return weighted_samples.end();
  }

  auto end() const {
    return weighted_samples.cend();
  }

  std::vector<double> Pdfs(std::vector<sample_t>& samples, std::size_t i) const {
    const auto& sample = samples[i];

    std::vector<double> pdfs(samples.size());
    for (std::size_t j = 0, N = samples.size(); j < N; ++j) {
      pdfs[j] = samples[j].Pdf(sample); 
    }

    return pdfs;
  }

  std::vector<double> Pdfs(const std::vector<std::reference_wrapper<sample_t>>& samples, std::size_t i) const {
    const auto& sample = samples[i].get();

    std::vector<double> pdfs(samples.size());
    for (std::size_t j = 0, N = samples.size(); j < N; ++j) {
      pdfs[j] = samples[j].get().Pdf(sample); 
    }

    return pdfs;
  }

  std::vector<std::vector<double>> Pdfs(std::vector<sample_t>& samples) const {
    std::vector<std::vector<double>> pdfs(samples.size());

    for (std::size_t i = 0, N = samples.size(); i < N; ++i) {
      pdfs[i] = Pdfs(samples, i);
    }

    return pdfs;
  }

  Real Weight(std::vector<sample_t>& samples, std::size_t i) const {
    return Heuristic{}(i, Pdfs(samples, i));
  }

  Real Weight(const std::vector<std::reference_wrapper<sample_t>>& samples, std::size_t i) const {
    return Heuristic{}(i, Pdfs(samples, i));
  }

  const auto& operator[](std::size_t i) const {
    return weighted_samples[i];
  }

  auto& operator[](std::size_t i) {
    return weighted_samples[i];
  }

  std::size_t Size() const {
    return weighted_samples.size();
  }
};

template <typename Heuristic>
struct combine_t {
  template <typename Sample, typename... Samples>
  auto operator()(Sample& sample, Samples&... samples) const {
    static_assert(sizeof...(Samples) >= 1, "combine() requires at least 2 samples");

    return CombinedSample<Heuristic, std::decay_t<Sample>>{std::vector<std::reference_wrapper<std::decay_t<Sample>>>{sample, samples...}};
  }

  template <typename Sample>
  auto operator()(std::vector<Sample>& samples) const {
    return CombinedSample<Heuristic, std::decay_t<Sample>>{samples};
  }
};

template <>
struct combine_t<balance_heuristic> {
  template <typename... Is, bool Computed, typename E, typename Tuple, typename Data, typename... Ts>
  auto impl(TypeList<Is...>, const random_var<Computed, E, Tuple, Data>& rv, const Ts&... others) const {
    constexpr auto N = make_literal<sizeof...(Is) + 1>();

    auto branch_rv = pattern(
      when(N * make_random_var(u0) <= make_random_var(Is{} + one), others)...
      , otherwise(rv)
    );

    return balance(branch_rv); 
  }

  template <bool Computed, typename E, typename Tuple, typename Data, typename... Ts>
  auto operator()(const random_var<Computed, E, Tuple, Data>& rv, const Ts&... others) const {
    return impl(
      make_literal_sequence<sizeof...(Ts)>()
      , rv
      , others...
    );
  }
};

template <typename Heuristic>
constexpr combine_t<Heuristic> combine{};





template <typename T>
decltype(auto) get_by_key(const RandomSequence<T>& seq, std::size_t i) {
  return seq.store.at(i)->rv.get();
}

template <typename T>
struct RandomSequenceView {
  using random_sequence_t = RandomSequence<T>;
  using random_sequence_view_t = RandomSequenceView<T>;

  RandomSequenceView(const random_sequence_t& seq)
    : seq{seq}
    , I{0}
    , N{seq.store.size()}
  {}

  RandomSequenceView(const random_sequence_t& seq, const std::size_t I, const std::size_t N)
    : seq{seq}
    , I{I}
    , N{N}
  {}

  const random_sequence_t& seq;
  std::size_t I;
  std::size_t N;

  const auto& operator[](std::size_t i) const {
    return seq.store.at(I + i).get().rv.get();
  }

  auto Size() const {
    return N;
  }

  Real Pdf(const random_sequence_view_t& query_view_seq) const {
    return Pdf(query_view_seq.seq);
  }

  Real Pdf(const random_sequence_t& query_seq) const {
    return seq.Pdf(query_seq);
  }

  bool AllValid() const {
    return seq.AllValid();
  }
};

template <typename T>
RandomSequenceView<T> make_view(const RandomSequence<T>& seq) {
  return {seq};
}


template <typename Seq>
std::ostream& operator<<(std::ostream& out, const RandomSequence<Seq>& seq) {
  int i = 0;
  for (const auto& sample : seq.store) {
    out << i++ << ": " << sample.get() << "\n";
  }
  return out << "\n";
}

template <typename T>
bool operator==(const RandomSequence<T>& a, const RandomSequence<T>& b) {
  std::size_t N = a.Size().Eval();
  if (N != b.Size().Eval()) {
    return false;
  }

  for (std::size_t i = 0; i < N; ++i) {
    if (a[i].Eval() != b[i].Eval()) {
      return false;
    }
  }

  return true;
}

template <typename T>
struct SequenceElement {
  using element_t = T;
  using optional_element_t = boost::optional<T>;
  using sequence_t = RandomSequence<element_t>;

  SequenceElement(const optional_element_t& r)
    : rv{r}
  {}

  SequenceElement(const element_t& r)
    : rv{r}
  {}

  auto Value() const {
    return rv.get().Value();
  }

  optional_element_t rv;
};


template <typename T, typename D>
std::ostream& operator<<(std::ostream& out, const Element<T, D>& elm) {
  return out << "id = " << elm.id << "\n" <<  elm.Value();
}

template <typename T>
std::ostream& operator<<(std::ostream& out, const SequenceElement<T>& elm) {
  if (!elm.rv || !elm.rv.get().Valid()) {
    return out << "invalid\n";
  }
  return out << elm.rv.get();
}

template <typename T>
bool operator==(const SequenceElement<T>& a, const SequenceElement<T>& b) {
  return a.rv == b.rv;
}

template <typename T, typename Data>
bool operator==(const SequenceElement<Element<T, Data>>& a, const Element<T, Data>& b) {
  return a.rv && a.rv == b;
}

template <typename T, typename Data>
bool operator==(const Element<T, Data>& a, const SequenceElement<Element<T, Data>>& b) {
  return b.rv && a == b.rv;
}

template <typename... Ts, typename T, typename Data>
auto get_data_as_map_impl(TypeSet<Ts...>, const Element<T, Data>& rv) {
  return merge_all(named_constant(Ts{}, rv.Get(Ts{})).values...);
}

template <typename... Ts, typename T, typename Data>
auto get_data_as_map(TypeSet<Ts...> keys, const Element<T, Data>& rv) {
  return get_data_as_map_impl(MergeAll(make_type_set(get_name(Ts{}))...), rv);
}

template <typename E>
SequenceElement<E> make_element_impl(const E& elm) {
  return {elm};
}

template <typename T, typename E, typename D>
auto make_element(const boost::optional<Element<E, D>>& elm) {
  return std::vector<std::decay_t<decltype(make_element_impl(elm.get()))>>{make_element_impl(elm.get())};
}

template <typename T, typename E, typename Tuple, typename Data>
auto make_element(const random_var<false, E, Tuple, Data>& rv) {
  return std::vector<std::decay_t<decltype(make_element_impl(make_elm<T>(rv)))>>{make_element_impl(make_elm<T>(rv))};
}

template <typename T, typename E, typename Tuple, typename Data>
auto make_element(const random_var<true, E, Tuple, Data>& rv) {
  return std::vector<std::decay_t<decltype(make_element_impl(make_elm<T>(rv)))>>{make_element_impl(make_elm<T>(rv))};
}

template <typename T, typename E, typename Tuple, typename Data>
auto make_element(const boost::optional<random_var<true, E, Tuple, Data>>& rv) {
  return std::vector<std::decay_t<decltype(make_element_impl(make_elm<T>(rv)))>>{make_element_impl(make_elm<T>(rv))};
}

template <typename T, typename... Ts>
auto make_element(const boost::optional<hana::tuple<Ts...>>& samples) {
  using sample_t = std::decay_t<decltype(hana::at_c<0>(std::declval<hana::tuple<Ts...>>()))>;
  using optional_sample_t = boost::optional<sample_t>;
  using elm_t = decltype(make_element_impl(make_elm<T>(std::declval<sample_t>())));

  std::vector<elm_t> elements;

  if (!samples) {
    for (std::size_t i = 0, N = sizeof...(Ts); i < N; ++i) {
      elements.push_back(make_element_impl(make_elm<T>(optional_sample_t{})));
    }
    return elements;
  }

  hana::for_each(samples.get(), [&](const auto& sample) {
    elements.push_back(make_element_impl(make_elm<T>(sample)));
  });

  return elements;
}

template <typename T>
struct PdfFn : CachedFunction<PdfFn<T>, Real> {
  using element_t = T;
  using optional_element_t = boost::optional<T>;
  using sequence_t = RandomSequence<element_t>;
  using sequence_element_t = std::shared_ptr<SequenceElement<T>>;
  using pdf_fn_t = std::function<Real(const sequence_t&, const std::vector<optional_element_t>&, int, bool, SourceDistributionHelper&)>;

  template <typename R, typename... Args>
  PdfFn(Node<R>& node, Args&&... args)
    : pdf_fn{MakePdfFunction(node, std::forward<Args>(args)...)}
  {}

  template <typename E, typename Tuple, typename Data>
  PdfFn(const random_var<true, E, Tuple, Data>& rv)
    : pdf_fn{MakePdfFunction(rv)}
  {}


  template <typename R, typename... Args>
  pdf_fn_t MakePdfFunction(Node<R>& node, Args&&... args) {
    return [=](const sequence_t& seq, const std::vector<optional_element_t>& query_sample, int I, bool reversed, SourceDistributionHelper &source_distribution_helper) mutable {
      return Pdf_(
        node
        , query_sample
        , reversed ? reverse_(slice_(I + 1, seq)) : slice_(0, I, seq)
        , source_distribution_helper
        , args...
      );
    };
  }

  Real impl(const sequence_t& seq, const std::vector<optional_element_t>& query_samples, int I, bool reversed, SourceDistributionHelper &source_distribution_helper) const {
    return pdf_fn(seq, query_samples, I, reversed, source_distribution_helper);
  }

  std::size_t hash_value(const sequence_t& seq, const std::vector<optional_element_t>& query_samples, int I, bool reversed, SourceDistributionHelper &source_distribution_helper) const {
    std::size_t seed{0};
    if (reversed) {
      for (std::size_t i = seq.Size() - 1; i >= I + 1; --i) {
        boost::hash_combine(seed, seq[i]);
      }
    } else {
      for (std::size_t i = 0; i < I; ++i) {
        boost::hash_combine(seed, seq[i]);
      }
    }
    
    for (std::size_t i = 0, N = query_samples.size(); i < N; ++i) {
      boost::hash_combine(seed, query_samples[i].get());
    }
    return seed;
  }

  template <typename E, typename Tuple, typename Data>
  pdf_fn_t MakePdfFunction(const random_var<true, E, Tuple, Data>& rv) {
    return [=](const sequence_t& seq, const std::vector<optional_element_t>& query_samples, int, bool, SourceDistributionHelper &source_distribution_helper) mutable {
      return Pdf_(query_samples, rv, source_distribution_helper);
    };
  }

private:
  pdf_fn_t pdf_fn;
};

template <typename T>
struct node_output_size_t {
  static constexpr std::size_t N = 1;
};

template <typename... Ts>
struct node_output_size_t<hana::tuple<Ts...>> {
  static constexpr std::size_t N = sizeof...(Ts);
};

template <typename T>
constexpr std::size_t node_output_size = node_output_size_t<T>::N; 

template <typename T, typename R, typename... Args>
constexpr auto get_output_size(Node<R>& node, Args&&... args) {
  return node_output_size<decltype(node.Sample(std::declval<T>(), std::forward<Args>(args)...))>;
}

template <typename T>
struct Strategy {
  using element_t = T;
  using optional_element_t = boost::optional<T>;
  using sequence_t = RandomSequence<element_t>;
  using sequence_element_t = SequenceElement<T>;
  using pdf_fn_t = std::function<Real(const sequence_t&, const std::vector<optional_element_t>&, int, bool, SourceDistributionHelper&)>;
  using sample_fn_t = std::function<std::vector<sequence_element_t>(const sequence_t&)>;

  template <typename R, typename... Args>
  Strategy(Node<R>& node, Args&&... args)
    : sample_fn{MakeSampleFunction(node, std::forward<Args>(args)...)}
    , pdf_fn{PdfFn<T>(node, std::forward<Args>(args)...)}
    , output_size{get_output_size<sequence_t>(node, std::forward<Args>(args)...)}
  {}

  template <typename E, typename Tuple, typename Data>
  Strategy(const random_var<true, E, Tuple, Data>& rv)
    : sample_fn{MakeSampleFunction(rv)}
    , pdf_fn{PdfFn<T>(rv)}
    , output_size{1}
  {}

  template <typename E, typename Tuple, typename Data>
  sample_fn_t MakeSampleFunction(const random_var<true, E, Tuple, Data>& rv) {
    return [=](const sequence_t& seq) mutable {
      using result_t = boost::optional<random_var<true, E, Tuple, Data>>;
      auto result = seq.AllValid() ? rv : result_t{};
      return make_element<T>(result);
    };
  }

  template <typename R, typename... Args>
  sample_fn_t MakeSampleFunction(Node<R>& node, Args&&... args) {
    return [=](const sequence_t& seq) mutable {
      using result_t = boost::optional<decltype(node.Sample(seq, args...))>;
      auto result = seq.AllValid() ? node.Sample(seq, args...) : result_t{};
      return make_element<T>(result);
    };
  }

  Real Pdf(const sequence_t& seq, const std::vector<optional_element_t>& query_samples, int I, bool reversed, SourceDistributionHelper &source_distribution_helper) {
    return pdf_fn(seq, query_samples, I, reversed, source_distribution_helper);
  }

  auto Sample(const sequence_t& seq) {
    return sample_fn(seq);
  }

  sample_fn_t sample_fn;
  pdf_fn_t pdf_fn;
  std::size_t output_size;
};

template <typename S, typename T, typename... Args>
auto make_strategy(const Node<T>& node, Args&&... args) {
  return Strategy<S>{node, std::forward<Args>(args)...};
}


template <typename S>
bool is_valid(const RandomSequence<S>& seq) {
  return seq.AllValid();
}

template <typename S>
auto cast_to_constant(RandomSequence<S> seq) {
  for (int i = 0, N = seq.store.size(); i < N; ++i) {
    using pdf_fn_t = std::function<Real(const RandomSequence<S>&, const std::vector<boost::optional<S>>&, int, bool, SourceDistributionHelper&)>;

    auto expected_result = seq.store.at(i).get();
    pdf_fn_t pdf_fn = [=](const RandomSequence<S>& seq, const std::vector<boost::optional<S>>& query_samples, int I, bool reversed, SourceDistributionHelper&) {
      if (expected_result.rv.get() == query_samples[0].get()) {
        return Real(1);
      }
      return Real(0);
    };

    seq.strategies[i] = std::make_shared<Strategy<S>>(*seq.strategies[i]);
    seq.strategies[i]->pdf_fn = pdf_fn;
  }
  return seq;
}

template <typename S>
RandomSequenceView<S> slice_(std::size_t I, std::size_t N, const RandomSequenceView<S>& view) {
  return {view.seq, view.I + I, N};
}

template <typename S>
auto slice_(std::size_t I, const RandomSequence<S>& seq) {
  return slice_(I, seq.Size() - I, seq);
}

template <typename T>
std::vector<T> slice_impl(std::size_t I, std::size_t N, const std::vector<T>& vec) {
  return std::vector<T>(vec.cbegin() + I, vec.cbegin() + I + N);
}

template <typename S>
std::size_t matching_strategy_index(std::size_t I, const RandomSequence<S>& seq) {
  std::size_t strategy_index = 0;
  for (std::size_t i = 0; strategy_index < I && i < I; ) {
    i += seq.strategies[strategy_index]->output_size;
    ++strategy_index;
  }
  return strategy_index;
}

template <typename S>
RandomSequence<S> slice_(std::size_t I, std::size_t N, const RandomSequence<S>& seq) {
  auto strategy_start_index = matching_strategy_index(I, seq);
  auto strategy_end_index = matching_strategy_index(I + N, seq);

  //using sample_t = typename RandomSequence<S>::sample_t;
  return {
    slice_impl(I, N, seq.store)
    , seq.strategies.Slice(strategy_start_index, strategy_end_index - strategy_start_index)
    , slice_impl(I, N, seq.reversed)
  };
}

// version of slice that doesn't allocate extra memory
template <typename S>
void slice_(const RandomSequence<S> &source, const std::size_t I, const std::size_t N, RandomSequence<S> &target) {
  auto strategy_start_index = matching_strategy_index(I, source);
  auto strategy_end_index = matching_strategy_index(I + N, source);

  target.store.assign(source.store.cbegin() + I, source.store.cbegin() + I + N);
  target.reversed.assign(source.reversed.cbegin() + I, source.reversed.cbegin() + I + N);
  target.strategies.store.assign(source.strategies.store.cbegin() + strategy_start_index, source.strategies.store.cbegin() + strategy_end_index);
}


template <typename S>
auto reverse_(const RandomSequence<S>& seq) {
  RandomSequence<S> new_seq{};
  for (int i = seq.Size() - 1; i >= 0; --i) {
    new_seq.store.push_back(seq.store.at(i));
    new_seq.reversed.push_back(!seq.reversed.at(i));
  }
  for (int i = seq.strategies.Size() - 1; i >= 0; --i) {
    new_seq.strategies.Append(seq.strategies.store.at(i));
  }
  return new_seq;
}

// version of reverse that doesn't allocate extra memory
template <typename S>
void reverse_(const RandomSequence<S> &source, RandomSequence<S> &target) {
  target.store.assign(source.store.crbegin(), source.store.crend());
  target.reversed.assign(source.reversed.crbegin(), source.reversed.crend());
  target.strategies.store.assign(source.strategies.store.crbegin(), source.strategies.store.crend());

  for (int i = 0; i < target.Size(); i++) {
    target.reversed[i] = !target.reversed[i];
  }
}



template <typename S>
auto RandomSequence<S>::Concat(const RandomSequence<S>& other) const {
  RandomSequence<S> seq{};
  for (const auto& element : store) {
    seq.store.push_back(element);
  }

  for (const auto& strategy : strategies.store) {
    seq.strategies.Append(strategy);
  }

  for (bool r : reversed) {
    seq.reversed.push_back(r);
  }

  for (const auto& element : other.store) {
    seq.store.push_back(element);
  }

  for (const auto& strategy : other.strategies.store) {
    seq.strategies.Append(strategy);
  }

  for (bool r : other.reversed) {
    seq.reversed.push_back(r);
  }

  return seq;
}

// version of concat that doesn't allocate extra memory
template <typename S>
void concat_(const RandomSequence<S> &source1, const RandomSequence<S> &source2, RandomSequence<S> &target) {
  target.store.assign(source1.store.cbegin(), source1.store.cend());
  target.reversed.assign(source1.reversed.cbegin(), source1.reversed.cend());
  target.strategies.store.assign(source1.strategies.store.cbegin(), source1.strategies.store.cend());

  target.store.insert(target.store.end(), source2.store.cbegin(), source2.store.cend());
  target.reversed.insert(target.reversed.end(), source2.reversed.cbegin(), source2.reversed.cend());
  target.strategies.store.insert(target.strategies.store.end(), source2.strategies.store.cbegin(), source2.strategies.store.cend());
}

// reset the random sequence
template <typename S>
void clear(RandomSequence<S> &sequence) {
  sequence.store.clear();
  sequence.reversed.clear();
  sequence.strategies.store.clear();
}

// version of cast_to_constant that doesn't allocate extra memory
template <typename S>
void cast_to_constant(const RandomSequence<S> &input, RandomSequence<S> &output) {
  output.store.assign(input.store.cbegin(), input.store.cend());
  output.reversed.assign(input.reversed.cbegin(), input.reversed.cend());
  output.strategies.store.assign(input.strategies.store.cbegin(), input.strategies.store.cend());

  for (int i = 0, N = output.store.size(); i < N; ++i) {
    using pdf_fn_t = std::function<Real(const RandomSequence<S>&, const std::vector<boost::optional<S>>&, int, bool, SourceDistributionHelper&)>;

    auto expected_result = output.store.at(i).get();
    pdf_fn_t pdf_fn = [=](const RandomSequence<S>& seq, const std::vector<boost::optional<S>>& query_samples, int I, bool reversed, SourceDistributionHelper&) {
      if (expected_result.rv.get() == query_samples[0].get()) {
        return Real(1);
      }
      return Real(0);
    };

    output.strategies[i] = std::make_shared<Strategy<S>>(*output.strategies[i]);
    output.strategies[i]->pdf_fn = pdf_fn;
  }
}

template <typename T, typename D, bool ComputedB, typename B, typename MapB, typename DataB>
Real Pdf(const Element<T, D>& sample, random_var<ComputedB, B, MapB, DataB> distribution_rv) {
  return Pdf(make_random_var(sample), distribution_rv);
}



template <typename T, typename D, bool ComputedB, typename B, typename MapB, typename DataB>
Real Pdf2(const Element<T, D>& sample, random_var<ComputedB, B, MapB, DataB> distribution_rv) {
  return Pdf(make_random_var(sample), distribution_rv);
}

template <typename T, bool ComputedB, typename B, typename MapB, typename DataB>
Real Pdf(const SequenceElement<T>& sample, random_var<ComputedB, B, MapB, DataB> distribution_rv) {
  return Pdf(sample.Value(), distribution_rv);
}
  
template <typename T>
Real Pdf(const RandomSequence<T>& sample_seq, const RandomSequence<T>& distribution_seq) {
  if (sample_seq.Size() != distribution_seq.Size()) {
    return 0;
  }
  return distribution_seq.Pdf(sample_seq);
}

template <typename T>
Real Pdf(const RandomSequence<T>& sample_seq, const RandomSequence<T>& distribution_seq, SourceDistributionHelper &source_distribution_helper) {
  if (sample_seq.Size() != distribution_seq.Size()) {
    return 0;
  }
  return distribution_seq.Pdf(sample_seq, source_distribution_helper);
}

template <typename S>
void RandomSequence<S>::Sample(std::size_t strategy_index, std::size_t sample_index) {
  if (sample_index >= store.size() || store[sample_index]) {
    return;
  }

  if (reversed[sample_index]) {
    Sample(strategy_index + 1, sample_index + strategies[strategy_index]->output_size);
  }

  auto samples = strategies[strategy_index]->Sample(reversed[sample_index] ? reverse_(slice_(sample_index + 1, *this)) : slice_(0, sample_index, *this));
  for (std::size_t j = 0, N = samples.size(); j < N; ++j) {
    store[sample_index + j] = samples[j];
  }
}

template <typename S>
void RandomSequence<S>::Sample() {
  std::size_t strategy_index = 0;
  std::size_t sample_index = 0;
  std::size_t N = store.size();

  for (; sample_index < N; sample_index += strategies[strategy_index]->output_size, ++strategy_index) {
    Sample(strategy_index, sample_index);
  }
}

template <typename S>
template <typename T, typename... Args>
void RandomSequence<S>::Append(Node<T>& node, Args&&... args) {
  strategies.Append(node, std::forward<Args>(args)...);

  auto output_size = strategies.Back()->output_size;

  using result_t = boost::optional<SequenceElement<S>>;
  for (std::size_t i = 0; i < output_size; ++i) {
    store.push_back(result_t{});
    reversed.push_back(false);
  }
}

template <typename S>
void RandomSequence<S>::Append(const Strategy<S>& strategy) {
  strategies.Append(std::make_shared<Strategy<S>>(strategy));

  auto output_size = strategies.Back()->output_size;

  using result_t = boost::optional<SequenceElement<S>>;
  for (std::size_t i = 0; i < output_size; ++i) {
    store.push_back(result_t{});
    reversed.push_back(false);
  }
}

template <typename S>
void RandomSequence<S>::Append(std::shared_ptr<Strategy<S>> strategy) {
  strategies.Append(std::move(strategy));
  auto output_size = strategies.Back()->output_size;
  using result_t = boost::optional<SequenceElement<S>>;
  for (std::size_t i = 0; i < output_size; ++i) {
    store.push_back(result_t{});
    reversed.push_back(false);
  }
}

template <typename S>
template <typename E, typename Tuple, typename Data>
void RandomSequence<S>::Append(random_var<true, E, Tuple, Data> rv) {
  strategies.Append(rv);
  auto output_size = strategies.Back()->output_size;
  using result_t = boost::optional<SequenceElement<S>>;
  for (std::size_t i = 0; i < output_size; ++i) {
    store.push_back(result_t{});
    reversed.push_back(false);
  }
}

template <typename S>
void RandomSequence<S>::RemoveBack() {
  store.pop_back();
  strategies.RemoveBack();
  reversed.pop_back();
}


template <typename S>
template <typename T, typename... Args>
void StrategySequence<S>::Append(Node<T>& node, Args&&... args) {
  store.push_back(std::make_shared<Strategy<S>>(node, std::forward<Args>(args)...));
}

template <typename S>
template <typename E, typename Tuple, typename Data>
void StrategySequence<S>::Append(const random_var<true, E, Tuple, Data>& rv) {
  store.push_back(std::make_shared<Strategy<S>>(rv));
}

template <typename S>
void StrategySequence<S>::Append(std::shared_ptr<strategy_t> strategy) {
  store.push_back(std::move(strategy));
}

template <typename S>
void StrategySequence<S>::RemoveBack() {
  store.pop_back();
}

template <typename S>
const auto& StrategySequence<S>::operator[](std::size_t i) const {
  return store[i];
}

template <typename S>
auto& StrategySequence<S>::operator[](std::size_t i) {
  return store[i];
}

template <typename S>
const auto& StrategySequence<S>::Back() const {
  return store.back();
}

template <typename S>
auto& StrategySequence<S>::Back() {
  return store.back();
}

template <typename S>
std::size_t StrategySequence<S>::Size() const {
  return store.size();
}

template <typename S>
auto StrategySequence<S>::begin() const {
  return store.begin();
}

template <typename S>
auto StrategySequence<S>::end() const {
  return store.end();
}

template <typename S>
StrategySequence<S> StrategySequence<S>::Slice(std::size_t I, std::size_t N) const {
  return {slice_impl(I, N, store)}; 
}

template <typename Store>
template <typename Fn>
void RandomSequence<Store>::Append(std::size_t I, const Fn& fn) {
  for (std::size_t i = 0; i < I; ++i) {
    Append(fn);
  }
}

template <typename Valid, typename Value, typename... Ds>
constexpr auto optional_sample(Valid&& valid, Value&& value, Ds&&... data) {
  return make_optional_sample_(
    std::forward<Valid>(valid)
    , std::forward<Value>(value)
    , std::forward<Ds>(data)...
  );
}

template <typename Value, typename... Ds>
constexpr auto sample(Value&& value, Ds&&... data) {
  return make_sample_(std::forward<Value>(value), std::forward<Ds>(data)...);
}



template <typename Store>
template <typename T, typename... Args>
RandomSequence<Store> RandomSequence<Store>::Mutate(Node<T>& node, Args&&... args) const {
  auto seq = node.Sample(*this, std::forward<Args>(args)...);
  seq.Sample();
  return seq;
}




} // end namespace aether

#endif
