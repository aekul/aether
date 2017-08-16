#ifndef AETHER_CONTEXT_PDF_CONTEXT_H
#define AETHER_CONTEXT_PDF_CONTEXT_H

#include "aether/Context.h"
#include "aether/RandomVar.h"
#include "aether/DynamicRandomVar.h"
#include "aether/Strategy.h"

namespace aether {

struct DiscreteChoice {
  template <typename T>
  DiscreteChoice(const discrete_random_var_dynamic<T>& rv)
    : index{0}
    , length{rv.Size()}
  {}

  bool IsValid() const {
    return index < length;
  }

  void Next() {
    ++index;
  }

  std::size_t index;
  std::size_t length;
};

struct PdfContext : Context<PdfContext> {
  Real trace_pdf{1};
  std::vector<DiscreteChoice> dc_stack{};
  std::size_t choice_index{0};
  std::vector<const Distribution2D*> dist2Ds;
  int targetSize;

  PdfContext(const int targetSize) : targetSize(targetSize) {}

  void Update() {
    while (HasMoreChoices()) {
      dc_stack.back().Next();

      if (dc_stack.back().IsValid()) {
        break;
      }

      dc_stack.pop_back();
    }

    Reset();
  }

  void Next() {
    ++choice_index;
  }

  void Reset() {
    choice_index = 0;
    trace_pdf = 1;
    dist2Ds.clear();
  }

  bool HasMoreChoices() const {
    return !dc_stack.empty();
  }

  const DiscreteChoice& Current() const {
    return dc_stack.at(choice_index);
  }

  template <typename T>
  void AddDiscreteChoice(const discrete_random_var_dynamic<T>& rv) {
    dc_stack.emplace_back(rv);
  }

  bool IsValid() const {
    return choice_index < dc_stack.size();
  }

  template <typename Fn, typename... Args>
  constexpr auto constant_call(Fn&& fn, Args&&... args);

  template <typename T, typename... Args>
  auto Sample(const discrete_random_var_dynamic<T>& rv, Args&&... args);

  template <typename Sampler>
  Real Uniform1D(Sampler& sampler);

  template <typename Sampler>
  std::array<Real, 2> Uniform2D(Sampler& sampler);

  template <typename Sampler>
  std::array<Real, 2> PiecewiseConstant2D(Sampler& sampler, const Distribution2D *dist2D);

  inline bool MatchTargetSize(const std::size_t size);
};

inline PdfContext make_pdf_context(const int targetSize = 0) {
  return PdfContext(targetSize);
}

template <typename Fn, typename... Args>
constexpr auto PdfContext::constant_call(Fn&& fn, Args&&... args) {
  using result_t = decltype(std::forward<Fn>(fn)(std::forward<Args>(args)...)); 
  return result_t{};
}

template <typename Sampler>
Real PdfContext::Uniform1D(Sampler& sampler) {
  return 0;
}

template <typename Sampler>
std::array<Real, 2> PdfContext::Uniform2D(Sampler& sampler) {
  return std::array<Real, 2>{{0.0, 0.0}};
}

template <typename Sampler>
std::array<Real, 2> PdfContext::PiecewiseConstant2D(Sampler& sampler, const Distribution2D *dist2D) {
  dist2Ds.push_back(dist2D);
  Argument arg1, arg2;
  arg1.record.isRecord = arg2.record.isRecord = true;
  arg1.record.distID = arg2.record.distID = dist2Ds.size() - 1;
  arg1.record.maxDimension = arg2.record.maxDimension = 2;
  arg1.record.dimension = 0; arg2.record.dimension = 1;
  return std::array<Real, 2>{{arg1.value, arg2.value}};
}

template <typename T, typename... Args>
auto PdfContext::Sample(const discrete_random_var_dynamic<T>& rv, Args&&... args) {
  if (!IsValid()) {
    AddDiscreteChoice(rv);
  }

  const DiscreteChoice& dc = Current();
  auto value = rv.Value(dc.index);
  trace_pdf *= rv.Pdf(value);
  Next();
  return value;
}

inline bool PdfContext::MatchTargetSize(const std::size_t size) {
  return size == targetSize;
}


template <typename T>
float Pdf(const T& actual_value, const T& expected_value) {
  return actual_value == expected_value ? 1.f : 0.f;
}


template <typename T>
struct pdf_traits;

template <bool Computed, typename E, typename Map, typename Data>
struct pdf_traits<random_var<Computed, E, Map, Data>> {
  using data_keys_t = get_data_keys_t<random_var<Computed, E, Map, Data>>;
};


//template <typename Sample, bool Computed, typename E, typename Map, typename Data>
//float Pdf(const std::vector<Sample>& samples, random_var<Computed, E, Map, Data> distribution_rv) {
  //constexpr auto named_param_keys_a = typename pdf_traits<Sample>::data_keys_t{};
  //constexpr auto named_param_keys_b = get_named_param_names(named_params(E{}));

  //static_assert(named_param_keys_b.Difference(named_param_keys_a).Size() == 0, "Pdf(): Named param missing from sample");
  
  //auto np = named_params(distribution_rv);
  //auto named_param_values = get_data_as_map(np, samples);


  ////Ty<decltype(named_params(E{}))> s{};

  ////static_assert(named_param_keys_b.Difference(named_param_keys_a).Size() == 0, "Pdf(): Named param missing from sample");

  ////auto np = named_params(distribution_rv);
  ////auto named_param_values = get_data_as_map(np, sample);
  ////copy(distribution_rv.values, named_param_values);
  ////return distribution_rv.Pdf(sample.Value());
//}

template <typename Sample, bool Computed, typename E, typename Map, typename Data>
Real Pdf(const Sample& sample,
         random_var<Computed, E, Map, Data> distribution_rv,
         SourceDistributionHelper &source_distribution_helper) {
  constexpr auto named_param_keys_a = typename pdf_traits<Sample>::data_keys_t{};
  constexpr auto named_param_keys_b = get_named_param_names(named_params(E{}));

  static_assert(named_param_keys_b.Difference(named_param_keys_a).Size() == 0, "Pdf(): Named param missing from sample");

  auto np = named_params(distribution_rv);
  auto named_param_values = get_data_as_map(np, sample);
  copy(distribution_rv.values, named_param_values);
  return distribution_rv.Pdf(get_sample_value<output_dimensions(E{})>(sample), source_distribution_helper);
}

template <typename Sample, bool Computed, typename E, typename Map, typename Data>
Real Pdf(const boost::optional<Sample>& sample,
         const random_var<Computed, E, Map, Data>& distribution_rv,
         SourceDistributionHelper &source_distribution_helper) {
  if (!sample) {
    return 0;
  }

  return Pdf(sample.get(), distribution_rv, source_distribution_helper);
}

template <bool ComputedB, typename B, typename MapB, typename DataB>
Real Pdf(const typename random_var<ComputedB, B, MapB, DataB>::result_t& sample,
         random_var<ComputedB, B, MapB, DataB> distribution_rv,
         SourceDistributionHelper &source_distribution_helper) {
  return distribution_rv.Pdf(sample, source_distribution_helper);
}

template <typename Sample, bool Computed, typename T, typename Map, typename Data>
Real Pdf(const Sample& sample,
         const boost::optional<random_var<Computed, T, Map, Data>>& distribution_rv,
         SourceDistributionHelper &source_distribution_helper) {
  if (!distribution_rv) {
    assert(false);
    return 0;
  }

  return Pdf(sample, distribution_rv.get(), source_distribution_helper);
}

template <typename Sample, typename... Ts, std::size_t... Is>
float Pdf(const std::vector<Sample>& samples, const hana::tuple<Ts...>& distribution_rvs, std::index_sequence<Is...>, SourceDistributionHelper &source_distribution_helper) {
  return Pdf(samples, make_random_vector(hana::at_c<Is>(distribution_rvs)...), source_distribution_helper);
}

template <typename Sample, typename... Ts>
float Pdf(const std::vector<Sample>& samples, const hana::tuple<Ts...>& distribution_rvs, SourceDistributionHelper &source_distribution_helper) {
  return Pdf(samples, distribution_rvs, std::index_sequence_for<Ts...>{}, source_distribution_helper);
}

template <typename Sample, typename... Ts>
float Pdf(const std::vector<boost::optional<Sample>>& optional_samples, const hana::tuple<Ts...>& distribution_rvs, SourceDistributionHelper &source_distribution_helper) {
  for (const auto& sample : optional_samples) {
    if (!sample) {
      return 0;
    }
  }

  std::vector<Sample> samples;
  for (const auto& sample : optional_samples) {
    samples.push_back(sample.get());
  }
  return Pdf(samples, distribution_rvs, std::index_sequence_for<Ts...>{}, source_distribution_helper);
}

template <typename QuerySample, typename Distribution>
float Pdf_(const std::vector<boost::optional<QuerySample>>& optional_query_samples, const Distribution& distribution, SourceDistributionHelper &source_distribution_helper) {
  for (const auto& sample : optional_query_samples) {
    if (!sample) {
      return 0;
    }
  }

  std::vector<QuerySample> query_samples;
  for (const auto& sample : optional_query_samples) {
    query_samples.push_back(sample.get());
  }
  return Pdf(query_samples, distribution, source_distribution_helper);
}

template <typename QuerySample, typename Distribution>
Real Pdf_(const QuerySample& query_sample, const Distribution& distribution, SourceDistributionHelper &source_distribution_helper) {
  return Pdf(query_sample, distribution, source_distribution_helper);
}

template <typename Input, typename Output, typename T>
Real Pdf_(Input& input, const Output& output, const RandomSequence<T>& input_seq) {
  return Pdf_(input, output, input_seq);
}

// Probably some kind of random_var thing
// target size is only meaningful in RandomSequence, so just randomly return something
template <typename T>
std::size_t get_target_size(const T &target) {
  return 1;
}

template <typename T>
std::size_t get_target_size(const RandomSequence<T> &target) {
  return target.Size();
}

template <typename A, typename Output, typename T, typename... Args>
Real Pdf_(Node<A>& node, const Output& output, const RandomSequence<T>& input_seq,
            SourceDistributionHelper &source_distribution_helper, Args&&... args) {
  auto context = make_pdf_context(get_target_size(output));
  Real pdf{0};

  while (true) {
    auto distribution = node(context, input_seq, std::forward<Args>(args)...);
    source_distribution_helper.reset(&context.dist2Ds);
    pdf += context.trace_pdf * Pdf_(output, distribution, source_distribution_helper);

    context.Update();
    if (!context.HasMoreChoices()) {
      break;
    }
  }

  return pdf;
}

template <typename A, typename T, typename... Args>
Real Pdf_(Node<A>& node, const RandomSequence<T>& output_seq, const RandomSequence<T>& input_seq,
            const std::vector<int> &shared_rv, SourceDistributionHelper &source_distribution_helper, Args&&... args) {
  auto context = make_pdf_context(get_target_size(output_seq));
  Real pdf{0};

  while (true) {
    RandomSequence<T> distribution = node(context, input_seq, std::forward<Args>(args)...);
    source_distribution_helper.reset(&context.dist2Ds);
    // Check if distribution retains all the copied r.v. in output_seq
    bool retain = distribution.Size() == output_seq.Size();
    if (retain) {
      for (size_t i = 0; i < input_seq.Size(); i++) {
        if (shared_rv[i] != -1) {
          if (distribution.strategies[shared_rv[i]].get() != input_seq.strategies[i].get()) {
            retain = false;
            break;
          }
        }
      }
    }
    if (retain) {
      pdf += context.trace_pdf * Pdf_(output_seq, distribution, source_distribution_helper);
    }

    context.Update();
    if (!context.HasMoreChoices()) {
      break;
    }
  }

  return pdf;
}

template <typename Mutation, typename T>
Real ConditionalPdf(Node<Mutation>& mutation, const RandomSequence<T>& output_seq, const RandomSequence<T>& input_seq) {
  if (!output_seq.AllValid() || output_seq.IsEmpty()) {
    return Real(0);
  }

  std::vector<int> shared_rv;
  shared_rv.reserve(input_seq.Size());
  // For each r.v. in input_seq, check if there is any r.v. in output_seq that shares the same strategy
  // If so, it means that the output r.v. is copied from input r.v., and this results in a Dirac conditional pdf
  // For Metropolis-Hastings application we want to cancel the Diracs out between the numerator and denominator
  // Therefore we want to keep all the copied r.v..
  // If even one of them is removed in Node's operation, we set the pdf to zero
  for (size_t i = 0; i < input_seq.Size(); i++) {
    for (size_t j = 0; j < output_seq.Size(); j++) {
      if (output_seq.strategies[j].get() == input_seq.strategies[i].get()) {
        shared_rv.push_back(j);
        break;
      }
    }
    if (shared_rv.size() <= i) {
      // If not found we set the index to -1
      shared_rv.push_back(-1);
    }
  }

  SourceDistributionHelper source_distribution_helper;
  return Pdf_(mutation, output_seq, cast_to_constant(input_seq), shared_rv, source_distribution_helper);
}

} // end namespace aether

#endif
