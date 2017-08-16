#ifndef AETHER_PRINT_H
#define AETHER_PRINT_H

#include <cstdio>
#include <string>
#include <deque>

#include "Add.h"
#include "Literal.h"
#include "Pow.h"
#include "Product.h"
#include "Trig.h"
#include "Variable.h"
#include "Vector.h"

namespace aether {

constexpr char u_phi[] = "\u03C6";
constexpr char u_sqrt[] = "\u221A";
constexpr char u_pi[] = "\u03C0";
constexpr char u_mu[] = "\u03BC";
constexpr char u_xi[] = "\u03BE";
constexpr char u_omega[] = "\u03C9";
constexpr char u_theta[] = "\u03B8";
constexpr char u_delta[] = "\u03B4";
constexpr char u_top_half_integral[] = "\u2320";
constexpr char u_integral[] = "\u222B";
constexpr char u_cross[] = "\u00D7";
constexpr char u_large_dot[] = "\u22C5";
constexpr char u_norm[] = "\u2016";

//static bool print_debug = true;
static bool print_debug = false;


//static bool pair_debug = true;
static bool pair_debug = false;

inline void pad_to_length(std::deque<std::string>& output, std::deque<std::size_t>& lengths, int index, std::size_t length) {
  while (lengths[index] < length) {
    output[index].push_back(' ');
    ++lengths[index];
  }
}

inline void pad_all_to_equal_length(std::deque<std::string>& output, std::deque<std::size_t>& lengths) {
  std::size_t max_length = *std::max_element(lengths.begin(), lengths.end());

  for (std::size_t i = 0; i < output.size(); ++i) {
    pad_to_length(output, lengths, i, max_length);
  }
}

struct output {
  output() : output(0) {}

  output(std::size_t o) : joinline(o) {
    append();
  }

  void prepend() {
    strs.push_front("");
    lengths.push_front(0);
  }

  void append() {
    strs.push_back("");
    lengths.push_back(0);
  }

  void append(std::size_t i, const std::string& str, std::size_t length) {
    strs[i] += str;
    lengths[i] += length;
  }

  void append(const std::string& str, std::size_t length) {
    append(joinline, str, length);
  }

  void append(const std::string& str) {
    append(str, str.size());
  }

  void operator+=(const std::string& str) {
    output other;
    other.append(joinline, str, str.size());
    operator+=(other);
  }

  int lines_above() const {
    return joinline;
  }

  int lines_below() const {
    return strs.size() - joinline - 1;
  }

  void append_from_baseline(const output& other) {
    int rows_to_prepend = other.strs.size() - lines_above();
    for (int i = 0; i < rows_to_prepend; ++i) {
      prepend();
      ++joinline;
    }

    pad_all_to_equal_length(strs, lengths);

    for (std::size_t i = joinline - other.strs.size(), j = 0; j < other.strs.size(); ++i, ++j) {
      strs[i] += other.strs[j];
      lengths[i] += other.lengths[j];
    }

    pad_all_to_equal_length(strs, lengths);
  }

  void operator+=(const output& other) {
    int rows_to_prepend = other.lines_above() - lines_above();
    for (int i = 0; i < rows_to_prepend; ++i) {
      prepend();
      ++joinline;
    }

    int rows_to_append = other.lines_below() - lines_below();
    for (int i = 0; i < rows_to_append; ++i) {
      append();
    }

    pad_all_to_equal_length(strs, lengths);

    for (std::size_t i = joinline - other.lines_above(), j = 0; j < other.strs.size(); ++i, ++j) {
      strs[i] += other.strs[j];
      lengths[i] += other.lengths[j];
    }

    pad_all_to_equal_length(strs, lengths);
  }

  std::deque<std::string> strs;
  std::deque<std::size_t> lengths;
  std::size_t joinline;
};

inline void append_opening_norm(output& o, int level) {
  bool pair_debug = false;
  //bool pair_debug = true;
  if (pair_debug) {
    o.append(std::to_string(level) + "||", 2 + std::to_string(level).size());
  } else {
    o.append("||");
  }
}

inline void append_closing_norm(output& o, int level) {
  bool pair_debug = false;
  //bool pair_debug = true;
  if (pair_debug) {
    o.append("||" + std::to_string(level), 2 + std::to_string(level).size());
  } else {
    o.append("||");
  }
}

inline void append_opening_bracket(output& o, int level) {
  bool pair_debug = false;
  //bool pair_debug = true;
  if (pair_debug) {
    o.append(std::to_string(level) + "[", 1 + std::to_string(level).size());
  } else {
    o.append("[");
  }
}

inline void append_closing_bracket(output& o, int level) {
  bool pair_debug = false;
  //bool pair_debug = true;
  if (pair_debug) {
    o.append("]" + std::to_string(level), 1 + std::to_string(level).size());
  } else {
    o.append("]");
  }
}

inline void append_opening_parenthesis(output& o, int level) {
  bool pair_debug = false;
  //bool pair_debug = true;
  if (pair_debug) {
    o.append(std::to_string(level) + "(", 1 + std::to_string(level).size());
  } else {
    o.append("(");
  }
}

inline void append_closing_parenthesis(output& o, int level) {
  bool pair_debug = false;
  //bool pair_debug = true;
  if (pair_debug) {
    o.append(")" + std::to_string(level), 1 + std::to_string(level).size());
  } else {
    o.append(")");
  }
}

inline void append_opening_brace(output& o, int level) {
  bool pair_debug = false;
  //bool pair_debug = true;
  if (pair_debug) {
    o.append(std::to_string(level) + "{", 1 + std::to_string(level).size());
  } else {
    o.append("{");
  }
}

inline void append_closing_brace(output& o, int level) {
  bool pair_debug = false;
  //bool pair_debug = true;
  if (pair_debug) {
    o.append("}" + std::to_string(level), 1 + std::to_string(level).size());
  } else {
    o.append("}");
  }
}

template <int ID>
inline output pr(variable<ID>, int&) {
  output o{};
  auto s = std::to_string(ID);
  o.append(0, "var" + s, 3 + s.length());
  return o;
}

inline output pr(theta_t, int&) {
  output o{};
  o.append(0, std::string(u_theta), 1);
  return o;
}

inline output pr(phi_t, int&) {
  output o{};
  o.append(0, std::string(u_phi), 1);
  return o;
}

inline output pr(radius_t, int&) {
  output o{};
  o.append(0, "r", 1);
  return o;
}

inline output pr(u_t, int&) {
  output o{};
  o.append(0, "u", 1);
  return o;
}

inline output pr(v_t, int&) {
  output o{};
  o.append(0, "v", 1);
  return o;
}

inline output pr(inverse_variable<0>, int&) {
  output o{};
  o.append(0, "x", 1);
  return o;
}

inline output pr(inverse_variable<1>, int&) {
  output o{};
  o.append(0, "y", 1);
  return o;
}

inline output pr(inverse_variable<2>, int&) {
  output o{};
  o.append(0, "z", 1);
  return o;
}

template <int ID>
inline output pr(inverse_variable<ID>, int&) {
  output o{};
  auto s = std::to_string(ID);
  o.append(0, "iv" + std::to_string(ID), 2 + s.length());
  return o;
}

template <int ID, typename T>
inline output pr(subexpr<ID, T>, int& level) {
  output o{};
  auto s = std::to_string(ID);
  o.append(0, "se" + std::to_string(ID), 2 + s.length());
  if (print_debug) {
    int l = level;
    ++level;
    append_opening_bracket(o, l);
    o += pr(T{}, level);
    append_closing_bracket(o, l);
  }
  return o;
}


template <int ID, std::size_t N, std::size_t I>
inline output pr(param<ID, N, I>, int&) {
  output o{};
  auto id = std::to_string(ID);
  auto i = std::to_string(I);
  o.append(0, "p" + id + i, 1 + id.length() + i.length());
  return o;
}

inline output pr(literal<491701844, 78256779>, int&) {
  output o{};

  o.append(0, "2" + std::string(u_pi), 2);
  return o;
}

inline output pr(literal<245850922, 78256779>, int&) {
  output o{};

  o.append(0, u_pi, 1);
  return o;
}

template <std::intmax_t N>
inline output pr(literal<N, 1>, int&) {
  output o{};

  auto numerator_len = std::to_string(N).size();
  o.append(0, std::to_string(N), numerator_len);
  return o;
}

template <std::intmax_t N, std::intmax_t D>
inline output pr(literal<N, D>, int&) {
  output o(1);
  o.append();
  o.append();

  auto numerator_len = std::to_string(N).size();
  auto denom_len = std::to_string(D).size();

  o.append(0, std::to_string(N), numerator_len);
  for (int i = 0, n = std::max(numerator_len, denom_len); i < n; ++i) {
    o.append(1, "-", 1);
  }
  o.append(2, std::to_string(D), denom_len);
  pad_all_to_equal_length(o.strs, o.lengths);
  return o;
}

inline output pr_list(TypeList<>, const std::string&, int&) {
  return output{};
}

template <typename T>
inline output pr_list(TypeList<T>, const std::string&, int& level) {
  return pr(T{}, level);
}

template <typename A, typename B, typename... Ts>
inline output pr_list(TypeList<A, B, Ts...>, const std::string& op, int& level) {
  output o = pr(A{}, level);
  o.append(" " + op + " ", 3);
  o += pr_list(TypeList<B, Ts...>{}, op, level);
  return o;
}

inline std::string condition_op_to_string(logical_or_tag_t) {
  return "||";
}

inline std::string condition_op_to_string(logical_and_tag_t) {
  return "&&";
}

inline std::string condition_op_to_string(less_than_tag_t) {
  return "<";
}

inline std::string condition_op_to_string(less_than_equal_tag_t) {
  return "<=";
}

inline std::string condition_op_to_string(greater_than_tag_t) {
  return ">";
}

inline std::string condition_op_to_string(greater_than_equal_tag_t) {
  return ">=";
}

inline std::string condition_op_to_string(equal_tag_t) {
  return "==";
}

inline std::string condition_op_to_string(not_equal_tag_t) {
  return "!=";
}

inline std::string condition_op_to_string(logical_not_tag_t) {
  return "!";
}

template <typename T>
inline output pr(vectorlength<T>, int& level) {
  output o{};
  int l = level;
  ++level;
  append_opening_norm(o, l);
  o += pr(T{}, level);
  append_closing_norm(o, l);
  return o;
}

template <typename C, typename V>
inline output pr(branchcaseexpr<C, V>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append("else if ");
  o += pr_list(TypeList<C, V>{}, ":", level);
  return o;
}

template <typename V>
inline output pr(branchcaseexpr<trueexpr, V>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append(" else ");
  o += pr(V{}, level);
  return o;
}

template <typename C, typename V, typename... Ts>
inline output pr(branchexpr<branchcaseexpr<C, V>, Ts...>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append("if ");
  o += pr_list(TypeList<C, V>{}, ":", level);
  o += pr_list(TypeList<Ts...>{}, " ", level);
  return o;
}


inline output pr(trueexpr, int& level) {
  output o{};
  int l = level;
  ++level;
  append_opening_bracket(o, l);
  o.append("t", 1);
  append_closing_bracket(o, l);
  return o;
}

inline output pr_impl(TypeList<>, TypeList<>, int& level) {
  output o{};
  return o;
}

template <typename C, typename V>
inline output pr_impl(TypeList<C>, TypeList<V>, int& level) {
  output o{};
  int l = level;
  ++level;
  o += pr_list(TypeList<C, V>{}, std::string(u_cross), level);
  return o;
}

template <typename C1, typename C2, typename... Cs, typename V1, typename V2, typename... Vs>
inline output pr_impl(TypeList<C1, C2, Cs...>, TypeList<V1, V2, Vs...>, int& level) {
  output o{};
  int l = level;
  ++level;
  o += pr_list(TypeList<C1, V1>{}, std::string(u_cross), level);
  o.append(" + ", 3);
  o += pr_impl(TypeList<C2, Cs...>{}, TypeList<V2, Vs...>{}, level);
  return o;
}


template <typename... Ts>
inline output pr(productexpr<Ts...>, int& level) {
  output o;
  o += pr_list(TypeList<Ts...>{}, std::string(u_large_dot), level);
  return o;
}

template <typename... Ts>
inline output pr(addexpr<Ts...>, int& level) {
  output o;
  int l = level;
  ++level;
  append_opening_parenthesis(o, l);
  o += pr_list(TypeList<Ts...>{}, std::string("+"), level);
  append_closing_parenthesis(o, l);
  return o;
}

template <typename... Ts>
inline output pr(matrixexpr<Ts...>, int& level) {
  output o{};
  int l = level;
  ++level;
  append_opening_brace(o, l);
  o += pr_list(TypeList<Ts...>{}, std::string(","), level);
  append_closing_brace(o, l);
  return o;
}

template <typename C, typename... Ts>
inline output pr(vectorexpr<C, Ts...>, int& level) {
  output o{};
  int l = level;
  ++level;
  append_opening_brace(o, l);
  o += pr_list(TypeList<Ts...>{}, std::string(","), level);
  append_closing_brace(o, l);
  return o;
}

template <typename A, typename B>
inline output pr(dotproduct<A, B>, int& level) {
  output o{};
  o += pr_list(TypeList<A, B>{}, std::string(u_large_dot), level);
  return o;
}

template <typename A, typename B>
inline output pr(crossproduct<A, B>, int& level) {
  output o{};
  o += pr_list(TypeList<A, B>{}, std::string(u_cross), level);
  return o;
}

template <typename T, typename E>
inline output pr(powerexpr<T, E>, int& level) {
  output o;
  int l = level;
  if (size(T{}) >= 2) {
    ++level;
    append_opening_parenthesis(o, l);
  }
  o += pr(T{}, level);
  if (size(T{}) >= 2) {
    append_closing_parenthesis(o, l);
  }
  o.append_from_baseline(pr(E{}, level));
  return o;
}

template <typename T>
inline output pr(sinexpr<T>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append("sin");
  append_opening_parenthesis(o, l);
  o += pr(T{}, level);
  append_closing_parenthesis(o, l);
  return o;
}

template <typename T>
inline output pr(arcsinexpr<T>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append("arcsin");
  append_opening_parenthesis(o, l);
  o += pr(T{}, level);
  append_closing_parenthesis(o, l);
  return o;
}

template <typename T>
inline output pr(cosexpr<T>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append("cos");
  append_opening_parenthesis(o, l);
  o += pr(T{}, level);
  append_closing_parenthesis(o, l);
  return o;
}

template <typename T>
inline output pr(arccosexpr<T>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append("arccos");
  append_opening_parenthesis(o, l);
  o += pr(T{}, level);
  append_closing_parenthesis(o, l);
  return o;
}

template <typename T>
inline output pr(tanexpr<T>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append("tan");
  append_opening_parenthesis(o, l);
  o += pr(T{}, level);
  append_closing_parenthesis(o, l);
  return o;
}

template <typename T>
inline output pr(arctanexpr<T>, int& level) {
  output o{};
  int l = level;
  ++level;
  o.append("arctan");
  append_opening_parenthesis(o, l);
  o += pr(T{}, level);
  append_closing_parenthesis(o, l);
  return o;
}


struct print_t {

  template <typename E, typename Table, typename Tuple>
  void operator()(random_var<E, Table, Tuple>) const {
    using consume = int[];
    (void)consume{1, (this->operator()(E{}), printf("\n"), 1)};
  }

  template <typename... Ts>
  void operator()(TypeList<Ts...>) const {
    using consume = int[];
    (void)consume{1, (this->operator()(Ts{}), printf("\n"), 1)...};
  }

  template <typename Tag, typename... Ts>
  void operator()(expr<Tag, Ts...> n) const {
    int level = 0;
    output o = pr(n, level);

    for (auto s : o.strs) {
      printf("%s\n", s.c_str());
    }
  }
};

constexpr print_t print{};


} // end namespace aether


#endif
