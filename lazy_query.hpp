// Copyright (c) 2020 Nathaniel Shead.

#ifndef LAZYQUERY_H_INCLUDED
#define LAZYQUERY_H_INCLUDED

#include <functional>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <vector>

// Record type in the database
namespace db {
  // just use std::string everywhere for now
  // std::unordered_map doesn't support transparent comparators until C++20
  // TODO: automatically treat `const char[N]` as `std::string`?
  class Record {
    // helper to construct from set of parameters
    template <std::size_t... I, typename... Args>
    void set_from_tuple(std::index_sequence<I...>, std::tuple<Args...>&& args) {
      static_assert(std::conjunction_v<std::is_convertible<
                      decltype(std::get<I * 2>(std::move(args))), std::string>...>,
                    "keys must all be convertible to std::string");

      (set(std::get<I*2>(std::move(args)), std::get<I*2 + 1>(std::move(args))), ...);
    }

  public:
    template <typename... Args, std::enable_if_t<sizeof...(Args) != 1, int> = 0>
    Record(Args&&... args) {
      static_assert(sizeof...(Args) % 2 == 0, "argument count must be even");
      data.reserve(sizeof...(Args) / 2);
      set_from_tuple(std::make_index_sequence<sizeof...(Args) / 2>(),
                     std::forward_as_tuple(std::forward<Args>(args)...));
    }

    Record(const Record& other) {
      data.reserve(other.data.size());
      for (auto&& [key, value] : other.data)
        data.try_emplace(key, value->clone());
    }
    Record& operator=(const Record& other) {
      Record(other).data.swap(data);
      return *this;
    }

    Record(Record&& other) noexcept = default;
    Record& operator=(Record&& other) noexcept = default;

    // list existing keys in the record set
    std::vector<std::string> keys() const {
      std::vector<std::string> result;
      result.reserve(data.size());
      for (auto&& [key, value] : data)
        result.emplace_back(key);
      return result;
    }

    // list existing values with given type in record set
    template <typename T>
    std::vector<std::pair<std::string, T>> values() const {
      std::vector<std::pair<std::string, T>> result;
      result.reserve(data.size());
      for (auto&& [key, _] : data) {
        if (auto value = get_ptr<T>(key)) {
          result.emplace_back(key, *value);
        }
      }
      return result;
    }

    // get a copy of the value stored with given key
    // if the key doesn't exist, or the supplied type is wrong, returns nothing
    template <typename T>
    std::optional<T> get(const std::string& key) const {
      if (auto ptr = get_ptr<T>(key))
        return *ptr;
      return std::nullopt;
    }

    // get a pointer to the value stored with given key
    // if the key doesn't exist, or the supplied type is wrong, returns nullptr
    template <typename T>
    const T* get_ptr(const std::string& key) const noexcept {
      if (auto it = data.find(key); it != data.end()) {
        if (auto value = dynamic_cast<Value<T>*>(it->second.get())) {
          return std::addressof(value->data);
        }
      }
      return nullptr;
    }

    // set the value for the given `key` to `val`;
    // returns true if new value inserted, false if value replaced
    template <typename T>
    bool set(const std::string& key, T&& val) {
      using V = Value<std::decay_t<T>>;
      auto r = data.insert_or_assign(key, std::make_unique<V>(std::forward<T>(val)));
      return r.second;
    }

    // print this record
    friend std::ostream& operator<<(std::ostream& os, const Record& r) {
      os << "{";
      auto first = true;
      for (auto&& [key, value] : r.data) {
        if (first) { first = false; os << " "; } else { os << ", "; }
        os << key << ": ";
        value->print(os);
      }
      os << " }";
      return os;
    }

  private:
    struct ValueBase {
      virtual ~ValueBase() = default;
      virtual std::unique_ptr<ValueBase> clone() const = 0;
      virtual void print(std::ostream& os) const = 0;
    };

    template <typename T>
    struct Value : ValueBase {
      T data;
      Value(T t) : data(std::move(t)) {}
      std::unique_ptr<ValueBase> clone() const override {
        return std::make_unique<Value>(*this);
      }
      void print(std::ostream& os) const override {
        os << data;
      }
    };

    std::unordered_map<std::string, std::unique_ptr<ValueBase>> data;
  };
} // namespace db

// some basic traits
namespace db::detail {
  // an operation
  struct OpBase {};
  template <typename T>
  struct is_operation : std::is_base_of<OpBase, std::decay_t<T>> {};
  template <typename T>
  inline constexpr bool is_operation_v = is_operation<T>::value;

  // a value wrapper
  struct ValueBase {};
  template <typename T>
  struct is_value : std::is_base_of<ValueBase, std::decay_t<T>> {};
  template <typename T>
  inline constexpr bool is_value_v = is_value<T>::value;

  // a proxy value
  struct ProxyBase {};
  template <typename T>
  struct is_proxy : std::is_base_of<ProxyBase, std::decay_t<T>> {};
  template <typename T>
  inline constexpr bool is_proxy_v = is_proxy<T>::value;

  // any valid expression category
  template <typename T>
  struct is_valid : std::bool_constant<is_value_v<T> or is_operation_v<T>>{};
  template <typename T>
  inline constexpr bool is_valid_v = is_valid<T>::value;

  // typecheck if we're dealing with a real value
  template <typename T, typename = void>
  struct is_untyped : std::false_type {};
  template <typename T>
  struct is_untyped<T, std::enable_if_t<
    std::is_same_v<typename T::value_type, void>
  >> : std::true_type {};
  template <typename T>
  inline constexpr bool is_untyped_v = is_untyped<T>::value;
}  // namespace db::detail

// abstract proxy type
namespace db::detail {
  // ugly macro madness time
  // but it really simplifies defining all the operator overloads

#define DB_DETAIL_PROXY_RELOP_DECL(op)                                         \
  template <typename U, std::enable_if_t<not is_proxy_v<U>, int> = 0>          \
  friend bool operator op(const Proxy& left, U&& right) {                      \
    return left.go([&](auto&& v) { return v op right; });                      \
  }                                                                            \
  template <typename U, std::enable_if_t<not is_proxy_v<U>, int> = 0>          \
  friend bool operator op(U&& left, const Proxy& right) {                      \
    return right.go([&](auto&& v) { return left op v; });                      \
  }                                                                            \
  template <typename U, typename C, typename P>                                \
  friend bool operator op(const Proxy& left, const Proxy<U, C, P>& right) {    \
    return left.go([&](auto&& l) {                                             \
      return right.go([&](auto&& r) { return l op r; });                       \
    });                                                                        \
  }

#define DB_DETAIL_PROXY_ARITHOP_DECL(op)                                       \
  template <typename U, std::enable_if_t<not is_proxy_v<U>, int> = 0>          \
  friend auto operator op(const Proxy& left, U&& right) {                      \
    return make_proxy<T>(*left.r, left.compare, [left, right](auto&& x) {      \
      return left.proj(x) op right;                                            \
    });                                                                        \
  }                                                                            \
  template <typename U, std::enable_if_t<not is_proxy_v<U>, int> = 0>          \
  friend auto operator op(U&& left, const Proxy& right) {                      \
    return make_proxy<T>(*right.r, right.compare, [left, right](auto&& x) {    \
      return left op right.proj(x);                                            \
    });                                                                        \
  }                                                                            \
  template <typename U, typename C, typename P>                                \
  friend auto operator op(const Proxy& left, const Proxy<U, C, P>& right) {    \
    /* TODO: doesn't work when they're two different kinds of proxy */         \
    return make_proxy<T>(*left.r, left.compare, [left, right](auto&& x) {      \
      return left.proj(x) op right;                                            \
    });                                                                        \
  }

  // a proxy object to handle computed comparisons
  template <typename T, typename Comparison, typename Projection>
  struct Proxy;

  // used to combine both expicit types and type deduction
  template <typename T, typename Comparison, typename Projection>
  auto make_proxy(const Record &r, Comparison&& c, Projection&& p) {
    return Proxy<T, std::decay_t<Comparison>, std::decay_t<Projection>>{
        r, std::forward<Comparison>(c), std::forward<Projection>(p)};
  }

  template <typename T, typename Comparison>
  auto make_proxy(const Record &r, Comparison&& c) {
    auto identity = [](auto&& x) -> decltype(auto) {
      return std::forward<decltype(x)>(x); };
    return make_proxy<T>(r, std::forward<Comparison>(c), identity);
  }

  template <typename T, typename Comparison, typename Projection>
  struct Proxy : private ProxyBase {
    template <typename C, typename P>
    Proxy(const Record& r, C&& compare, P&& proj)
      : r(&r), compare{ std::forward<C>(compare) }, proj{ std::forward<P>(proj) }
    {}

    using value_type = std::decay_t<decltype(
      std::declval<Projection>()(std::declval<T>()))>;

    // pointer to the currently processed record
    const Record* r;

    // function to perform the final filtering of this proxy type; of form
    //   bool (const Record& r, function<bool(auto&& value)> callback);
    // where `r` is `*this->r` and `value` is the contained value in the proxy
    // the callback returns whether `value` compares true
    Comparison compare;

    // project a proxy value into a new type; of form
    //   U (T&& old_value);
    Projection proj;

    template <typename F>
    auto go(F&& f) const {
      return compare(*r, [&](auto&& v) { return std::forward<F>(f)(proj(v)); });
    }

    explicit operator bool() const {
      return compare(*r, proj);
    }

    // binary
    // relations
    DB_DETAIL_PROXY_RELOP_DECL(<)
    DB_DETAIL_PROXY_RELOP_DECL(>)
    DB_DETAIL_PROXY_RELOP_DECL(<=)
    DB_DETAIL_PROXY_RELOP_DECL(>=)
    DB_DETAIL_PROXY_RELOP_DECL(==)
    DB_DETAIL_PROXY_RELOP_DECL(!=)

    // arithmetic
    DB_DETAIL_PROXY_ARITHOP_DECL(+)
    DB_DETAIL_PROXY_ARITHOP_DECL(-)
    DB_DETAIL_PROXY_ARITHOP_DECL(*)
    DB_DETAIL_PROXY_ARITHOP_DECL(/)
    DB_DETAIL_PROXY_ARITHOP_DECL(%)

    // bitwise
    DB_DETAIL_PROXY_ARITHOP_DECL(&)
    DB_DETAIL_PROXY_ARITHOP_DECL(|)
    DB_DETAIL_PROXY_ARITHOP_DECL(^)

    // unary
    // (operator! is covered by operator bool)
    auto operator-() const {
      return make_proxy<T>(*r, compare, [proj=proj](auto&& x) { return -proj(x); });
    }
    auto operator+() const {
      return make_proxy<T>(*r, compare, [proj=proj](auto&& x) { return +proj(x); });
    }
    auto operator~() const {
      return make_proxy<T>(*r, compare, [proj=proj](auto&& x) { return ~proj(x); });
    }
  };

#undef DB_DETAIL_ANY_PROXY_RELOP_DECL
#undef DB_DETAIL_ANY_PROXY_ARITHOP_DECL
} // namespace db::detail

// object and expression wrapper generators
namespace db::detail {
  // a constant value; this is always well typed
  template <typename T>
  struct Constant : private ValueBase {
    using value_type = T;

    template <typename U>
    Constant(U&& data) : data{ std::forward<U>(data) } {}

    T data;

    template <typename U>
    auto get([[maybe_unused]] const Record& r) const {
      static_assert(std::is_same_v<T, U>);
      return &data;
    }
  };

  // the value with given key;
  // if T is `void`, derive type from the context of the expression;
  // if this is not possible, static_assert
  template <typename Value = void>
  struct Key : private ValueBase {
    using value_type = Value;

    Key(std::string key) : key{ std::move(key) } {}
    std::string key;

    template <typename T>
    auto get(const Record& r) const {
      return r.get_ptr<T>(key);
    }
  };

  // tests for the existance of a key
  struct HasKey : private OpBase {
    using value_type = bool;

    HasKey(std::string key) : key(std::move(key)) {}
    std::string key;

    std::optional<value_type> operator()(const Record& r) const {
      auto keys = r.keys();
      return { std::find(keys.begin(), keys.end(), key) != keys.end() };
    }
  };

  // wildcard, value of /any/ key with given type
  template <typename Value = void>
  struct Any : private ValueBase {
    using value_type = Value;

    // matches keys with the given type
    template <typename T>
    auto get(const Record& r) const {
      auto compare = [](const Record& r, auto&& op) {
        const auto values = r.values<T>();
        return std::any_of(values.begin(), values.end(), [&](auto&& v) {
          return op(v.second);
        });
      };
      return std::optional{ make_proxy<T>(r, compare) };
    }
  };

  // wildcard, value of /all/ keys with given type
  template <typename Value = void>
  struct All : private ValueBase {
    using value_type = Value;

    // matches keys with the given type
    template <typename T>
    auto get(const Record& r) const {
      auto compare = [](const Record& r, auto&& op) {
        const auto values = r.values<T>();
        return std::all_of(values.begin(), values.end(), [&](auto&& v) {
          return op(v.second);
        });
      };
      return std::optional{ make_proxy<T>(r, compare) };
    }
  };

  template <typename Fn, typename... Args>
  struct Invoker : private ValueBase {
  private:
    template <typename T>
    struct identity { using type = T; };

    template <typename T, typename = void>
    struct reified { using type = T; };
    template <typename T>
    struct reified<T, std::enable_if_t<is_valid_v<T>>> {
      using type = typename T::value_type;
    };
    template <typename T>
    using reified_t = typename reified<T>::type;

  public:
    using value_type = std::invoke_result_t<Fn, reified_t<Args>...>;

    Invoker(Fn fn, std::tuple<Args...>&& args)
      : fn(std::move(fn)), args(std::move(args))
    {}

    template <typename T>
    auto get(const Record& r) const {
      return get_impl(r, identity<T>{}, std::make_index_sequence<sizeof...(Args)>{});
    }

  private:
    Fn fn;
    std::tuple<Args...> args;

    // extracts a `std::optional<T>` from the args tuple at given index
    template <std::size_t I>
    auto extract(const Record& r) const {
      auto&& elem = std::get<I>(args);
      using elem_t = std::decay_t<decltype(elem)>;

      if constexpr (is_value_v<elem_t>) {
        if (auto ptr = elem.template get<typename elem_t::value_type>(r))
          return std::optional{ *std::move(ptr) };
        else
          return decltype(std::optional{ *std::move(ptr) }){};
      } else if constexpr (is_operation_v<elem_t>) {
        if (auto result = elem(r))
          return std::optional{ *std::move(result) };
        else
          return decltype(std::optional{ *std::move(result) }){};
      } else {
        return std::optional{ elem };
      }
    }

    template <typename T, std::size_t... I>
    auto get_impl(const Record& r, identity<T>, std::index_sequence<I...> seq) const {
      auto tuple = std::tuple{ extract<I>(r)... };
      if ((... and static_cast<bool>(std::get<I>(tuple)))) {
        if constexpr (std::disjunction_v<is_proxy<decltype(*std::get<I>(tuple))>...>) {
          return std::optional{ wrap_proxy(r, std::move(tuple), identity<T>{}, seq) };
        } else {
          return std::optional{ std::invoke(fn, *std::get<I>(tuple)...) };
        }
      } else {
        return decltype(get_impl(r, identity<T>{}, seq)){};
      }
    }

    template <std::size_t I, std::size_t E, typename Tuple, typename Params, typename Op>
    static bool wrap_proxy_impl(Tuple&& tuple, Params&& params, Op&& op) {
      static_assert(std::is_rvalue_reference_v<decltype(tuple)>);
      static_assert(std::is_rvalue_reference_v<decltype(params)>);
      if constexpr (I == E) {
        return op(std::forward<Params>(params));
      } else {
        if constexpr (is_proxy_v<decltype(*std::get<I>(tuple))>) {
          return std::get<I>(tuple)->go([&](auto&& v) {
            return wrap_proxy_impl<I+1, E>(
              std::move(tuple),
              std::tuple_cat(std::move(params), std::forward_as_tuple(v)),
              std::forward<Op>(op));
          });
        } else {
          return wrap_proxy_impl<I+1, E>(
            std::move(tuple),
            std::tuple_cat(std::move(params),
                           std::forward_as_tuple(*std::get<I>(std::move(tuple)))),
            std::forward<Op>(op));
        }
      }
    }

    template <typename T, typename Tuple, std::size_t... Is>
    auto wrap_proxy(const Record& r, Tuple&& tuple, identity<T>,
                    std::index_sequence<Is...>) const {
      static_assert(std::is_rvalue_reference_v<decltype(tuple)>);
      auto compare = [fn = fn, tuple = std::move(tuple)](const Record&, auto&& op) {
        return wrap_proxy_impl<0, sizeof...(Is)>(
          std::move(tuple), std::tuple{},
          [&](auto&& params){ return op(std::apply(fn, std::move(params))); });
      };
      return make_proxy<T>(r, compare);
    }
  };
} // namespace db::detail

// operator wrappers, this is where the DSL actually happens
namespace db::detail {
  // binary expressions with monadic failure
  template <typename Lhs, typename Rhs, typename Op>
  class BinOp : private OpBase {
  public:
    static_assert(is_valid_v<Lhs>,
                  "LHS must be a valid expression component");
    static_assert(is_valid_v<Rhs>,
                  "RHS must be a valid expression component");
    static_assert(not is_untyped_v<Lhs> or not is_untyped_v<Rhs>,
                  "At least one side of the expression must have explicit type");

    using lhs_value_type = std::conditional_t<
      is_untyped_v<Lhs>, typename Rhs::value_type, typename Lhs::value_type>;
    using rhs_value_type = std::conditional_t<
      is_untyped_v<Rhs>, typename Lhs::value_type, typename Rhs::value_type>;

    // our value_type is the resulting type of the expression
    using value_type = decltype(std::declval<Op>()(std::declval<lhs_value_type>(),
                                                   std::declval<rhs_value_type>()));

    template <typename L, typename R, typename O, typename M = std::bool_constant<true>>
    BinOp(L&& lhs, R&& rhs, O op, M = {})
      : lhs{ std::forward<L>(lhs) }, rhs{ std::forward<R>(rhs) }, op{ std::move(op) }
    {}

    // delegate to the child expressions/values
    // returns approximately an `optional<value_type>`, but not really because proxies
    auto operator()(const Record& r) const {
      // need to special case builtin "or": we just want to test one or the other
      // don't worry about monadic failure in this case
      if constexpr (std::is_same_v<Op, decltype(std::logical_or{})>
                    and std::is_same_v<value_type, bool>) {
        return std::optional{ either_true(r) };
      } else {
        return monadic_calculate(r);
      }
    }

  private:
    Lhs lhs;
    Rhs rhs;
    Op op;

    bool either_true(const Record& r) const {
      if constexpr (is_value_v<Lhs>) {
        if (auto lhs_ptr = lhs.template get<lhs_value_type>(r)) {
          if (*lhs_ptr) return true;
        }
      } else {
        if (auto lhs_expr = lhs(r)) {
          if (*lhs_expr) return true;
        }
      }
      if constexpr (is_value_v<Rhs>) {
        if (auto rhs_ptr = rhs.template get<rhs_value_type>(r)) {
          if (*rhs_ptr) return true;
        }
      } else {
        if (auto rhs_expr = rhs(r)) {
          if (*rhs_expr) return true;
        }
      }
      return false;
    }

    auto monadic_calculate(const Record& r) const {
      if constexpr (is_value_v<Lhs> and is_value_v<Rhs>) {
        if (auto lhs_ptr = lhs.template get<lhs_value_type>(r)) {
          if (auto rhs_ptr = rhs.template get<rhs_value_type>(r)) {
            return std::optional{ op(*std::move(lhs_ptr), *std::move(rhs_ptr)) };
          }
        }
      } else if constexpr (is_value_v<Lhs>) {
        if (auto lhs_ptr = lhs.template get<lhs_value_type>(r)) {
          if (auto right_expr = rhs(r)) {
            return std::optional{ op(*std::move(lhs_ptr), *std::move(right_expr)) };
          }
        }
      } else if constexpr (is_value_v<Rhs>) {
        if (auto left_expr = lhs(r)) {
          if (auto rhs_ptr = rhs.template get<rhs_value_type>(r)) {
            return std::optional{ op(*std::move(left_expr), *std::move(rhs_ptr)) };
          }
        }
      } else {
        if (auto left_expr = lhs(r)) {
          if (auto right_expr = rhs(r)) {
            return std::optional{ op(*std::move(left_expr), *std::move(right_expr)) };
          }
        }
      }

      // fallback if any of the above didn't return anything valid
      // this would be so much nicer with a monadic std::optional
      return decltype(monadic_calculate(r)){};
    }
  };

  // unary expressions, relational and not
  template <typename Expr, typename Op>
  class UnaryOp : private OpBase {
  public:
    static_assert(is_valid_v<Expr>,
                  "Must provide a valid expression component");
    static_assert(not is_untyped_v<Expr>,
                  "A unary expression cannot have unknown type");

    using value_type = typename Expr::value_type;

    template <typename E, typename O>
    UnaryOp(E&& expr, O op)
      : expr{ std::forward<E>(expr) }, op{ std::move(op) }
    {};

    auto operator()(const Record& r) const {
      if constexpr (is_value_v<Expr>) {
        if (auto expr_ptr = expr.template get<value_type>(r))
          return std::optional{ op(*std::move(expr_ptr)) };
      } else {
        if (auto e = expr(r))
          return std::optional{ op(*std::move(e)) };
      }
      return decltype((*this)(r)){};
    }

  private:
    Expr expr;
    Op op;
  };

  // use CTAD to automatically wrap non-expressions into Constants
  template <typename Expr>
  using wrapped = std::conditional<is_valid_v<Expr>, Expr, Constant<Expr>>;
  template <typename Expr>
  using wrapped_t = typename wrapped<Expr>::type;

  template <typename Lhs, typename Rhs, typename Op>
  BinOp(Lhs, Rhs, Op) -> BinOp<wrapped_t<Lhs>, wrapped_t<Rhs>, Op>;
  template <typename Expr, typename Op>
  UnaryOp(Expr, Op) -> UnaryOp<wrapped_t<Expr>, Op>;

  // operator overloads for expression operators (relies on ADL)

  // more macro goodness
  // ignore proxies in ops so that they can properly use their own calls
#define DB_DETAIL_UNARYOP_DECL(op, func)                                       \
  template <typename Expr, std::enable_if_t<not is_proxy_v<Expr>, int> = 0>    \
  auto operator op(Expr&& expr) {                                              \
    return UnaryOp(std::forward<Expr>(expr), func{});                          \
  }
#define DB_DETAIL_BINOP_DECL(op, func)                                         \
  template <typename Lhs, typename Rhs, std::enable_if_t<                      \
              not (is_proxy_v<Lhs> or is_proxy_v<Rhs>), int> = 0>              \
  auto operator op(Lhs&& lhs, Rhs&& rhs) {                                     \
    return BinOp(std::forward<Lhs>(lhs), std::forward<Rhs>(rhs), func{});      \
  }

  // no standard function object for unary +, so provide it
  struct unary_plus {
    template <typename T>
    constexpr auto operator()(T&& t) const -> decltype(+std::forward<T>(t)) {
      return +std::forward<T>(t);
    }
  };

  // relational operators
  DB_DETAIL_BINOP_DECL(<, std::less);
  DB_DETAIL_BINOP_DECL(>, std::greater);
  DB_DETAIL_BINOP_DECL(==, std::equal_to);
  DB_DETAIL_BINOP_DECL(!=, std::not_equal_to);
  DB_DETAIL_BINOP_DECL(<=, std::less_equal);
  DB_DETAIL_BINOP_DECL(>=, std::greater_equal);
  DB_DETAIL_BINOP_DECL(||, std::logical_or);
  DB_DETAIL_BINOP_DECL(&&, std::logical_and);
  DB_DETAIL_UNARYOP_DECL(!, std::logical_not);

  // arithmetic operators
  DB_DETAIL_BINOP_DECL(+, std::plus);
  DB_DETAIL_BINOP_DECL(-, std::minus);
  DB_DETAIL_BINOP_DECL(*, std::multiplies);
  DB_DETAIL_BINOP_DECL(/, std::divides);
  DB_DETAIL_BINOP_DECL(%, std::modulus);
  DB_DETAIL_UNARYOP_DECL(+, unary_plus);
  DB_DETAIL_UNARYOP_DECL(-, std::negate);

  // bitwise operators
  DB_DETAIL_BINOP_DECL(&, std::bit_and);
  DB_DETAIL_BINOP_DECL(|, std::bit_or);
  DB_DETAIL_BINOP_DECL(^, std::bit_xor);
  DB_DETAIL_UNARYOP_DECL(~, std::bit_not);

#undef DB_DETAIL_BINOP_DECL
#undef DB_DETAIL_UNARYOP_DECL
} // namespace db::detail

// main user interface for query expressions
namespace db {
  // a constant value to compare with;
  // this is usually unnecessary, the DSL try to wrap automatically
  template <typename T>
  auto constant(T&& t) {
    return detail::Constant<std::decay_t<T>>(std::forward<T>(t));
  }

  // represents an extracted value with given `key`;
  // if supplied, `Value` is the type of the value to extract
  // otherwise the type is inferred from context, if possible
  template <typename Value = void>
  auto get(std::string key) {
    return detail::Key<Value>(std::move(key));
  }

  // determines if a key exists
  auto has_key(std::string key) {
    return detail::HasKey(std::move(key));
  }

  // represents any key from the record
  inline constexpr detail::Any<> any{};
  template <typename T>
  inline constexpr detail::Any<T> any_t{};

  // call a function on query expressions
  template <typename Fn, typename... Args>
  auto invoke(Fn&& fn, Args&&... args) {
    return detail::Invoker(
      std::forward<Fn>(fn), std::make_tuple(std::forward<Args>(args)...));
  }

  // main interface into expression tree
  // use `operator()` to execute the query
  // will only return boolean values
  template <typename Expr>
  class Query {
  public:
    static_assert(detail::is_valid_v<Expr>,
                  "Expr must be a valid expression");
    static_assert(not std::is_same_v<typename Expr::value_type, void>,
                  "Expr must have a complete type");
    Query(Expr&& expr) : expr{ std::move(expr) } {}
    Query(const Expr& expr) : expr{ expr } {}

    bool operator()(const Record& r) {
      if constexpr (detail::is_operation_v<Expr>) {
        return expr(r).value_or(false);
      } else {
        if (auto ptr = expr.template get<typename Expr::value_type>(r))
          return static_cast<bool>(*ptr);
        return false;
      }
    }

  private:
    Expr expr;
  };

  // the database itself
  class Database {
  public:
    Database() = default;

    // construction from a list of records;
    // doesn't use initializer_list to prevent spurious copies
    template <typename First, typename... Rest,
              std::enable_if_t<std::is_constructible_v<Record, First>, int> = 0>
    Database(First&& first, Rest&&... rest) {
      static_assert((... and std::is_constructible_v<Record, Rest>));
      records.reserve(sizeof...(Rest) + 1);
      records.emplace_back(std::forward<First>(first));
      (records.emplace_back(std::forward<Rest>(rest)), ...);
    }

    // generate a new database with only the records satisfying the provided query
    template <typename Expr>
    Database filter(Expr&& e) const & {
      auto query = Query(std::forward<Expr>(e));
      std::vector<Record> result;
      std::copy_if(records.begin(), records.end(), std::back_inserter(result), query);

      Database new_db;
      new_db.records = std::move(result);
      return new_db;
    }

    // generate a new database with only the records satisfying the provided query;
    // leaves this database empty (only called on rvalues)
    template <typename Expr>
    Database filter(Expr&& e) && {
      auto query = Query(std::forward<Expr>(e));
      auto it = std::remove_if(records.begin(), records.end(), std::not_fn(query));
      records.erase(it, records.end());
      return Database(std::move(*this));
    }

    friend std::ostream& operator<<(std::ostream& os, const Database& d) {
      for (auto&& r : d.records)
        os << r << "\n";
      return os;
    }

  private:
    std::vector<Record> records;
  };
} // namespace db

#endif // LAZYQUERY_H_INCLUDED

// vim:sw=2:sts=2:
