// C++ Member type annotations

#ifndef ANNO_H
#define ANNO_H

#include <reflect>

/**
 * @file
 * @brief The member annotations library
 **/

//! The member annotations library
namespace anno {

namespace detail {
  template<template<auto...> class T, typename... Ts>
  constexpr auto concat() noexcept;

  template<template<auto...> class T, typename... Ts>
  using concatenate_t = decltype(concat<T, Ts...>());

  template<template<auto...> class T>
  struct template_type_nontype_args
  {};

  template<class T>
  struct type
  {};

  template<class Callable>
  concept PredicateReturnsBool =
    std::is_same_v<std::invoke_result_t<Callable, bool>, bool>;
} // namespace detail

/**
 * @brief Type helper
 *
 * Generates a filter criterion for @ref annotation_list::filter */
template<class T>
constexpr detail::type<T>
type()
{
  return detail::type<T>{};
}

/**
 * @brief Type helper
 *
 * Generates a filter criterion for @ref annotation_list::filter */
template<template<auto...> class T>
constexpr detail::template_type_nontype_args<T>
type()
{
  return detail::template_type_nontype_args<T>{};
}

/**
 * @brief Annotation list
 *
 * This class is used to hold lists of annotations, which are represented as
 *non-type template argument pack `Anns`.
 *
 * Example usage:
 * @snippet{trimleft} anno_snippets.cpp annotation_list
 **/
template<auto... Anns>
struct annotation_list
{
  //! @name Basic Queries
  //@{
  //! Number of annotations
  static constexpr size_t size = sizeof...(Anns);

  //! True iff not empty
  constexpr operator bool() { return size != 0; }
  //@}

  //! @name Iterating over Annotations
  //@{
  /**
   * @brief Execute @a f for each annotation
   *
   * @snippet{trimleft} anno_snippets.cpp annotation_list::for_each
   **/
  static constexpr void for_each(auto&& f) { (f(Anns), ...); }

  /**
   * @brief Disjunction
   *
   * Returns `(false || ... || p(Anns))`.
   **/
  template<class Predicate>
  static constexpr bool any(Predicate p)
  {
    static_assert(size != 0, "Called any() on an empty annotation_list");

    return (false || ... || p(Anns));
  }

  /**
   * @brief Conjunction
   *
   * Returns `(true && ... && p(Anns))`.
   **/
  template<class Predicate>
  static constexpr bool all(Predicate p)
  {
    static_assert(size != 0, "Called all() on an empty annotation_list");

    return (true && ... && p(Anns));
  }
  //@}

  //! @name Filtering Annotations
  //@{
  /**
   * @brief Filter using a predicate
   *
   * The resulting annotation_list contains each annotation @a A if and only if
   * @a Predicate(A) is true.
   *
   * @snippet{trimleft} anno_snippets.cpp annotation_list::filter(Predicate)
   * See the overloads below for shortcuts for filtering on type.
   **/
  template<auto Predicate>
    requires detail::PredicateReturnsBool<decltype(Predicate)>
  static constexpr auto filter();

  /**
   * @brief Filter based on annotation type
   *
   * This overload is used to filter annotation classes templated on non-type
   * arguments. Use @ref anno::type to generate the argument of this function.
   *
   * @snippet{trimleft} anno_snippets.cpp annotation_list::filter(NonType)
   **/
  template<template<auto...> class T>
  static constexpr auto filter(const detail::template_type_nontype_args<T>&)
  {
    return filter<[]<class A>(const A& a) {
      return requires { [&]<auto... Args>(const T<Args...>&) {}(a); };
    }>();
  }

  /**
   * @brief Filter based on annotation type
   *
   * This overload is used to filter annotations of a specific non-templated
   * type. Use @ref anno::type to generate the argument of this function.
   *
   * @snippet{trimleft} anno_snippets.cpp annotation_list::filter(Type)
   **/
  template<class T>
  static constexpr auto filter(const detail::type<T>&)
  {
    return filter<[]<class A>(const A& a) { return std::is_same_v<A, T>; }>();
  }

  //@}

  //! @name Direct Annotation Access
  //@{
  /**
   * @brief The first annotation
   *
   * This throws a `static_assert` if the annotation list is empty.
   **/
  static consteval auto front()
  {
    static_assert(size != 0, "Called front() on an empty annotation_list");

    return []<auto A0, auto... Rest>(const annotation_list<A0, Rest...>&) {
      return A0;
    }(annotation_list<Anns...>{});
  }

  /**
   * @brief Single annotation access
   *
   * This throws a `static_assert` if the list does not contain exactly one
   * annotation.
   **/
  static consteval auto get()
  {
    static_assert(size != 0, "Called get() on an empty annotation_list");
    static_assert(
      size <= 1,
      "Called get() on an annotation_list with more than one annotation");

    return []<auto A0>(const annotation_list<A0>&) {
      return A0;
    }(annotation_list<Anns...>{});
  }
  //@}
};

/**
 * @brief Represents a member in a struct
 *
 * Example usage:
 * @snippet{trimleft} anno_snippets.cpp member::get
 **/
template<class Struct, std::size_t IndexInStruct, class AnnotationList>
struct member
{
  using Annotations = AnnotationList;
  static constexpr std::size_t Index = IndexInStruct;

  //! The list of annotations
  constexpr Annotations annotations() const { return Annotations{}; }

  /**
   * @brief Filter annotations
   *
   * This is a shorthand for `annotations().filter(query)`.
   *
   * @snippet{trimleft} anno_snippets.cpp member::annotations(query)
   *
   * @sa @ref anno::type(), @ref anno::annotation_list::filter()
   **/
  template<class Query>
  constexpr auto annotations(const Query& query) const
  {
    return Annotations::template filter(query);
  }

  /**
   * @brief Index of this member in the struct
   *
   * @m_class{m-block m-warning}
   *
   * @par Warning:
   *   This is the original index in the struct and as thus is affected by the
   *"phantom" members added by the @ref ANNO() macro.
   **/
  consteval std::size_t index() const { return Index; }

  /**
   * @brief Name of this member
   **/
  constexpr std::string_view name() const
  {
    return reflect::member_name<IndexInStruct>(Struct{});
  }

  /**
   * @brief Access member
   *
   * Returns a reference to the member in instance @a s.
   *
   * @snippet{trimleft} anno_snippets.cpp member::get
   **/
  constexpr auto& get(Struct& s) { return reflect::get<IndexInStruct>(s); }

  //! @overload
  constexpr auto& get(const Struct& s)
  {
    return reflect::get<IndexInStruct>(s);
  }
};

/**
 * @brief A list of members
 *
 * The members are encoded as @ref member types in the template argument pack
 * `Members`.
 *
 * @snippet{trimleft} anno_snippets.cpp member::get
 **/
template<typename... Members>
struct member_list
{
  /**
   * Access a specific member
   **/
  template<std::size_t N>
  static consteval auto member()
  {
    return reflect::detail::nth_pack_element<N, Members...>(Members{}...);
  }

  template<std::size_t N>
  using Member = decltype(member<N>());

  /**
   * Iterate over members
   *
   * See above for an example.
   **/
  static constexpr void for_each(auto&& f) { (f(Members{}), ...); }
};

#define ANNO_CONCAT_(prefix, suffix) prefix##suffix
#define ANNO_CONCAT(prefix, suffix) ANNO_CONCAT_(prefix, suffix)
#define ANNO_CUSTOM(init_code, line, ...)                                      \
  decltype([]() {                                                              \
    init_code;                                                                 \
    return anno::annotation_list<__VA_ARGS__>();                               \
  }()) ANNO_CONCAT(zzz_anno, line) [[no_unique_address]];

/**
 * @brief Inline annotation
 *
 * The `ANNO()` macro adds an inline annotation to the following struct member.
 * The arguments to this macro are expected to be annotation *instances*.
 *
 * Example:
 * @snippet{trimleft} anno_snippets.cpp ANNO
 *
 * @m_class{m-block m-warning}
 *
 * @par Warning
 *   `ANNO()` defines additional "phantom" members to store the annotations.
 *   While these members do not take up memory space due to
 *   `[[no_unique_address]]`, they interfere with structured binding and
 *   aggregate initialization. In the above example, `Struct{2, false}` would
 *   fail to compile and `Struct{.number=2, .value=true}` would have to be used.
 **/
#define ANNO(...) ANNO_CUSTOM(, __COUNTER__, __VA_ARGS__)

#define ANNO_EXTERN_CUSTOM(init_code, line, member, ...)                       \
  namespace anno {                                                             \
    namespace detail {                                                         \
      template<>                                                               \
      struct external_annotation<anno::detail::struct_from_member_t<member>,   \
                                 anno::detail::type_from_member_t<member>,     \
                                 anno::detail::index_from_member<member>()>    \
      {                                                                        \
        using Value = decltype([]() {                                          \
          init_code;                                                           \
          return anno::annotation_list<__VA_ARGS__>();                         \
        }());                                                                  \
      };                                                                       \
    }                                                                          \
  }

/**
 * @brief External annotation
 *
 * Annotations can be made external to the class using `ANNO_EXTERN()`.
 * The @a member argument should be a member pointer.
 * The additional arguments to this macro are expected to be annotation
 * *instances*.
 *
 * Note that `ANNO_EXTERN()` has to be used in global namespace scope.
 *
 * Example usage:
 * @snippet{trimleft} anno_snippets.cpp ANNO_EXTERN
 **/
#define ANNO_EXTERN(...) ANNO_EXTERN_CUSTOM(, __COUNTER__, __VA_ARGS__)

#define ANNO_NESTED_CUSTOM(init_code, counter, member, ...)                    \
  ::anno::detail::nested_annotation<member, decltype([]() {                    \
                                      init_code;                               \
                                      return ::anno::annotation_list<          \
                                        __VA_ARGS__>();                        \
                                    }())>                                      \
    ANNO_CONCAT(zzz_anno, counter) [[no_unique_address]];

/**
 * @brief Nested annotation
 *
 * This macro is used to define nested annotations.
 * The @a member argument should be a member pointer.
 * The additional arguments to this macro are expected to be annotation
 **instances*.
 *
 * Example usage:
 * @snippet{trimleft} anno_snippets.cpp ANNO_NESTED
 **/
#define ANNO_NESTED(member, ...)                                               \
  ANNO_NESTED_CUSTOM(, __COUNTER__, member, __VA_ARGS__)

namespace detail {
  template<typename T>
  struct wrapConcat
  {};

  template<template<auto...> class T, auto... VA, auto... VB>
  constexpr auto operator+(T<VA...>, wrapConcat<T<VB...>>) noexcept
  {
    return T<VA..., VB...>();
  }

  template<template<typename...> class T, class... VA, class... VB>
  constexpr auto operator+(T<VA...>, wrapConcat<T<VB...>>) noexcept
  {
    return T<VA..., VB...>();
  }

  template<int... V>
  using IndexSeq = std::index_sequence<V...>;

  template<auto... VA, auto... VB>
  constexpr auto operator+(const IndexSeq<VA...>&,
                           const wrapConcat<IndexSeq<VB...>>&) noexcept
  {
    return IndexSeq<VA..., VB...>();
  }

  template<template<auto...> class T, typename... Ts>
  constexpr auto concat() noexcept
  {
    return (T<>{} + ... + wrapConcat<Ts>());
  }

  template<template<typename...> class T, typename... Ts>
  constexpr auto concat() noexcept
  {
    return (T<>{} + ... + wrapConcat<Ts>());
  }

  template<class T>
  constexpr bool is_annotation_list(const T& = {})
  {
    if constexpr (requires {
                    []<auto... A>(const annotation_list<A...>&) {}(T{});
                  })
      return true;
    else
      return false;
  }

  template<class Struct, std::size_t Index>
  using MemberType =
    std::remove_cvref_t<decltype(reflect::get<Index>(Struct{}))>;

  template<class T>
  struct member_type_helper;

  template<class C, class T>
  struct member_type_helper<T C::*>
  {
    using struct_type = C;
    using member_type = T;
  };

  template<auto M>
  using struct_from_member_t = member_type_helper<decltype(M)>::struct_type;

  template<auto M>
  using type_from_member_t = member_type_helper<decltype(M)>::member_type;

  template<auto M>
  constexpr auto index_from_member()
  {
    using Struct = struct_from_member_t<M>;
    using Type = type_from_member_t<M>;
    constexpr Struct instance{};

    auto ptr = &(instance.*M);

    std::size_t ret = 0;

    auto check = [&]<std::size_t I>(std::integral_constant<std::size_t, I>) {
      if constexpr (std::is_same_v<MemberType<Struct, I>, Type>) {
        if (&reflect::get<I>(instance) == ptr) {
          ret = I;
          return true;
        } else
          return false;
      } else
        return false;
    };

    bool found = [&]<auto... Idx>(std::index_sequence<Idx...>) {
      return (false || ... ||
              check(std::integral_constant<std::size_t, Idx>()));
    }(std::make_index_sequence<reflect::size<Struct>()>());

    if (found)
      return ret;
    else
      return static_cast<std::size_t>(-1);
  }

  template<class Struct, class MemberType, std::size_t Index>
  struct external_annotation
  {
    using Value = annotation_list<>;
  };

  template<auto MemberPointer, class Annotations>
  struct nested_annotation
  {
    using Value = Annotations;
    static constexpr auto Pointer = MemberPointer;

    template<typename T, std::size_t index>
    static constexpr bool matches()
    {
      return index == index_from_member<MemberPointer>() &&
             std::is_same_v<T, type_from_member_t<MemberPointer>>;
    }
  };
} // namespace detail

/**
 * @brief Analyze Struct and get members
 *
 * This method is the main entrance into anno's API. It returns a @ref member_list struct describing the members and their annotations.
 *
 * Example usage:
 * @snippet{trimleft} anno_snippets.cpp member::get
 **/
template<typename Struct>
constexpr auto
members(const Struct& s = {})
{
  // Get indices of all real members (not annotations)
  auto memberIndices = [&]<auto... Ns>(detail::IndexSeq<Ns...>) {
    return detail::concat<
      detail::IndexSeq,
      std::conditional_t<
        detail::is_annotation_list<detail::MemberType<Struct, Ns>>(),
        detail::IndexSeq<>,
        detail::IndexSeq<Ns>>...>();
  }(std::make_index_sequence<reflect::size<Struct>()>());

  // The following only makes sense if we actually have members
  if constexpr (memberIndices.size() > 0) {
    // For each member, where do we need to start looking for annotations?
    auto annotationStartIndicesPadded =
      [&]<auto... Ns>(detail::IndexSeq<Ns...>) {
        return detail::IndexSeq<0, Ns + 1 ...>();
      }(memberIndices);

    // Remove the last item  - doesn't make sense to look after it for
    // annotations and we need a list of the same size as memberIndices
    auto annotationStartIndices = []<auto... Ns>(detail::IndexSeq<Ns...>) {
      constexpr auto asTuple = std::make_tuple(Ns...);

      return [&]<auto... I>(std::index_sequence<I...>) {
        return detail::IndexSeq<std::get<I>(asTuple)...>{};
      }(std::make_index_sequence<sizeof...(Ns) - 1>());
    }(annotationStartIndicesPadded);

    // Called to generate the MemberList for each member
    auto getMember = []<std::size_t annotationStart, std::size_t memberIndex>(
                       std::integral_constant<std::size_t, annotationStart>,
                       std::integral_constant<std::size_t, memberIndex>) {
      using MemberType = detail::MemberType<Struct, memberIndex>;

      auto parseNested = []() {
        // Parse nested annotations if present
        if constexpr (requires { typename Struct::anno; }) {
          return []<auto... I>(std::index_sequence<I...>) {
            return detail::concat<
              annotation_list,
              std::conditional_t<
                detail::MemberType<typename Struct::anno, I>::
                  template matches<MemberType, memberIndex>(),
                typename detail::MemberType<typename Struct::anno, I>::Value,
                annotation_list<>>...>();
          }(std::make_index_sequence<reflect::size<typename Struct::anno>()>());
        } else
          return annotation_list<>();
      };

      auto annotations = [&]<auto... Ns>(std::index_sequence<Ns...>) {
        using Annotations = detail::concatenate_t<
          annotation_list,
          std::conditional_t<
            detail::is_annotation_list<
              detail::MemberType<Struct, annotationStart + Ns>>(),
            detail::MemberType<Struct, annotationStart + Ns>,
            annotation_list<>>...,
          decltype(parseNested()),
          typename detail::external_annotation<
            Struct,
            detail::MemberType<Struct, memberIndex>,
            memberIndex>::Value>;
        return Annotations();
      }(std::make_index_sequence<memberIndex - annotationStart>());

      return member<Struct,
                    static_cast<std::size_t>(memberIndex),
                    decltype(annotations)>();
    };

    return
      [&]<auto... startIndex, auto... memberIndex>(
        detail::IndexSeq<startIndex...>, detail::IndexSeq<memberIndex...>) {
        return detail::concat<
          member_list,
          member_list<decltype(getMember(
            std::integral_constant<std::size_t, startIndex>(),
            std::integral_constant<std::size_t, memberIndex>()))>...>();
      }(annotationStartIndices, memberIndices);
  } else
    return member_list<>{};
}

template<auto... Anns>
template<auto Predicate>
  requires detail::PredicateReturnsBool<decltype(Predicate)>
constexpr auto
annotation_list<Anns...>::filter()
{
  return detail::concat<annotation_list,
                        std::conditional_t<Predicate(Anns),
                                           annotation_list<Anns>,
                                           annotation_list<>>...>();
}

namespace tests {
  void failed();
  inline constexpr auto expect = [](bool cond) {
    if (not cond) {
      failed();
    }
  };

  template<class Check>
  struct VerboseCheck
  {
    static_assert(Check::value);
    static constexpr bool value = Check::value;
  };

  namespace anns {
    template<std::size_t N>
    struct Help
    {
      constexpr Help(const char (&str)[N])
      {
        for (std::size_t i = 0; i < N; ++i)
          string[i] = str[i];
      }

      char string[N];
    };

    struct Short
    {
      constexpr Short(char c)
        : key{ c }
      {
      }

      char key;
    };
  } // namespace anns

  struct Test
  {
    ANNO(anns::Help{ "my help string" }, anns::Short{ 'h' })
    bool help = false;

    ANNO(anns::Help{ "verbose" }, anns::Short{ 'v' })
    ANNO(anns::Short{ 'd' })
    bool verbose = false;

    struct anno
    {
      ANNO_NESTED(&Test::help, anns::Short{ 'x' })
    };
  };
}
}

ANNO_EXTERN(&anno::tests::Test::help, anno::tests::anns::Short{ 'f' })

namespace anno {
namespace tests {
  static_assert(sizeof(Test) == 2,
                "Annotations should not increase struct size");
  static_assert(
    VerboseCheck<std::is_same<
      decltype(anno::members<Test>()),
      member_list<member<Test,
                         1,
                         annotation_list<anns::Help{ "my help string" },
                                         anns::Short{ 'h' },
                                         anns::Short{ 'x' },
                                         anns::Short{ 'f' }>>,
                  member<Test,
                         4,
                         annotation_list<anns::Help{ "verbose" },
                                         anns::Short{ 'v' },
                                         anns::Short{ 'd' }>>>>>::value,
    "Unexpected result from get()");

  static_assert([]() {
    std::size_t memberCounter = 0;
    std::size_t annotationCounter = 0;

    anno::members<Test>().for_each([&](const auto& member) {
      member.annotations().for_each(
        [&](const auto& ann) { annotationCounter++; });

      memberCounter++;
    });

    expect(memberCounter == 2);
    expect(annotationCounter == 7);

    return true;
  }());

  static_assert([]() {
    auto helpAnn = anno::members<Test>()
                     .member<0>()
                     .annotations(anno::type<anns::Help>())
                     .get();

    expect(std::string_view{ helpAnn.string } == "my help string");

    return true;
  }());
}
}

#endif
