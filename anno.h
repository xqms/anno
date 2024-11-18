// C++ Member type annotations

#ifndef ANNO_H
#define ANNO_H

#include <reflect>

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
} // namespace detail

template<class T>
constexpr detail::type<T>
type()
{
  return detail::type<T>{};
}

template<template<auto...> class T>
constexpr detail::template_type_nontype_args<T>
type()
{
  return detail::template_type_nontype_args<T>{};
}

template<auto... Anns>
struct AnnotationList
{
  static constexpr size_t size = sizeof...(Anns);

  constexpr operator bool() { return size != 0; }

  static constexpr void for_each(auto&& f) { (f(Anns), ...); }

  template<auto Predicate>
  static constexpr auto filter();

  template<template<auto...> class T>
  static constexpr auto filter(const detail::template_type_nontype_args<T>&)
  {
    return filter<[]<class A>(const A& a) {
      return requires { [&]<auto... Args>(const T<Args...>&) {}(a); };
    }>();
  }

  template<class T>
  static constexpr auto filter(const detail::type<T>&)
  {
    return filter<[]<class A>(const A& a) { return std::is_same_v<A, T>; }>();
  }

  static consteval auto front()
  {
    static_assert(size != 0, "Called front() on an empty AnnotationList");

    return []<auto A0, auto... Rest>(const AnnotationList<A0, Rest...>&) {
      return A0;
    }(AnnotationList<Anns...>{});
  }

  static consteval auto get()
  {
    static_assert(size != 0, "Called get() on an empty AnnotationList");
    static_assert(
      size <= 1,
      "Called get() on an AnnotationList with more than one annotation");

    return []<auto A0>(const AnnotationList<A0>&) {
      return A0;
    }(AnnotationList<Anns...>{});
  }
};

template<class Struct, std::size_t IndexInStruct, typename AnnotationList>
struct Member
{
  using Annotations = AnnotationList;
  static constexpr std::size_t Index = IndexInStruct;

  constexpr Annotations annotations() const { return Annotations{}; }

  template<class T>
  constexpr auto annotations(const T& t) const
  {
    return Annotations::template filter(t);
  }

  consteval std::size_t index() const { return Index; }

  constexpr auto name() const
  {
    return reflect::member_name<IndexInStruct>(Struct{});
  }

  constexpr auto& get(Struct& s) { return reflect::get<IndexInStruct>(s); }
};

template<typename... Members>
struct MemberList
{
  template<std::size_t N>
  static consteval auto member()
  {
    return reflect::detail::nth_pack_element<N, Members...>(Members{}...);
  }

  template<std::size_t N>
  using Member = decltype(member<N>());

  static constexpr void for_each(auto&& f) { (f(Members{}), ...); }
};

#define ANNO_CONCAT_(prefix, suffix) prefix##suffix
#define ANNO_CONCAT(prefix, suffix) ANNO_CONCAT_(prefix, suffix)
#define ANNO_CUSTOM(init_code, line, ...)                                      \
  decltype([]() {                                                              \
    init_code;                                                                 \
    return anno::AnnotationList<__VA_ARGS__>();                                \
  }()) ANNO_CONCAT(zzz_anno, line) [[no_unique_address]];
#define ANNO(...) ANNO_CUSTOM(, __COUNTER__, __VA_ARGS__)

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
  using IndexSeq = std::integer_sequence<int, V...>;

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
  constexpr bool isAnnotationList(const T& = {})
  {
    if constexpr (requires {
                    []<auto... A>(const AnnotationList<A...>&) {}(T{});
                  })
      return true;
    else
      return false;
  }

  template<class... T>
  constexpr auto tupleToMemberList(std::tuple<T...>)
  {
    return MemberList<T...>();
  }
} // namespace detail

template<typename Struct>
constexpr auto
members(const Struct& s = {})
{
  // Get indices of all real members (not annotations)
  auto memberIndices = [&]<auto... Ns>(detail::IndexSeq<Ns...>) {
    return detail::concat<
      detail::IndexSeq,
      std::conditional_t<detail::isAnnotationList<std::remove_cvref_t<
                           decltype(reflect::get<Ns>(s))>>(),
                         detail::IndexSeq<>,
                         detail::IndexSeq<Ns>>...>();
  }(std::make_integer_sequence<int, reflect::size<Struct>()>());

  // For each argument, where do we need to start looking for annotations?
  auto annotationStartIndices = [&]<auto... Ns>(detail::IndexSeq<Ns...>) {
    return detail::IndexSeq<0, Ns + 1 ...>();
  }(memberIndices);

  // Make member index list with same size
  auto paddedMembers = [&]<auto... Ns>(detail::IndexSeq<Ns...>) {
    return detail::IndexSeq<Ns..., -1>();
  }(memberIndices);

  auto getMember = []<int annotationStart, int memberIndex>(
                     std::integral_constant<int, annotationStart>,
                     std::integral_constant<int, memberIndex>) {
    auto annotations =
      [&]<auto... Ns>(std::index_sequence<Ns...>) {
        using Annotations = detail::concatenate_t<
          AnnotationList,
          std::conditional_t<
            detail::isAnnotationList<std::remove_cvref_t<
              decltype(reflect::get<annotationStart + Ns>(s))>>(),
            std::remove_cvref_t<decltype(reflect::get<annotationStart + Ns>(
              s))>,
            AnnotationList<>>...>;
        return Annotations();
      }(std::make_index_sequence < (memberIndex >= annotationStart)
          ? (memberIndex - annotationStart)
          : 0 > ());

    return Member<Struct,
                  static_cast<std::size_t>(memberIndex),
                  decltype(annotations)>();
  };

  return [&]<auto... startIndex, auto... memberIndex>(
           detail::IndexSeq<startIndex...>, detail::IndexSeq<memberIndex...>) {
    return detail::concat<
      MemberList,
      std::conditional_t<memberIndex >= 0,
                         MemberList<decltype(getMember(
                           std::integral_constant<int, startIndex>(),
                           std::integral_constant<int, memberIndex>()))>,
                         MemberList<>>...>();
  }(annotationStartIndices, paddedMembers);
}

template<class Struct>
using members_t = decltype(members<Struct>());

template<auto... Anns>
template<auto F>
constexpr auto
AnnotationList<Anns...>::filter()
{
  using Matches = detail::concatenate_t<
    AnnotationList,
    std::conditional_t<F(Anns), AnnotationList<Anns>, AnnotationList<>>...>;

  return Matches{};
}

namespace tests {
  void failed();
  inline constexpr auto expect = [](bool cond) {
    if (not cond) {
      failed();
    }
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
  };

  static_assert(sizeof(Test) == 2,
                "Annotations should not increase struct size");
  static_assert(
    std::is_same_v<
      anno::members_t<Test>,
      MemberList<Member<Test,
                        1,
                        AnnotationList<anns::Help{ "my help string" },
                                       anns::Short{ 'h' }>>,
                 Member<Test,
                        4,
                        AnnotationList<anns::Help{ "verbose" },
                                       anns::Short{ 'v' },
                                       anns::Short{ 'd' }>>>>,
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
    expect(annotationCounter == 5);

    return true;
  }());

  static_assert([]() {
    auto helpAnn = anno::members_t<Test>::Member<0>::Annotations::filter(
                     anno::type<anns::Help>())
                     .get();

    expect(std::string_view{ helpAnn.string } == "my help string");

    return true;
  }());
}

}

#endif
