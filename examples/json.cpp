// Example showing a JSON serializer
// WARNING: use a proper JSON library instead, this code does not
//  escape strings correctly and is only meant to showcase the annotation
//  system.

#include <anno.h>

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>

namespace json {

template<std::size_t N>
struct Rename
{
  constexpr Rename(const char (&str)[N]) { std::copy_n(str, N, name); }

  char name[N];
};

template<auto Predicate>
struct SkipIf
{
  static constexpr bool eval(const auto& value) { return Predicate(value); }
};

}

struct Person
{
  ANNO(json::Rename{ "first name" })
  std::string first;

  ANNO(json::Rename{ "middle name" },
       json::SkipIf<[](const std::string& val) { return val.empty(); }>{})
  std::string middle;

  ANNO(json::Rename{ "last name" })
  std::string last;
};

template<class Struct>
std::string
toJSON(const Struct& s)
{
  std::stringstream ss;
  ss << "{\n";

  bool first = true;

  anno::members<Struct>().for_each([&](auto member) {
    std::string name = [&]() {
      if constexpr (auto renameList =
                      member.annotations(anno::type<json::Rename>()))
        return std::string{ renameList.get().name };
      else
        return std::string{ member.name() };
    }();

    const auto& value = member.get(s);

    if constexpr (auto skips = member.annotations(anno::type<json::SkipIf>())) {
      bool skip = skips.any([&](auto skip) { return skip.eval(value); });

      if (skip)
        return;
    }

    if (!first)
      ss << ",\n";

    ss << "  \"" << name << "\": ";

    if constexpr (std::is_arithmetic_v<decltype(value)>)
      ss << value;
    else if constexpr (std::is_convertible_v<decltype(value), std::string>)
      ss << '"' << value << '"'; // WARNING: does not escape strings!
    first = false;
  });

  if (!first)
    ss << "\n";

  ss << "}";

  return ss.str();
}

int
main(int argc, char** argv)
{
  Person person{ .first = "Peter", .last = "Doe" };

  std::cout << toJSON(person) << "\n";
}
