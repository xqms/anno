// Annotations for argument parsing

#include <anno.h>

#include <algorithm>
#include <format>
#include <iostream>
#include <sstream>

template<std::size_t N>
struct Help
{
  constexpr Help(const char (&str)[N]) { std::copy_n(str, N, string); }

  char string[N];
};

struct Short
{
  char key;
};

struct Test
{
  ANNO(Help{ "my help string" }, Short{ 'h' })
  bool help = false;

  ANNO(Help{ "input file" }, Short{ 'i' })
  ANNO(Short{ 'c' })
  ANNO(Short{ 'd' })
  std::string_view inputFile;
};

int
main(int argc, char** argv)
{
  anno::members<Test>().for_each([&](const auto& member) {
    std::string help;
    if constexpr (auto helpList = member.annotations(anno::type<Help>())) {
      help = helpList.get().string;
    }

    std::stringstream names;
    names << "--" << member.name();

    if constexpr (auto shortList = member.annotations(anno::type<Short>())) {
      shortList.for_each([&](const auto& ann) { names << ", -" << ann.key; });
    }

    std::cout << std::format(" {:<30} {}\n", names.str(), help);
  });
}
