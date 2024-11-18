// Annotations for argument parsing

#include <anno.h>

#include <algorithm>
#include <iostream>
#include <format>
#include <sstream>

namespace anns
{
    template<std::size_t N>
    struct Help
    {
        constexpr Help(const char (&str)[N]) {
            std::copy_n(str, N, string);
        }

        char string[N];
    };

    struct Short
    {
        char key;
    };
}

struct Test
{
    ANNO(anns::Help{"my help string"}, anns::Short{'h'})
    bool help = false;

    ANNO(anns::Help{"input file"}, anns::Short{'i'})
    ANNO(anns::Short{'c'})
    ANNO(anns::Short{'d'})
    std::string_view inputFile;
};

int main(int argc, char** argv)
{
    anno::members<Test>().for_each([&](const auto& member){
        std::string help;
        if constexpr(auto Help = member.annotations(anno::type<anns::Help>()))
        {
            help = Help.get().string;
        }

        std::stringstream names;
        names << "--" << member.name();

        if constexpr(auto Shorts = member.annotations(anno::type<anns::Short>()))
        {
            Shorts.for_each([&](const auto& ann){
                names << ", -" << ann.key;
            });
        }

        std::cout << std::format(" {:<30} {}\n", names.str(), help);
    });

    Test test;
}
