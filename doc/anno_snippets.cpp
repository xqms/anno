// Snippets for documentation

#include <iostream>

#include <anno.h>

struct MyAnnotation
{
  bool value = false;
};

template<std::size_t N>
struct AnnotationWithNonTypeArgs
{};

template<class T>
struct AnnotationWithTypeArgs
{};

struct Struct
{
  ANNO(MyAnnotation{})
  int number;

  bool value;
};

int
main(int argc, char** argv)
{
  //! [annotation_list]
  struct MyStruct
  {
    ANNO(MyAnnotation{}, AnnotationWithNonTypeArgs<2>{})
    int number;

    bool value;
  };

  auto number_annotations = anno::members<MyStruct>().member<0>().annotations();

  // The result is an annotation_list containing the annotations
  static_assert(
    std::is_same_v<
      decltype(number_annotations),
      anno::annotation_list<MyAnnotation{}, AnnotationWithNonTypeArgs<2>{}>>);

  // Do something for all annotations of type MyAnnotation
  number_annotations.filter(anno::type<MyAnnotation>())
    .for_each([](auto annotation) {
      // ...
    });
  //! [annotation_list]

  //! [annotation_list::for_each]
  anno::members<Struct>().for_each([](auto member) {
    member.annotations().for_each([&](auto annotation) {
      std::cout << member.name() << " has an annotation\n";
    });
  });
  //! [annotation_list::for_each]

  //! [annotation_list::filter(Predicate)]
  auto is_my_annotation = []<class A>(const A& annotation) {
    return std::is_same_v<A, MyAnnotation>;
  };

  anno::members<Struct>().for_each([&](auto member) {
    std::cout << "Member " << member.name() << " has "
              << member.annotations().template filter<is_my_annotation>().size
              << " annotations of type MyAnnotation.\n";
  });
  //! [annotation_list::filter(Predicate)]

  //! [annotation_list::filter(NonType)]
  anno::members<Struct>().for_each([](auto member) {
    auto list =
      member.annotations().filter(anno::type<AnnotationWithNonTypeArgs>());
    std::cout << "Member " << member.name() << " has " << list.size
              << " annotations of type AnnotationWithNonTypeArgs.\n";
  });
  //! [annotation_list::filter(NonType)]

  //! [annotation_list::filter(Type)]
  anno::members<Struct>().for_each([](auto member) {
    auto list = member.annotations().filter(anno::type<MyAnnotation>());
    std::cout << "Member " << member.name() << " has " << list.size
              << " annotations of type MyAnnotation.\n";
  });
  //! [annotation_list::filter(Type)]

  //! [member::annotations(query)]
  anno::members<Struct>().for_each([](auto member) {
    auto list = member.annotations(anno::type<MyAnnotation>());
    std::cout << "Member " << member.name() << " has " << list.size
              << " annotations of type MyAnnotation.\n";
  });
  //! [member::annotations(query)]

  //! [member::get]
  struct Print{};

  struct Person
  {
    ANNO(Print{})
    std::string firstName;

    ANNO(Print{})
    std::string lastName;

    std::string password;
  };

  Person p{.firstName="Joe", .lastName="Davis", .password="secret"};

  anno::members<Person>().for_each([&](auto member){
    if constexpr(member.annotations(anno::type<Print>()))
      std::cout << member.name() << ": " << member.get(p) << "\n";
  });
  //! [member::get]
}
