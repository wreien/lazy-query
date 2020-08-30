#include <iostream>
#include <string>
#include <functional>
#include "lazy_query.hpp"

int main() {
  using namespace std::literals;

  // general example
  auto d = db::Database{
    db::Record{ "first"s, 1, "second"s, 2, "third"s, 3 },
    db::Record{ "now"s, "we"s, "go"s, "to"s, "the"s, 3, "yet"s, 1000 },
    db::Record{ "yet"s, 1000, "another"s, "record"s },
  };
  std::cout << d.filter(
    db::any() <= 3 and (db::get("first"s) == 1 or db::get("now"s) == "we"s));

  // chained builder-style
  std::cout << "---\n";
  auto other = std::move(d).filter(db::any() == 3).filter(db::has_key("first"s));
  std::cout << other;

  // complex arithmetic
  std::cout << "---\n";
  std::cout << db::Database{
    db::Record{ "a", 1, "b", 2, "c", 3 },
    db::Record{ "a", 2, "b", 2, "c", 2 },
    db::Record{ "a", 3, "b", 2, "c", 1 },
    db::Record{ "a", 4, "b", 2, "c", 10 },
  }.filter( db::any() * 2 > 15 or 3 - -db::get<int>("c") < 2 * db::get("a") );

  // manual type specification
  std::cout << "---\n";
  const auto inclusion_exclusion = db::Database{
    db::Record{ "a", true, "b", "included" },
    db::Record{ "a", false, "b", "excluded" },
  };
  std::cout << inclusion_exclusion.filter(db::get<bool>("a"));
  std::cout << inclusion_exclusion.filter(db::any<bool>());
  std::cout << inclusion_exclusion.filter(not db::any<bool>());

  // type inference
  std::cout << "---\n";
  std::cout << db::Database{
    db::Record{ "x", 1.f, "to_infer", 1, "inferred", "int * float => int" },
    db::Record{ "x", 1, "to_infer", 1.0f, "inferred", "int * float => float" },
  }.filter( db::any<int>() * db::any<float>() == db::get("to_infer") );

  // compare two any's
  std::cout << "---\n";
  std::cout << db::Database{
    db::Record{ "a", 1, "b", 1, "should be found", "no" },
    db::Record{ "a", 1, "b", 0, "should be found", "yes" },
    db::Record{ "a", 0, "b", 1, "should be found", "yes" },
    db::Record{ "a", 0, "b", 0, "should be found", "no" },
  }.filter( db::any() < db::any<int>() );

  // call functions on database types
  std::cout << "---\n";
  std::cout << db::Database{
    db::Record{ "string", "a"s },
    db::Record{ "string", "ab"s },
    db::Record{ "string", "abc"s },
  }.filter( db::invoke(&std::string::size, db::get<std::string>("string")) == 3UL );

  // call functions on proxy types
  std::cout << "---\n";
  std::cout << db::Database{
    db::Record{ "x", 1, "w", 3, "included", "yes" },
    db::Record{ "y", 2, "included", "yes" },
    db::Record{ "z", 3, "included", "no" },
  }.filter(
    db::invoke(std::multiplies{},
               -db::invoke(std::plus{}, db::any<int>(), db::any<int>()),
               2)
      == -8
  );
}
