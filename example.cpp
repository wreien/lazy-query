#include <iostream>
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
    db::any <= 3 and (db::get("first"s) == 1 or db::get("now"s) == "we"s));

  // chained builder-style
  std::cout << "---\n";
  auto other = std::move(d).filter(db::any == 3).filter(db::has_key("first"s));
  std::cout << other;

  // complex arithmetic
  std::cout << "---\n";
  std::cout << db::Database{
    db::Record{ "a", 1, "b", 2, "c", 3 },
    db::Record{ "a", 2, "b", 2, "c", 2 },
    db::Record{ "a", 3, "b", 2, "c", 1 },
    db::Record{ "a", 4, "b", 2, "c", 10 },
  }.filter( db::any * 2 > 15 or 3 - -db::get<int>("c") < 2 * db::get("a") );

  // manual type specification
  std::cout << "---\n";
  std::cout << db::Database{
    db::Record{ "a", true, "b", "included" },
    db::Record{ "a", false, "b", "excluded" },
  }.filter(db::get<bool>("a"));
  std::cout << db::Database{
    db::Record{ "a", true, "b", "included" },
    db::Record{ "a", false, "b", "excluded" },
  }.filter(db::any_t<bool>);

  // type inference
  std::cout << "---\n";
  std::cout << db::Database{
    db::Record{ "x", 1.f, "to_infer", 1, "inferred", "int * float => int" },
    db::Record{ "x", 1, "to_infer", 1.0f, "inferred", "int * float => float" },
  }.filter( db::any_t<int> * db::any_t<float> == db::get("to_infer") );
}