# lazy-query

A rudimentary implementation of a basic expression template system,
used for filtering a database of dynamically typed records with a rudimentary DSL.

Treat this as a version 0.1; it sort of functions,
but is not yet fit for any purpose whatsoever :)
In particular, don't assume there'll be any sort of API stability at this point.

## Requirements, Compilation

Requires any conforming C++17 compiler,
no extensions or third-party libraries needed.
Tested with GCC 10.1 and Clang 10.0.
There is no build system included, set it up as needed for your system.
For example:

    $ clang++ -std=c++17 -Wall -Wextra expr_template.cpp -o expr_template
    $ ./expr_template

I've noted that the error messages when the DSL is misused
are currently a little better with Clang than GCC,
since Clang respects the `static_assert`s more.

## Description

A `db::Record` is a dynamically typed key-value map.
All keys are `std::string`s, but the values can be any C++ type,
as long as it is copyable and printable (using `operator<<`.)
To retrieve a value you must know both the key for that value
and the type of the value that you stored.
A `db::Database` stores a collection of records;
intuitively, each record is a row in a NoSQL database.

The key part of this API is the function `db::Database::filter`.
This takes a query written in an embedded DSL, described below,
and returns a new database with only the records matching the query.
The function will not perform copies if called on an rvalue database.
This allows successive filters to be chained with only a small performance impact.

The DSL is a basic query language.
Information from a record is extracted using one of the provided free functions,
and this information can be composed using most standard C++ operators.
The final result of the query must be implicitly convertible to `bool`:
this is used to determine if the record should be filtered out.

Due to the dynamic nature of the records, the DSL requires type information.
Lots of the time the language will infer type from context; for example, in
`db::get("a") == 5` the query will check if `a` is an `int` equal to `5`.
However, this is not always possible, and occasionally it gets it wrong.
In this case you can supply an explicit type, `db::get<int>("a")`.

## Query Operations

Currently, the available functions are:

- `db::constant<[type]>(value)`: represents a constant value in the AST.
  This is usually unnecessary, as the system can generally
  create constant nodes in the AST automatically where required.
- `db::get<[type]>(key)`: this gets the value associated with `key`.
  The `type`, if left out, can often be inferred from context.
  If not, or the inferred type is wrong, you can provide it explicitly.
- `db::has_key(key)`: a simple boolean operation,
  returns whether the given `key` exists in the record (with any value type).
- `db::any`: a proxy value with purely inferred type information.
  Determines if _any_ value in the record matching the inferred type
  fulfils a comparison.
- `db::any_t<type>`: a proxy value with given type.
  Determines if _any_ value in the record matching the provided type
  fulfills a comparison.

These can largely be combined with each other and compared using:

- the comparison operators `<`, `>`, `<=`, `>=`, `==`, `!=`
- the arithmetic operators `+`, `-`, `*`, `/` and `%`
- the bitwise operators `&`, `|`, `^`, and `~`
- the relational operators `&&`, `||`, and `!`

This list may be expanded in time.

## Examples

The included `main` function has a few examples
showing off the features of the DSL.
