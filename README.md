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
The library itself is a single header, `lazy_query.hpp`;
include it in your project and you're done.

An example is provided as well.
It may be compiled and run using something like the following:

    $ clang++ -std=c++17 -Wall -Wextra example.cpp -o example
    $ ./example

I've noticed that the error messages when the DSL is misused
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
The final result of the query must be convertible to `bool`:
this is used to determine if the record should be filtered out.

Due to the dynamic nature of the records, the DSL requires type information.
Much of the time the language can infer the type from context;
for example, in `db::get("a") == 5`
the query will check if `a` is an `int` equal to `5`.
However, this is not always possible, and occasionally it gets it wrong.
If this is the case you can supply an explicit type, such as `db::get<int>("a")`.

## Query Operations

Currently, the available functions are:

- `db::constant<[type]>(value)`: represents a constant value in the AST.
  This is usually unnecessary, as the system can generally
  create constant nodes in the AST automatically where required.
- `db::get<[type]>(key)`: this gets the value associated with `key`.
  The `type`, if left out, can often be inferred from context.
  If not, or if the inferred type is wrong, you can provide it explicitly.
- `db::has_key<[type]>(key)`: a simple boolean operation, which
  returns whether the given `key` exists in the record.
  If `type` is provided returns true only if the key has the given type,
  otherwise returns that key exists with _any_ type.
- `db::any<[type]>()`: a proxy value, with possibly inferred type.
  Determines if _any_ value in the record with given type fulfils a comparison.
- `db::all<[type]>()`: a proxy value, with possibly inferred type.
  Determines if _all_ values in the record with given type fulfil a comparison.
- `db::invoke(func, args...)`: invoke `func` with given `args`, returning the result.
  This is a late-binding call, and is only invoked at filter time.
  Support for type inference is patchy as yet.
  `func` can be any callable, as with `std::invoke`.

The expressions returned from these functions can be
combined and compared using:

- the comparison operators `<`, `>`, `<=`, `>=`, `==`, `!=`
- the arithmetic operators `+`, `-`, `*`, `/` and `%`
- the bitwise operators `&`, `|`, `^`, and `~`
- the relational operators `&&`, `||`, and `!`

This list may be expanded in time.

## Examples

The included `example.cpp` file demonstrates
a number of the DSL's features.

## Future Work

In approximate order of likelihood...

- More extractors and operations
- Flesh out the database and record types
- Better error messages
- A comprehensive test suite
- Allow mutating operations, or a `map` function
- Generalise the DSL to arbitrary record designs
- ...
- An actually useful product
