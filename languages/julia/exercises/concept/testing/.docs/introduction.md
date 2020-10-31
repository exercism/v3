<!-- TODO: Some explanation why tests are important or something -->

## Test macros

Julia's standard library provides the `Test` module for basic unit testing.

You can load the module using the `using` keyword:

```julia
julia> using Test

```

The `@test` macro[^1] is used for testing that the given expression evaluates to true:

[^1]: For this exercise, you don't need to worry about the differences between macros and functions in Julia. All you need to know is that they take a boolean expression as argument. Macros will be covered in more detail later in the track.<!--TODO: Check if there are actually exercises that cover macros before launch-->

```julia
julia> @test true
Test Passed

julia> @test 2 + 2 != 5
Test Passed
```

You can test that an expression throws an exception with the `@test_throws` macro:

```julia
julia> sqrt(-1)
ERROR: DomainError with -1.0:
sqrt will only return a complex result if called with a complex argument. Try sqrt(Complex(x)).
Stacktrace:
[...]

julia> @test_throws DomainError sqrt(-1)
Test Passed
      Thrown: DomainError
```

Other test macros, for example for marking tests as broken, are available but you will not need them in this exercise.

## Testsets

Related tests can be named and grouped together using a `@testset`:

```julia
julia> @testset "Addition" begin
           @test 1  + 1 ==  2
           @test 40 + 2 == 42
       end
Test Summary: | Pass  Total
Addition      |    2      2
```

Testsets can be nested inside each other:

```julia
julia> @testset "Arithmetics" begin
           @testset "Addition" begin
               @test 1  + 1 ==  2
               @test 40 + 2 == 42
           end
           @testset "Multiplication" begin
               @test 3 * 3 == 9
               @test 3 * 0 == 0
           end
       end
Test Summary: | Pass  Total
Arithmetics   |    4      4
```

All tests within a testset will be evaluated before the result will be displayed:

```julia
julia> @testset "Arithmetics" begin
           @testset "Addition" begin
               @test 1  + 1 ==  2
               @test 40 + 2 == 42
           end
           @testset "Multiplication" begin
               @test 3 * 3 == 9
               @test 1 * 1 == 2 # this test will fail
               @test 3 * 0 == 0
           end
       end
Multiplication: Test Failed at REPL[15]:8
  Expression: 1 * 1 == 2
   Evaluated: 1 == 2
Stacktrace:
[...]
Test Summary:    | Pass  Fail  Total
Arithmetics      |    4     1      5
  Addition       |    2            2
  Multiplication |    2     1      3
ERROR: Some tests did not pass: 4 passed, 1 failed, 0 errored, 0 broken.
```

## Source

The descriptions in this file are derived from the [Julia v1.5.2 Documentation](https://docs.julialang.org/en/v1.5.2/stdlib/Test/).
