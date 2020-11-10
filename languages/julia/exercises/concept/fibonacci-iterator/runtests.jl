using Test

include("fibonacci.jl")

const fib = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169, 63245986, 102334155, 165580141, 267914296, 433494437, 701408733, 1134903170, 1836311903, 2971215073, 4807526976, 7778742049, 12586269025]

@testset "Fib is defined" begin
    @test isdefined(@__MODULE__, :Fib)
    @test typeof(Fib(100)) == Fib
end

@testset "Can be iterated" begin
    @test [a for a in Fib(50)] == fib
end

@testset "Can be collected" begin
    @test collect(Fib(50)) == fib
    @test Base.IteratorSize(Fib) == Base.HasLength()
end

@testset "Has an eltype" begin
    @test typeof(collect(Fib(50))) == Vector{Int}
    @test Base.IteratorEltype(Fib) == Base.HasEltype()
end
