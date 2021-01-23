using Test

include("game.jl")

# Julia 1.0 compat
# The function definition of eachrow is taken from Julia Base,
# released under the MIT license: https://julialang.org/license
if VERSION < v"1.1"
    @eval eachrow(A) = (view(A, i, :) for i in axes(A, 1))
end

@testset "fast attack" begin
    @test !can_do_fast_attack(true)
    @test  can_do_fast_attack(false)
end

@testset "spying" begin
    character_state_combinations = Bool[
        0 0 0 0;
        0 0 1 1;
        0 1 0 1;
        0 1 1 1;
        1 0 0 1;
        1 0 1 1;
        1 1 1 1;
    ]

    for state in eachrow(character_state_combinations)
        @test can_spy(state[1:3]...) == state[4]
    end
end

@testset "signaling prisoner" begin
    character_state_combinations = Bool[
        0 0 0;
        0 1 1;
        1 0 0;
        1 1 0;
    ]

    for state in eachrow(character_state_combinations)
        @test can_signal_prisoner(state[1:2]...) == state[3]
    end
end

@testset "freeing prisoner" begin
    character_state_combinations = Bool[
        0 0 0 0 0;
        0 0 0 1 1;
        0 0 1 0 1;
        0 0 1 1 1;
        0 1 0 0 0;
        0 1 0 1 0;
        0 1 1 0 0;
        0 1 1 1 0;
        1 0 0 0 0;
        1 0 0 1 1;
        1 0 1 0 0;
        1 0 1 1 1;
        1 1 0 0 0;
        1 1 0 1 0;
        1 1 1 0 0;
        1 1 1 1 0;
    ]

    for state in eachrow(character_state_combinations)
        @test can_free_prisoner(state[1:4]...) == state[5]
    end
end
