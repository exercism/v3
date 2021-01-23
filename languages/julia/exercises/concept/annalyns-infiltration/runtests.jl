using Test

include("game.jl")

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
