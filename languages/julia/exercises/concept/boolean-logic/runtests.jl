using Test

include("game-logic.jl")

@testset "fast attack" begin
    @test !can_do_fast_attack(true)
    @test  can_do_fast_attack(false)
end

@testset "spying" begin
    character_state_combinations = [
        (false, false, false, false),
        (false, false, true, true),
        (false, true, false, true),
        (false, true, true, true),
        (true, false, false, true),
        (true, false, true, true),
        (true, true, true, true),
    ]

    for state in character_state_combinations
        @test can_spy(state[1:3]...) == state[4]
    end
end

@testset "signaling prisoner" begin
    character_state_combinations = [
        (false, false, false),
        (false, true, true),
        (true, false, false),
        (true, true, false),
    ]

    for state in character_state_combinations
        @test can_signal_prisoner(state[1:2]...) == state[3]
    end
end

@testset "freeing prisoner" begin
    character_state_combinations = [
        (false, false, false, false, false),
        (false, false, false, true, true),
        (false, false, true, false, true),
        (false, false, true, true, true),
        (false, true, false, false, false),
        (false, true, false, true, false),
        (false, true, true, false, false),
        (false, true, true, true, false),
        (true, false, false, false, false),
        (true, false, false, true, true),
        (true, false, true, false, false),
        (true, false, true, true, true),
        (true, true, false, false, false),
        (true, true, false, true, false),
        (true, true, true, false, false),
        (true, true, true, true, false),
    ]

    for state in character_state_combinations
        @test can_free_prisoner(state[1:4]...) == state[5]
    end
end
