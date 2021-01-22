using SimpleMock
using Test

include("game.jl")

@testset "fast attack" begin
    @test !can_do_fast_attack(true)
    @test  can_do_fast_attack(false)
end

@testset "fog" begin
    # This exercise isn't about mathematical methods to determine the distribution of a random sample,
    # therefore we only test that stumble() returns boolean values and not all return values are identical.
    fogs = collect(is_foggy() for _ in 1:1000)
    @test eltype(fogs) == Bool
    @test !all(fogs)
    @test any(fogs)
    mock(rand) do rnd
        is_foggy()
        @test called_once_with(rnd, Bool)
    end
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

    @testset "no fog" begin
        mock((is_foggy) => Mock(false)) do _
            for state in character_state_combinations
                @test can_spy(state[1:3]...) == state[4]
            end
        end
    end

    @testset "fog" begin
        mock((is_foggy) => Mock(true)) do _
            for state in character_state_combinations
                @test !can_spy(state[1:3]...)
            end
        end
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

@testset "distracted dog" begin
    # This exercise isn't about mathematical methods to determine the distribution of a random sample,
    # therefore we only test that is_dog_distracted() returns boolean values and not all return values are identical.
    @test Set(is_dog_distracted() for _ in 1:1000) == Set([true, false])
end

@testset "freeing prisoner" begin
    character_state_combinations_with_dog = [
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

    @testset "dog is not distracted" begin
        mock((is_dog_distracted) => Mock(false)) do _
            for state in character_state_combinations_with_dog
                @test can_free_prisoner(state[1:4]...) == state[5]
            end
        end
    end

    character_state_combinations_without_dog = [
        (false, false, false, false, false),
        (false, false, false, true, false), # this situation is solely determined by the dog's presence
        (false, false, true, false, true),
        (false, false, true, true, true),
        (false, true, false, false, false),
        (false, true, false, true, false),
        (false, true, true, false, false),
        (false, true, true, true, false),
        (true, false, false, false, false),
        (true, false, false, true, false), # this situation is solely determined by the dog's presence
        (true, false, true, false, false),
        (true, false, true, true, false), # this situation is solely determined by the dog's presence
        (true, true, false, false, false),
        (true, true, false, true, false),
        (true, true, true, false, false),
        (true, true, true, true, false),
    ]

    @testset "dog is distracted" begin
        mock((is_dog_distracted) => Mock(true)) do _
            for state in character_state_combinations_without_dog
                @test can_free_prisoner(state[1:4]...) == state[5]
            end
        end
    end
end

@testset "loot" begin
    # This exercise isn't about mathematical methods to determine the distribution of a random sample,
    # therefore we only test that stumble() returns Int values and contains at least one of each possible number.
    loots = collect(loot() for _ in 1:1000)
    @test eltype(loots) == Int
    @test 3:13 ⊆ loots
end
