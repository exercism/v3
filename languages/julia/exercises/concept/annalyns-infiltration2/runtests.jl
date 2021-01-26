using Test

include("game.jl")

# This exercise isn't about mathematical methods to determine the distribution of a random sample,
# therefore we only test that all possible values are returned at least once.

@testset "fog" begin
    @test Set(is_foggy() for _ in 1:1000) == Set([true, false])
end

@testset "distracted dog" begin
    @test Set(is_dog_distracted() for _ in 1:1000) == Set([true, false])
end

@testset "loot purse" begin
    @test Set(loot() for _ in 1:1000) == Set(3:13)
end

@testset "loot crate" begin
    crate = Set(["Cabbage", "Daring Dagger", "Sneaky Shoes"])
    @test Set(loot(crate) for _ in 1:1000) == crate
end
