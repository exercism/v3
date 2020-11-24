using Test

include("leap.jl")

@testset "year not divisible by 4 in common year" begin
    @test !isleapyear(2015)
end

@testset "year divisible by 2, not divisible by 4 in common year" begin
    @test !isleapyear(1970)
end

@testset "year divisible by 4, not divisible by 100 in leap year" begin
    @test isleapyear(1996)
end

@testset "year divisible by 4 and 5 is still a leap year" begin
    @test isleapyear(1960)
end

@testset "year divisible by 100, not divisible by 400 in common year" begin
    @test !isleapyear(2100)
end

@testset "year divisible by 100 but not by 3 is still not a leap year" begin
    @test !isleapyear(1900)
end

@testset "year divisible by 400 in leap year" begin
    @test isleapyear(2000)
end

@testset "year divisible by 400 but not by 125 is still a leap year" begin
    @test isleapyear(2400)
end

@testset "year divisible by 200, not divisible by 400 in common year" begin
    @test !isleapyear(1800)
end
