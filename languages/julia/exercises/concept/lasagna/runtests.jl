using Test

include("lasagna.jl")

@testset "preparation time" begin
    @test preptime(2) == 4
    @test preptime(3) == 6
    @test preptime(8) == 16
end

@testset "remaining time" begin
    @test remaining_time(30) == 30
    @test remaining_time(50) == 10
    @test remaining_time(60) == 0
end

@testset "total working time" begin
    @test total_working_time(3, 20) == 26
end
