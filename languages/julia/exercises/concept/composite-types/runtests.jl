using Test

include("robots.jl")

using Test

include("chesspieces.jl")

@testset "subtype" begin
    @test Pawn <: Chesspiece
end

@testset "colours" begin
    p = Pawn(:black)
    @test colour(p) == :black
    
    p = Pawn(:white)
    @test colour(p) == :white
end

@testset "colour validation" begin
    @test_throws DomainError Pawn(:yellow)
end
