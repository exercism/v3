using Test

include("emoji-times.jl")

@testset "Addition" begin
    @test 🕐 + 🕒 == 🕓
    @test 🕒 + 🕧 == 🕞
end

@testset "Addition overflow" begin
    @test 🕚 + 🕚 == 🕙
end

@testset "Subtraction" begin
    @test 🕗 - 🕔 == 🕒
    @test 🕤 - 🕞 == 🕕
end

@testset "Subtraction underflow" begin
    @test 🕓 - 🕘 == 🕖
end

@testset "0-element" begin
    @test 🕚 + 🕛 == 🕚
    @test 🕚 - 🕛 == 🕚
end
