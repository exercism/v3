using Test

include("vehicle-purchase.jl")

@testset "Affordability" begin
    @test canibuy("1974 Ford Pinto", 516, 100) == "Yes! I'm getting a 1974 Ford Pinto."
    @test canibuy("2014 Bugatti Veyron", 562_500, 5000) == "Damn! No 2014 Bugatti Veyron for me."
    @test canibuy("2020 Gazelle Medeo", 3000, 50) == "I'll have to be frugal if I want a 2020 Gazelle Medeo."
end

@testset "Licence" begin
    @test licence("2014 Bugatti Veyron", "car") == "The 2014 Bugatti Veyron requires a licence to operate."
    @test licence("2020 Gazelle Medeo", "bike") == "The 2020 Gazelle Medeo requires no licence to operate."
end

@testset "Registration Fees" begin
    @test fee(  562_500,  6,  "car") == 2250
    @test fee(   25_000,  3,  "car") ==  175
    @test fee(   34_000, 30,  "car") ==   25
    @test fee(    3_000,  0, "bike") ==    0
end
