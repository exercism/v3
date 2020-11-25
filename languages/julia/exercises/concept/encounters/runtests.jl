using Test

include("encounters.jl")

@testset "type abstraction" begin
    @test Dog <: Pet
    @test Cat <: Pet
end

# create some pets
buddy = Dog("Buddy")
sadie = Dog("Sadie")
minka = Cat("Minka")
felix = Cat("Felix")

@testset "names" begin
    @test name(buddy) == "Buddy"
    @test name(sadie) == "Sadie"
    @test name(minka) == "Minka"
    @test name(felix) == "Felix"
end

@testset "encounters" begin
    @test encounter(sadie, buddy) == "Sadie meets Buddy and sniffs."
    @test encounter(buddy, felix) == "Buddy meets Felix and chases."
    @test encounter(minka, sadie) == "Minka meets Sadie and hisses."
    @test encounter(felix, minka) == "Felix meets Minka and slinks."
end

# define a new type of Pet to test the fallback
# this belongs to the testset below but struct definitions within the local scope of testsets are not supported in Julia <1.1
struct Horse <: Pet
    name::String
end
name(h::Horse) = h.name

jenny = Horse("Jenny")

@testset "pet fallbacks" begin
    @test encounter(buddy, jenny) == "Buddy meets Jenny and is cautious."
    @test encounter(sadie, jenny) == "Sadie meets Jenny and is cautious."
    @test encounter(minka, jenny) == "Minka meets Jenny and is cautious."
    @test encounter(felix, jenny) == "Felix meets Jenny and is cautious."

    @test encounter(jenny, buddy) == "Jenny meets Buddy and is cautious."
    @test encounter(jenny, sadie) == "Jenny meets Sadie and is cautious."
    @test encounter(jenny, minka) == "Jenny meets Minka and is cautious."
    @test encounter(jenny, felix) == "Jenny meets Felix and is cautious."
end

struct Car
    license_plate::String
end
name(c::Car) = c.license_plate

car = Car("W-12345X")

@testset "non-pet fallback" begin
    @test encounter(buddy, car) == "Buddy meets W-12345X and runs away."
    @test encounter(minka, car) == "Minka meets W-12345X and runs away."
    @test encounter(jenny, car) == "Jenny meets W-12345X and runs away."
end

# Test generic fallback with a random type
# In practice this would be nonsense, but here we want to ensure the student doesn't implement a specific method for a given type
typename = Symbol("RandomType", rand(UInt))
@eval struct $typename end
@eval name(::$typename) = $(string(typename))
@eval t = $typename()

@testset "generic fallback" begin
    @test encounter(t, buddy) == "$(name(t)) meets Buddy and nothing happens."
    @test encounter(buddy, t) == "Buddy meets $(name(t)) and runs away."
end
