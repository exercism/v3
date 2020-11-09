abstract type Pet end

struct Dog <: Pet
    name::AbstractString
end

struct Cat <: Pet
    name::AbstractString
end

encounter(a, b) = "$(name(a)) meets $(name(b)) and $(meets(a, b))."

# name
name(p::Pet) = p.name

# fallbacks
meets(a::Pet, b::Pet) = "is cautious"
meets(a::Pet, b) = "runs away"
meets(a, b) = "nothing happens"

# specific types
meets(a::Dog, b::Dog) = "sniffs"
meets(a::Dog, b::Cat) = "chases"
meets(a::Cat, b::Dog) = "hisses"
meets(a::Cat, b::Cat) = "slinks"
