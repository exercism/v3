# Step 1
struct Fib
    n::Int
end

# Step 2
function Base.iterate(fib::Fib, state=(i=1, vals=(1, 1)))
    state.i > fib.n && return nothing

    if state.i <= 2
        return 1, (i=state.i + 1, vals=(1, 1))
    end

    aₙ₋₂, aₙ₋₁ = state.vals
    aₙ = aₙ₋₂ + aₙ₋₁

    aₙ, (i=state.i + 1, vals=(aₙ₋₁, aₙ))
end

# Step 3
Base.length(fib::Fib) = fib.n

# Step 4
Base.eltype(::Type{Fib}) = Int
