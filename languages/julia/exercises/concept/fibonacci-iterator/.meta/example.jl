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

    aₙ = state.vals[1] + state.vals[2]

    aₙ, (i=state.i + 1, vals=(state.vals[2], aₙ))
end

# Step 3
Base.length(fib::Fib) = fib.n

# Step 4
Base.eltype(::Type{Fib}) = Int
