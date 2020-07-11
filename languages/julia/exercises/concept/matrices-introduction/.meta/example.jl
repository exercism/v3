const E = [
    0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    0 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 1 0;
    0 1 0 0 1 0 1 0 0 0 0 1 0 1 0 0 1 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1;
    0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0;
    0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 1 0;
    0 1 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 0;
    0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0;
    0 0 1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0;
]

function frown!(E)
    E[7, 7] = E[7, 12] = E[9, 9] = E[9, 10] = 0
    E[9, 7] = E[9, 12] = E[7, 9] = E[7, 10] = 1
    E
end

frown(E) = frown!(copy(E))

rot270(E) = E'

rot90(E) = rotr90(E)

stickerwall(E) = vcat(
    hcat(E, zeros(Int, size(E, 1)), ones(Int, size(E, 1)), zeros(Int, size(E, 1)), vcat(frown(E))),
    zeros(Int, 2size(E, 2) + 3)',
    ones(Int, 2size(E, 2) + 3)',
    zeros(Int, 2size(E, 2) + 3)',
    hcat(vcat(frown(E)), zeros(Int, size(E, 1)), ones(Int, size(E, 1)), zeros(Int, size(E, 1)), E)
)
