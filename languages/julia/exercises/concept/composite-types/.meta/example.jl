abstract type Chesspiece end

struct Pawn <: Chesspiece
    colour::Symbol

    function Pawn(colour)
        if colour in (:black, :white)
            new(colour)
        else
            throw(DomainError(colour, "colour must be :black or :white"))
        end
    end
end
