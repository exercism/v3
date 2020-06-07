using REPL.REPLCompletions: emoji_symbols

import Base: +, -, show

struct Clock
    t::Rational{Int}

    function Clock(t::Rational{Int})
        new(mod(t, 12))
    end
end

+(c₁::Clock, c₂::Clock) = Clock(c₁.t + c₂.t)
-(c₁::Clock, c₂::Clock) = Clock(c₁.t - c₂.t)

# function show(io::IO, c::Clock)
#     if c.t % 2 == 0
#         show(io, emoji_symbols["\\:clock$(Int(c.t)):"])
#     else
#         t = Int(c.t - 1//2)
#         show(io, emoji_symbols["\\:clock$(t)30:"])
#     end
# end

for i in 1:12
    @eval const $(Symbol(emoji_symbols["\\:clock$i:"])) = Clock($i//1)
    @eval const $(Symbol(emoji_symbols["\\:clock$(i)30:"])) = Clock($i + 1//2)
end
