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

for i in 1:12
    @eval const $(Symbol(emoji_symbols["\\:clock$i:"])) = Clock($i//1)
    @eval const $(Symbol(emoji_symbols["\\:clock$(i)30:"])) = Clock($i + 1//2)
end
