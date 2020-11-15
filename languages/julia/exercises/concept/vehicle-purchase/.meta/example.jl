# Step 1
function canibuy(vehicle, price, monthly_budget)
    if price / 60 < monthly_budget
        "Yes! I'm getting a $vehicle."
    elseif price / 60 > monthly_budget + 10
        "Damn! No $vehicle for me."
    else
        "I'll have to be frugal if I want a $vehicle."
    end
end

# Step 2
function licence(vehicle, kind)
    s = kind == "car" ? "a" : "no"
    "The $vehicle requires $s licence to operate."
end

# Step 3
function fee(msrp, age, kind)
    kind == "bike" && return 0
    age >= 10 && return 25

    value = maximum((msrp, 25000))
    percent = 1 - age / 10
    
    return value * percent / 100
end
