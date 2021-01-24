card(stack, idx) = stack[idx]

function replace_card!(stack, r)
    stack[r.first] = r.second
    stack
end

insert_card_at_top!(stack, new_card) = push!(stack, new_card)

function remove_card_from_top!(stack)
    pop!(stack)
    stack
end

insert_card_at_bottom!(stack, new_card) = pushfirst!(stack, new_card)

function remove_card_from_bottom!(stack)
    popfirst!(stack)
    stack
end

remove_card!(stack, idx) = deleteat!(stack, idx)

check_stack_size(stack, n) = length(stack) == n
