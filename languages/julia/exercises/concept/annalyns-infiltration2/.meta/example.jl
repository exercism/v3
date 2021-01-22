can_do_fast_attack(knight_awake) = !knight_awake

is_foggy() = rand(Bool)

can_spy(knight_awake, archer_awake, prisoner_awake) = !is_foggy() && (knight_awake || archer_awake || prisoner_awake)
can_signal_prisoner(archer_awake, prisoner_awake) = prisoner_awake && !archer_awake

is_dog_distracted() = rand() < 0.25

function can_free_prisoner(knight_awake, archer_awake, prisoner_awake, dog_present)
    (!knight_awake && !archer_awake && prisoner_awake) || (dog_present && !is_dog_distracted() && !archer_awake)
end

loot() = rand(3:13)
