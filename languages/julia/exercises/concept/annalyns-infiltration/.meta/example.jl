can_do_fast_attack(knight_awake) = !knight_awake
can_spy(knight_awake, archer_awake, prisoner_awake) = knight_awake || archer_awake || prisoner_awake
can_signal_prisoner(archer_awake, prisoner_awake) = prisoner_awake && !archer_awake
can_free_prisoner(knight_awake, archer_awake, prisoner_awake, dog_present) = !knight_awake && !archer_awake && prisoner_awake || dog_present && !archer_awake
