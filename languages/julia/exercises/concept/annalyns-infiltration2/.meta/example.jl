is_foggy() = rand(Bool)
is_dog_distracted() = rand() < 0.25
loot() = rand(3:13)
loot(crate) = rand(crate)
