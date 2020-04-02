-module(booleans_tests).
-include_lib("eunit/include/eunit.hrl").

cannot_execute_fast_attack_if_knight_is_awake_test() ->
    KnightIsAwake = true,
    ?assertNot(booleans:can_fast_attack(KnightIsAwake)).

can_execute_fast_attack_if_knight_is_sleeping_test() ->
    KnightIsAwake = false,
    ?assert(booleans:can_fast_attack(KnightIsAwake)).

cannot_spy_if_everyone_is_sleeping_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = false,
    PrisonerIsAwake = false,
    ?assertNot(booleans:can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake)).

can_spy_if_everyone_but_knight_is_sleeping_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = false,
    PrisonerIsAwake = false,
    ?assert(booleans:can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake)).

can_spy_if_everyone_but_archer_is_sleeping_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = true,
    PrisonerIsAwake = false,
    ?assert(booleans:can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake)).

can_spy_if_everyone_but_prisoner_is_sleeping_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = false,
    PrisonerIsAwake = true,
    ?assert(booleans:can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake)).

can_spy_if_only_knight_is_sleeping_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = true,
    PrisonerIsAwake = true,
    ?assert(booleans:can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake)).

can_spy_if_only_archer_is_sleeping_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = false,
    PrisonerIsAwake = true,
    ?assert(booleans:can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake)).

can_spy_if_only_prisoner_is_sleeping_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = true,
    PrisonerIsAwake = false,
    ?assert(booleans:can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake)).

can_spy_if_everyone_is_awake_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = true,
    PrisonerIsAwake = true,
    ?assert(booleans:can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake)).

can_signal_prisoner_if_archer_is_sleeping_and_prisoner_is_awake_test() ->
    ArcherIsAwake = false,
    PrisonerIsAwake = true,
    ?assert(booleans:can_signal_prisoner(ArcherIsAwake, PrisonerIsAwake)).

cannot_signal_prisoner_if_archer_is_awake_and_prisoner_is_sleeping_test() ->
    ArcherIsAwake = true,
    PrisonerIsAwake = false,
    ?assertNot(booleans:can_signal_prisoner(ArcherIsAwake, PrisonerIsAwake)).

cannot_signal_prisoner_if_archer_and_prisoner_are_both_sleeping_test() ->
    ArcherIsAwake = false,
    PrisonerIsAwake = false,
    ?assertNot(booleans:can_signal_prisoner(ArcherIsAwake, PrisonerIsAwake)).

cannot_signal_prisoner_if_archer_and_prisoner_are_both_awake_test() ->
    ArcherIsAwake = true,
    PrisonerIsAwake = true,
    ?assertNot(booleans:can_signal_prisoner(ArcherIsAwake, PrisonerIsAwake)).

cannot_release_prisoner_if_everyone_is_awake_and_pet_dog_is_present_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = true,
    PrisonerIsAwake = true,
    PetDogIsPresent = true,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_everyone_is_awake_and_pet_dog_is_absent_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = true,
    PrisonerIsAwake = true,
    PetDogIsPresent = false,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

can_release_prisoner_if_everyone_is_asleep_and_pet_dog_is_present_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = false,
    PrisonerIsAwake = false,
    PetDogIsPresent = true,
    ?assert(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_everyone_is_asleep_and_pet_dog_is_absent_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = false,
    PrisonerIsAwake = false,
    PetDogIsPresent = false,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

can_release_prisoner_if_only_prisoner_is_awake_and_pet_dog_is_present_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = false,
    PrisonerIsAwake = true,
    PetDogIsPresent = true,
    ?assert(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

can_release_prisoner_if_only_prisoner_is_awake_and_pet_dog_is_absent_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = false,
    PrisonerIsAwake = true,
    PetDogIsPresent = false,
    ?assert(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_only_archer_is_awake_and_pet_dog_is_present_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = true,
    PrisonerIsAwake = false,
    PetDogIsPresent = true,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_only_archer_is_awake_and_pet_dog_is_absent_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = true,
    PrisonerIsAwake = false,
    PetDogIsPresent = false,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

can_release_prisoner_if_only_knight_is_awake_and_pet_dog_is_present_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = false,
    PrisonerIsAwake = false,
    PetDogIsPresent = true,
    ?assert(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_only_knight_is_awake_and_pet_dog_is_absent_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = false,
    PrisonerIsAwake = false,
    PetDogIsPresent = false,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_only_knight_is_asleep_and_pet_dog_is_present_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = true,
    PrisonerIsAwake = true,
    PetDogIsPresent = true,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_only_knight_is_asleep_and_pet_dog_is_absent_test() ->
    KnightIsAwake = false,
    ArcherIsAwake = true,
    PrisonerIsAwake = true,
    PetDogIsPresent = false,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

can_release_prisoner_if_only_archer_is_asleep_and_pet_dog_is_present_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = false,
    PrisonerIsAwake = true,
    PetDogIsPresent = true,
    ?assert(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_only_archer_is_asleep_and_pet_dog_is_absent_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = false,
    PrisonerIsAwake = true,
    PetDogIsPresent = false,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_only_prisoner_is_asleep_and_pet_dog_is_present_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = true,
    PrisonerIsAwake = false,
    PetDogIsPresent = true,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).

cannot_release_prisoner_if_only_prisoner_is_asleep_and_pet_dog_is_absent_test() ->
    KnightIsAwake = true,
    ArcherIsAwake = true,
    PrisonerIsAwake = false,
    PetDogIsPresent = false,
    ?assertNot(booleans:can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent)).
