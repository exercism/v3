-module(example).

can_fast_attack(KnightIsAwake) ->
    not KnightIsAwake.

can_spy(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake) ->
    KnightIsAwake orelse ArcherIsAwake orelse PrisonerIsAwake.

can_signal_prisoner(ArcherIsAwake, PrisonerIsAwake) ->
    not ArcherIsAwake andalso PrisonerIsAwake.

can_free_prisoner(KnightIsAwake, ArcherIsAwake, PrisonerIsAwake, PetDogIsPresent) ->
    (not KnightIsAwake andalso not ArcherIsAwake andalso PrisonerIsAwake)
        orelse (ArcherIsAwake andalso PetDogIsPresent).
