-module(booleans).

%%% Implement the function can_fast_attack/1
-spec can_fast_attack(KnightIsAwake :: boolean()) -> boolean().

%%% Implement the function can_spy/3
-spec can_spy(KnightIsAwake :: boolean(),
              ArcherIsAwake :: boolean(),
              PrisonerIsAwake :: boolean()) -> boolean().

%%% Implement the function can_signal_prisoner/2
-spec can_signal_prisoner(ArcherIsAwake :: boolean(),
                          PrisonerIsAwake :: boolean()) -> boolean().

%%% Implement the function can_free_prisoner/4
-spec can_free_prisoner(KnightIsAwake :: boolean(),
                        ArcherIsAwake :: boolean(),
                        PrisonerIsAwake :: boolean(),
                        PetDogIsPresent :: boolean()) -> boolean().
