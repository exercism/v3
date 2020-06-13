package booleans


//canExecuteFastAttack indicates if the knight is awake.
func canExecuteFastAttack(knightIsAwake bool) bool {
    return false
}

//canSpy indicates if the knight, archer and the prisoner, respectively, are awake
func  canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake bool) bool {
    return true
}

//canSignalPrisoner checks you can send a signal. You'll get caught by the archer if you signal while they're awake
func canSignalPrisoner(archerIsAwake, prisonerIsAwake bool) bool {
   return true
}

//canFreePrisoner indicates if the knight, archer and the prisoner, respectively, are awake and if Annalyn's pet dog is present. 
// it returns wether or not you can free Annalyn's friend
func canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent bool) bool {
   return false
}