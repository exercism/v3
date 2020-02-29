export function canExecuteFastAttack(knightIsAwake) {
  return !knightIsAwake;
}

export function canApproachAndSpy(knightIsAwake, archerIsAwake, prisonerIsAwake) {
  return knightIsAwake || archerIsAwake || prisonerIsAwake;
}

export function canSignalPrisoner(archerIsAwake, prisonerIsAwake) {
  return prisonerIsAwake && !archerIsAwake;
}

export function canReleasePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent) {
  return !knightIsAwake && !archerIsAwake && prisonerIsAwake || petDogIsPresent && !archerIsAwake;
}
