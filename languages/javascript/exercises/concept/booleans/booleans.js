/**
 * If the knight is sleeping, then the fast attack is not available.
 *
 * @param {boolean} knightIsAwake
 * 
 * @return {boolean} Whether or not you can execute a fast attack.
 */
export function canExecuteFastAttack(knightIsAwake) {
  throw new Error('Implement the canExecuteFastAttack function')
}

/**
 * You can only spy if someone is awake; you wouldn't want to go through all the trouble just to find out nothing's going to happen, would you?
 *
 * @param {boolean} knightIsAwake
 * @param {boolean} archerIsAwake
 * @param {boolean} prisonerIsAwake
 * 
 * @returns {boolean} Whether or not you can spy on someone.
 */
export function canSpy(knightIsAwake, archerIsAwake, prisonerIsAwake) {
  throw new Error('Implement the canSpy function')
}

/**
 * If you signal to the prisoner while the archer is awake, the archer will catch you! However, the prisoner also has to be awake for them to receive your signal!
 *
 * @param {boolean} archerIsAwake
 * @param {boolean} prisonerIsAwake
 * 
 * @returns {boolean} Whether or not you can send a signal to the prisoner.
 */
export function canSignalPrisoner(archerIsAwake, prisonerIsAwake) {
  throw new Error('Implement the canSignalPrisoner function')
}

/**
 * @param {boolean} knightIsAwake
 * @param {boolean} archerIsAwake
 * @param {boolean} prisonerIsAwake
 * @param {boolean} petDogIsPresent
 */
export function canFreePrisoner(knightIsAwake, archerIsAwake, prisonerIsAwake, petDogIsPresent) {
  throw new Error('Implement the canFreePrisoner function')
}

