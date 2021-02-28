// @ts-check
//
// The line above enables type checking for this file. Various IDEs interpret
// the @ts-check directive. It will give you helpful autocompletion when
// implementing this exercise.

/**
 * Create a function which returns a object making the use of comparison to find the stats with the same value
 *
 * @param {Object} pokemon1 the stats of the first pokemon
 * @param {Object} pokemon2 the stats of the second pokemon
 *
 * @returns {Object} a object with the common stats and also with the same value
 *
 */

export function checkPokemonData(pokemon1, pokemon2) {
  const result = {};
  for (let key in pokemon1) {
    if (pokemon2[key] !== undefined) {
      if (pokemon2[key] == pokemon1[key]) {
        result[key] = pokemon1[key];
      }
    }
  }
  return result;
}

/**
 * Create a function which removes the empty or undefined data which return the object with the values
 *
 * @param {Object} pokemonData the common stats in both pokemon with equal value
 *
 * @returns  {Object} an object with the valid key value pair
 *
 */

export function validCommonDetails(pokemonData) {
  let result = {};
  for (let key in pokemonData) {
    if (pokemonData[key]) {
      result[key] = pokemonData[key];
    }
  }
  return result;
}

/**
 * Create the function which check if the particular number exist in the value of the common object and return true if the secret number exists
 *
 * @param {number} secretNumber the secretNumber that your logic check if exists in value of object
 * @param {Object} pokemonData the pokemon data with the valid values to check for number
 *
 * @return {Boolean} boolean if the secretNumber exists in the data
 *
 */

export function findSecretNumber(secretNumber, pokemonData) {
  for (let key in pokemonData) {
    if (pokemonData[key] == secretNumber) return true;
  }
  return false;
}
