import {
  checkPokemonData,
  validCommonDetails,
  findSecretNumber,
} from './pokemon-mania';

describe('equality', () => {
  describe('First Input', () => {
    const pokemon1 = {
      name: 'Pikachu',
      staticShock: '30',
      xp: 246,
      height: 4,
      weight: 120,
      hustle: null,
      lightingRod: '42',
      swarm: null,
      specialAttack: 'Tackle',
    };
    const pokemon2 = {
      name: 'Raichu',
      staticShock: '30',
      xp: 246,
      height: 8,
      weight: 120,
      hustle: null,
      lightingRod: '42',
      swarm: null,
      rage: '45',
      specialAttack: 'Tackle',
    };
    const expectedValue = {
      static: 30,
      xp: 246,
      weight: 120,
      hustle: null,
      lightingRod: 42,
      swarm: null,
      specialAttack: 'Tackle',
    };

    test(`checkPokemonData(${pokemon1},${pokemon2})`, () => {
      expect(checkPokemonData(pokemon1, pokemon2)).toEqual(expectedValue);
    });
    const expected = {
      static: '30',
      xp: 246,
      weight: 120,
      lightingRod: '42',
      specialAttack: 'Tackle',
    };
    test(`validCommonDetails(${expectedValue})`, () => {
      expect(validCommonDetails(expectedValue)).toEqual(expected);
    });
    test(`validCommonDetails(${expected})`, () => {
      expect(findSecretNumber(42, expected)).toBe(true);
    });
  });
  describe('Second Input', () => {
    const pokemon1 = {
      name: 'Vulpix',
      drought: '23',

      xp: 90,
      height: 6,
      weight: 100,
      hustle: null,
      lightingRod: null,
      swarm: null,
      specialAttack: 'Ember',
      flashFire: 40,
    };
    const pokemon2 = {
      name: 'Charmander',
      xp: 63,
      height: 6,
      weight: 100,
      hustle: null,
      lightingRod: null,
      swarm: null,
      torrent: null,
      blaze: '20',
      growl: '54',
      specialAttack: 'Solar Power',
    };
    const expectedValue = {
      height: 6,
      weight: 100,
      hustle: null,
      lightingRod: null,
      swarm: null,
    };

    test(`checkPokemonData(${pokemon1},${pokemon2})`, () => {
      expect(checkPokemonData(pokemon1, pokemon2)).toEqual(expectedValue);
    });
    const expected = { height: 6, weight: 100 };
    test(`validCommonDetails(${expectedValue})`, () => {
      expect(validCommonDetails(expectedValue)).toEqual(expected);
    });
    test(`validCommonDetails(${expected})`, () => {
      expect(findSecretNumber(23, expected)).toBe(false);
    });
  });
});
