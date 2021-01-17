In this exercise, you'll be implementing the game logic for the game of `pokemon mania` that your team of friend is developing.The game of pokemon will have trainers 1-1 battle with their pokemon.You are the incharge of developing this particular logic.You have to help the team to make the fight and add some new features to it

After sometimes your teams decides that each pokemon will have the different kind of stats in the form of the object with different values.These are task assigned to you.The values can be null or a number

## Tasks

### 1. Check if the Pokemon has the chance attacking with the same attack with same value.

Implement a function named `checkPokemonFight` that takes two values in the form of pokemon details as the object.This function will return the new pokemon data with the same properties and value.

```javascript
const pokemon1 = { name: 'charmeleon', hp: 22, absorb: null }
const pokemon2 = { name: 'charmander', hp: '22', absorb: null }
checkPokemonFight(pokemon1, pokemon2) //=>{hp:"22",absorb:null}
```

### 2- Find the stats details with valid value and that are not null from the result you obtained above

Implement a function named `validCommonDetails` that takes an object from the above the result and return the object with valid value.

```javascript
const pokemonDetails = { hp: "22", absorb: null }
validCommonDetails(pokemonDetails) //=>{hp:"22"}
```

### 3- You need to check if the number exist in the value of details in order to show the message of draw and not another move is possible from the result you obtained above

Implement a function named `findSecretNumber` that takes a Number and the above result as an object.The function return `true` if the number exists in the stats otherwise return `false`:

```javascript
const commonDetails={hp:"22"};
findSecretNumber(22,commonDetails)//=>true
```