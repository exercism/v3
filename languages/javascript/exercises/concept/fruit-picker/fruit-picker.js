// @ts-check

import { checkStatus, checkInventory } from './grocer';

// ADD YOUR CODE BELOW HERE.

/**
 * Returns the service status as a boolean value
 * @return {boolean}
 */
export function isServiceOnline() {
  throw new Error('Implement the isServiceOnline function');
}

/**
 * pick a fruit using the checkInventory API
 * @param {string} variety
 * @param {string} quantity
 * @param {function} callback
 * @return {unknown} the result from checkInventory
 */
export function pickFruit(variety, quantity, callback) {
  throw new Error('Implement the pickFruit function');
}

/**
 * This is a callback function to be passed to the checkInventory API
 * handles the next step once the inventory is known
 * @param {string} err
 * @param {Object} isAvailable
 * @return {string} whether the fruit was purchased 'PURCHASE' or 'NOOP'
 */
export function purchaseInventoryIfAvailable(err, isAvailable) {
  throw new Error('Implement the purchaseInventoryIfAvailable function');
}

/**
 * pick a fruit, and if it is available, purchase it
 * @param {string} variety
 * @param {number} quantity
 * @return {string} whether the fruit was purchased 'PURCHASE' or 'NOOP'
 */
export function pickAndPurchaseFruit(variety, quantity) {
  throw new Error('Implement the pickAndPurchaseFruit function');
}
