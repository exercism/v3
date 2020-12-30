// @ts-check

import { checkStatus, checkInventory } from './grocer';


/**
 * Returns the service status as a boolean value
 * @return {boolean}
 */
export function isServiceOnline() {
  return checkStatus((serviceStatus) => serviceStatus === 'ONLINE'))
}

/**
 * Pick a fruit using the checkInventory API
 *
 * @param {string} variety
 * @param {string} quantity
 * @param {function} callback
 * @return {unknown} the result from checkInventory
 */
export function pickFruit(variety, quantity, callback) {
  return checkInventory({ variety, quantity }, callback);
}

/**
 * This is a callback function to be passed to the checkInventory API
 * handles the next step once the inventory is known
 * @param {string | null} err
 * @param {boolean} isAvailable
 * @return {'PURCHASE' | 'NOOP'} whether the fruit was purchased 'PURCHASE' or 'NOOP'
 */
export function purchaseInventoryIfAvailable(err, isAvailable) {
  if (err) {
    throw new Error(err);
  }

  return isAvailable ? 'PURCHASE' : 'NOOP';
}

/**
 * Pick a fruit, and if it is available, purchase it
 *
 * @param {string} variety
 * @param {number} quantity
 * @return {'PURCHASE' | 'NOOP'} whether the fruit was purchased 'PURCHASE' or 'NOOP'
 */
export function pickAndPurchaseFruit(variety, quantity) {
  return pickFruit(variety, quantity, purchaseInventoryIfAvailable);
}
