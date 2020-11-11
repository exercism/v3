import { checkStatus, checkInventory } from './grocer';

// ADD YOUR CODE BELOW HERE.

/**
 * Returns the service status as a boolean value
 * @return {boolean}
 */
export function isServiceOnline() {
  return checkStatus((serviceStatus) => {
    if (serviceStatus === 'ONLINE') {
      return true;
    }

    return false;
  });
}

/**
 * pick a fruit using the checkInventory API
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
 * @param {string} err
 * @param {Object} isAvailable
 * @return {string} whether the fruit was purchased 'PURCHASE' or 'NOOP'
 */
export function purchaseInventoryIfAvailable(err, isAvailable) {
  if (err) {
    throw new Error(err);
  }

  if (isAvailable) {
    return 'PURCHASE';
  } else {
    return 'NOOP';
  }
}

/**
 * pick a fruit, and if it is available, purchase it
 * @param {string} variety
 * @param {number} quantity
 * @return {string} whether the fruit was purchased 'PURCHASE' or 'NOOP'
 */
export function pickAndPurchaseFruit(variety, quantity) {
  return pickFruit(variety, quantity, purchaseInventoryIfAvailable);
}
