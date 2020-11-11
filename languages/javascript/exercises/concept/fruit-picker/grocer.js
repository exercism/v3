/**
 * STORE STATUS API
 */

let STATUS = 'OFFLINE';

/**
 * For testing purposes, set the store status
 * @param  {string} status
 */
export function setStatus(status) {
  STATUS = status;
}

/**
 * For testing purposes, reset the store status
 */
export function resetStatus() {
  STATUS = 'OFFLINE';
}

/**
 * This callback type is called `statusCallback`, is invoked when status api called
 *
 * @callback statusCallback
 * @param {string} response
 */

/**
 * Invokes the callback with the store's status to simulate an API call
 * @param  {statusCallback} callback
 */
export function checkStatus(callback) {
  return callback(STATUS);
}

/**
 * INVENTORY API
 */

let LAST_QUERY = undefined;
let RESPONSE = undefined;

/**
 * For testing purposes, set the response to return when queried
 * @param  {any} ...nextResponse
 */
export function setResponse(...nextResponse) {
  RESPONSE = nextResponse;
}

/**
 * For testing purposes, get the last query
 * @return {string}
 */
export function getLastQuery() {
  return LAST_QUERY;
}

/**
 * For testing purposes, reset the last query
 */
export function resetQuery() {
  LAST_QUERY = undefined;
  RESPONSE = ['undefined response'];
}

/**
 * This callback type is called `inventoryCallback`, is invoked when inventory api called
 *
 * @callback inventoryCallback
 * @param {(null|string)} err
 * @param {boolean} isAvailable
 */

/**
 * Checks the inventory (RESPONSE) then invokes the callback with the result
 * @param  {Object} query
 * @param  {string} query.variety
 * @param  {number} query.quantity
 * @param  {inventoryCallback} callback
 * @return {unknown} return the result of the callback
 */
export function checkInventory(query, callback) {
  LAST_QUERY = query;
  return callback.apply(null, RESPONSE);
}
