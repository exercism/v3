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
 * Invokes the callback with the store's status to simulate an API call
 * @param  {StatusCallback} callback
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
 * Checks the inventory (RESPONSE) then invokes the callback with the result
 * @param  {GrocerQuery} query
 * @param  {InventoryCallback} callback
 * @return {AvailabilityAction} return the result of the callback
 */
export function checkInventory(query, callback) {
  LAST_QUERY = query;
  return callback.apply(null, RESPONSE);
}
