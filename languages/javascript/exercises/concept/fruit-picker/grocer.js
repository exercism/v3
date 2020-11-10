/**
 *
 */

let STATUS = 'OFFLINE';

export function checkStatus(callback) {
  return callback(STATUS);
}

export function setStatus(status) {
  STATUS = status;
}

export function resetStatus() {
  STATUS = 'OFFLINE';
}

/**
 *
 */

let LAST_QUERY = undefined;
let RESPONSE = undefined;

export function setResponse(...response) {
  RESPONSE = response;
}

export function getLastQuery() {
  return LAST_QUERY;
}

export function resetQuery() {
  LAST_QUERY = undefined;
  RESPONSE = ['undefined response'];
}

export function checkInventory(query, callback) {
  LAST_QUERY = query;
  return callback.apply(null, RESPONSE);
}
