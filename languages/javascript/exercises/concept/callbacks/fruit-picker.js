import { checkStatus, checkInventory } from './grocer';

// Write an inline callback
export function isServiceOpen() {
  return checkStatus((openStatus) => {
    if (openStatus === 'OPEN') {
      return true;
    }

    return false;
  });
}

// Use a passed in callback
export function pickFruit(variety, quantity, callback) {
  return checkInventory(
    {
      type: 'fruit',
      variety,
      quantity,
    },
    callback
  );
}

// Write a callback that can be reused
export function handleInventoryResult(err, isAvailable) {
  if (err) {
    throw new Error(err);
  }

  if (isAvailable) {
    return 'BUY';
  } else {
    return 'NO_OP';
  }
}
