import { checkStatus, checkInventory } from './grocer';

// Step 1: Write an inline callback
export function isServiceOpen() {
  return checkStatus((serviceStatus) => {
    if (serviceStatus === 'ONLINE') {
      return true;
    }

    return false;
  });
}

// Step 2: Use a passed in callback
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

// Step 3: Write a callback that can be reused
export function handleInventoryResult(err, isAvailable) {
  if (err) {
    throw new Error(err);
  }

  if (isAvailable) {
    return 'PURCHASE';
  } else {
    return 'NOOP';
  }
}

// Step 4: Put them together
export function pickAndPurchaseFruit(variety, quantity) {
  return pickFruit(variety, quantity, handleInventoryResult);
}
