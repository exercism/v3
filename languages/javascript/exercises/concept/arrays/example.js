export function getItem(array, index) {
  return array[index];
}

export function setItem(array, index, newValue) {
  array[index] = newValue;
  return array;
}

export function insertItemFromTop(array, newCard) {
  array.push(newCard)
  return array
}

export function removeItem(array, index) {
  array.splice(index, 1);
  return array;
}

export function removeItemFromTop(array) {
  array.pop();
  return array;
}

export function insertItemFromBottom(array, newCard) {
  array.unshift(newCard);
  return array;
}

export function removeItemFromBottom(array) {
  array.shift();
  return array;
}

export function checkLengthOfStack(array, stackLength) {
  return array.length === stackLength;
}
