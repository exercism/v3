/**
 * a fUnCtIoN To eAsIlY SpOnGeBoBiFy tExT
 *
 * @param {string} text
 * @returns {string}
 */
export function spongebobify(text) {
  return [...text]
    .map((char, index) =>
      index % 2 === 0 ? char.toLowerCase() : char.toUpperCase()
    )
    .join('');
}

/**
 * A 👏 function 👏 to 👏 easily 👏 clapify 👏 text
 *
 * @param {string} text
 * @returns {string}
 */
export function clapify(text) {
  if (isClapified(text)) {
    return text;
  }

  return text.split(' ').join(' 👏 ');
}

/**
 *
 *
 * @param {string} text
 * @returns {boolean}
 */
function isClapified(text) {
  return (
    text.includes('👏') &&
    text.split(' 👏 ').length * 2 - 1 === text.split(' ').length
  );
}

/**
 * A FUNCTION TO REPEATIFY TEXT
 * A FUNCTION TO REPEATIFY TEXT
 * A FUNCTION TO REPEATIFY TEXT
 * A FUNCTION TO
 * @param {string} text
 * @param {number} [limit=280]
 */
export function repeatify(text, limit = 280) {
  let repeated = (text + '\n').repeat(limit / text.length + 1);

  return tweetify(repeated, limit);
}

/**
 *
 *
 * @export
 * @param {string} text
 * @param {number} [limit=280]
 * @returns
 */
export function tweetify(text, limit = 280) {
  text = text.slice(0, limit);

  // remove emojis
  while (text.charCodeAt(text.length - 1) >= Math.pow(2, 16)) {
    text = text.slice(0, text.length - 2);
  }

  return text;
}
