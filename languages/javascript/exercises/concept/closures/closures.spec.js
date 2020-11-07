import {
  translate2d,
  scale2d,
  composeTransform,
  memoizeTransform,
} from './closures';

const fakeTransform = () => {
  let first = true;

  return () => {
    if (first) {
      first = false;
      return [1, 1];
    }

    return false;
  };
};

describe('closures', () => {
  describe('translate2d', () => {
    test('should return a function', () => {
      expect(typeof translate2d(0, 0)).toBe('function');
    });

    const dx = 3;
    const dy = -5;
    const translator = translate2d(dx, dy);
    const x1 = 0;
    const y1 = 0;
    const expected = [3, -5];
    xtest(`should be predictable`, () => {
      expect(translator(x1, y1)).toEqual(expected);
    });

    const x2 = 4;
    const y2 = 5;
    const reusedExpected = [7, 0];
    xtest('should be reusable', () => {
      expect(translator(x2, y2)).toEqual(reusedExpected);
    });
  });

  describe('scale2d', () => {
    xtest('should return a function', () => {
      expect(typeof scale2d(0, 0)).toBe('function');
    });

    const dx = 4;
    const dy = 2;
    const scaler = scale2d(dx, dy);
    const x1 = 1;
    const y1 = 1;
    const expected = [4, 2];
    xtest(`should be predictable`, () => {
      expect(scaler(x1, y1)).toEqual(expected);
    });

    const x2 = -2;
    const y2 = 5;
    const reusedExpected = [-8, 10];
    xtest('should be reusable', () => {
      expect(scaler(x2, y2)).toEqual(reusedExpected);
    });
  });

  describe('composeTransform', () => {
    const dx = -6;
    const dy = 10;
    const translator = translate2d(dx, dy);
    const sx = 3;
    const sy = 2;
    const scaler = scale2d(sx, sy);

    xtest('should return a function', () => {
      expect(typeof composeTransform(translator, scaler)).toBe('function');
    });

    xtest('should compose two translate functions', () => {
      const composeTranslate = composeTransform(translator, translator);
      expect(composeTranslate(0, 0)).toEqual([-12, 20]);
    });

    xtest('should compose two scale functions', () => {
      const composeScale = composeTransform(scaler, scaler);
      expect(composeScale(1, 1)).toEqual([9, 4]);
    });

    xtest('should compose in the correct order: g(f(x))', () => {
      const composed = composeTransform(scaler, translator);
      expect(composed(0, 0)).toEqual([-6, 10]);
    });

    xtest('should compose in the opposite order: g(f(x))', () => {
      const composed = composeTransform(translator, scaler);
      expect(composed(0, 0)).toEqual([-18, 20]);
    });
  });

  describe('memoizeTransform', () => {
    xtest('should return a function', () => {
      expect(typeof memoizeTransform(translate2d(0, 0))).toBe('function');
    });

    xtest('should return the same result if given the same input', () => {
      const memoizedTranslate = memoizeTransform(translate2d(2, 2));
      expect(memoizedTranslate(2, 2)).toEqual([4, 4]);
      expect(memoizedTranslate(2, 2)).toEqual([4, 4]);
    });

    xtest('should not call the memoized function if the input is the same', () => {
      const memoizedTransform = memoizeTransform(fakeTransform());
      expect(memoizedTransform(5, 5)).toEqual([1, 1]);
      expect(memoizedTransform(5, 5)).toEqual([1, 1]);
    });
  });
});
