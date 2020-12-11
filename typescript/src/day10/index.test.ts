import { countWays } from ".";

test('ways', () => {
  expect(countWays(0, 1)).toBe(1);
  expect(countWays(0, 2)).toBe(2);
  expect(countWays(0, 3)).toBe(4);
  expect(countWays(0, 4)).toBe(7);
  expect(countWays(0, 5)).toBe(13);
  expect(countWays(0, 6)).toBe(24);
  expect(countWays(0, 7)).toBe(44);
  expect(countWays(0, 8)).toBe(81);
})