import { waysToArrangeItems } from "../day10";

test('ways', () => {
  expect(waysToArrangeItems(1)).toBe(1);
  expect(waysToArrangeItems(2)).toBe(1);
  expect(waysToArrangeItems(3)).toBe(2);
  expect(waysToArrangeItems(4)).toBe(4);
  expect(waysToArrangeItems(5)).toBe(7);
  expect(waysToArrangeItems(6)).toBe(13);
  expect(waysToArrangeItems(7)).toBe(24);
  expect(waysToArrangeItems(8)).toBe(44);
})