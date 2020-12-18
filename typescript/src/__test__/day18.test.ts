import { advanced, basic } from "../day18"

test('basic evaluation works', () => {
  expect(basic('((1 + 5) * 2) + (1 + 2)')).toBe('15');
  expect(basic('1 + 2 * 3')).toBe('9');
})

test('advanced evlautation works', () => {
  expect(advanced('((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2')).toBe('23340')
})