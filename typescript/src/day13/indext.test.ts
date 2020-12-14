import { part2 } from "."

test('works', () => {
  expect(part2('17,x,13,19')).toEqual(3417);
  expect(part2('67,7,59,61')).toEqual(754018);
  expect(part2('67,x,7,59,61')).toEqual(779210);
  expect(part2('1789,37,47,1889')).toEqual(1202161486);
})

115106 % 67