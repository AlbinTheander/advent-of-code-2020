import { parseData, part2 } from "../day13"

  const testLines = (s: string): number => part2(parseData('0\n' + s).buses)
test('works', () => {
  expect(testLines('17,x,13,19')).toEqual(3417);
  expect(testLines('67,7,59,61')).toEqual(754018);
  expect(testLines('67,x,7,59,61')).toEqual(779210);
  expect(testLines('1789,37,47,1889')).toEqual(1202161486);
})

115106 % 67