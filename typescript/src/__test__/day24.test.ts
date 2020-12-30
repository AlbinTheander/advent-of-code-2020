import { follow } from "../day24";

test('follow works', () => {
  expect(follow([0, 0], 'nw w sw e e'.split(' '))).toEqual([0, 0]);
});