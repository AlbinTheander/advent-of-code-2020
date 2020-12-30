import { createCups, makeMove, toList } from '../day23';

test('makeMove works', () => {
  function testMakeMove(initial: string, expected: string) {
    const { cups, current } = createCups(initial);
    const newCurrent = makeMove(cups, current)
    const resultS = toList(cups, newCurrent).join('');
    expect(resultS).toEqual(expected);
  }
  testMakeMove('389125467', '289154673');
  testMakeMove('289154673', '546789132');
  testMakeMove('256183479', '834795612');
});