import { applyRule, parseData } from '../day19';

const example1 = `0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb`;

const example2 = `0: 1 2
1: 3 | 3 3
2: 4
3: "a"
4: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb`;

test('can parse rules', () => {
  const rules = parseData(example1).rules;
  expect(rules[4]).toEqual({ type: 'char', ch: 'a' });
  expect(rules[0]).toEqual({ type: 'composite', seqs: [['4', '1', '5']]})
  expect(rules[1]).toEqual({ type: 'composite', seqs: [['2', '3'], ['3', '2']]})
});

test('can match rules', () => {
  const rules = parseData(example1).rules;
  // expect(applyRule(rules, '4', 'abc')).toBe('bc');
  // expect(applyRule(rules, '5', 'abc')).toBe(false);
  // expect(applyRule(rules, '3', 'abc')).toBe('c');
  expect(applyRule(rules, '1', 'babbc')).toBe('c');
})

test('can match harder rules', () => {
  const rules = parseData(example2).rules;

  expect(applyRule(rules, '0', 'aabc')).toBe(false);
})