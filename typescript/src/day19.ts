import { count } from "./util/array";

type RuleId = string;
type RuleSequence = RuleId[];
type Rule = { type: 'char', ch: string } | { type: 'composite', seqs: RuleSequence[] }
export default function day19(data: string) {
  const { rules, messages } = parseData(data);

  const answer1 = part1(rules, messages);
  const answer2 = part2(rules, messages);

  console.log('===== Day 19 =====');
  console.log('The number of messages matching the rules is', answer1);
  console.log('The number of messages matching the corrected rules is', answer2);
}

export function parseData(data: string): { rules: Rule[], messages: string[] } {
  const rules = [];
  let [ruleData, messageData] = data.split('\n\n');
  ruleData.split('\n').forEach(line => {
    if (line.includes('"')) {
      const [_, id, ch] = line.match(/^(\d+).*"(.)"/);
      rules[id] = { type: 'char', ch };
    } else {
      const [id, ...ruleSeqs] = line.split(/[:|]/);
      const seqs = ruleSeqs.map(seq => seq.match(/\d+/g));
      rules[id] = { type: 'composite', seqs };
    }
  });
  const messages = messageData.split('\n');
  return { rules, messages };
}

function part1(rules: Rule[], messages: string[]): number {
  return count(messages, m => applyRule(rules, '0', m) === '');
}

function part2(rules: Rule[], messages: string[]): number {
  return count(messages, m => matchesCorrectedRule0(rules, m));
}

/**
 * After correcting rule 8 and 11, the interesting rules look like:
 * 
 * 0: 8 11   - First rule 8 and then rule 11
 * 8: 42 | 42 8  - One or more matches of rule 42
 * 11: 42 31 | 42 11 31  - One or more matches of rule 42 followed by the same amount of rule 31
 * 
 * So matching rule 0 means to march rule 42 a number of times, followed by matching rule 31 a
 * fewer number number of times.
 */
function matchesCorrectedRule0(rules: Rule[], s: string): boolean {
  let fortyTwos = 1;
  let s1 = applyRule(rules, '42', s);
  while (s1) {
    s1 = applyRule(rules, '42', s1);
    fortyTwos++;
    let s2 = s1;
    for(let i = 0; i < fortyTwos-1 && s2 !== false; i++) {
      s2 = applyRule(rules, '31', s2);
      if (s2 === '') return true;
    }
  }
  return false;
}

/**
 * applies the rule with the given ruleId to the string s and returns the remainder of the
 * string or false if the rule could not be applied
 */
export function applyRule(rules: Rule[], ruleId: RuleId, s: string): string | false {
  const rule = rules[ruleId];
  if (rule.type === 'char') {
    if (s[0] === rule.ch) return s.slice(1);
    return false;
  }
  for(const seq of rule.seqs) {
    const result = applySeq(rules, seq, s);
    if (result !== false) return result;
  };

  return false;
}

function applySeq(rules: Rule[], seq: RuleSequence, s: string): string | false {
  let s1: string | false = s;
  for(const rid of seq) {
    s1 = applyRule(rules, rid, s1);
    if (s1 === false) return false;
  }
  return s1;
}