export default function day19(data: string) {
  const { rules, messages } = parseData(data);

  const matching = messages.filter(m => matchRules2(rules, m));
  console.log(matching);
  console.log(matching.length);
}


export function parseData(data: string) {
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

function matchRule(rules, ruleId, s) {
  return applyRule(rules, ruleId, s) === '';
}

/*
8: 42 | 42 8
11: 42 31 | 42 11 31
 */
function matchRules2(rules, s) {
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

export function applyRule(rules, ruleId, s) {
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

function applySeq(rules, seq, s) {
  let s1 = s;
  for(const rid of seq) {
    s1 = applyRule(rules, rid, s1);
    if (s1 === false) return false;
  }
  return s1;
}