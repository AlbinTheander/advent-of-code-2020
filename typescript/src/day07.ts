interface BagRule {
  color: string;
  containsAmount: number;
  containsColor: string; 
}

export default function day07(data: string) {
  const bagRules = parseData(data);

  const answer1 = part1(bagRules);
  const answer2 = part2(bagRules);
  
  console.log('===== Day 7 =====');
  console.log('There are', answer1, 'bags that can contain a shiny gold bag');
  console.log('A shiny gold bag will contain', answer2, 'bags');
}

function part1(rules: BagRule[]): number {
  const toCheck = ['shiny gold'];
  const checked = new Set<string>();
  let count = 0;

  while (toCheck.length > 0) {
    const color = toCheck.pop();
    checked.add(color);
    const bags = rules
      .filter(rule => !checked.has(rule.color))
      .filter(rule => rule.containsColor === color)
    count += bags.length;
    bags.forEach(bag => toCheck.push(bag.color));
  }
  return count;
}

function part2(rules: BagRule[]): number {
  function countContent(color: string): number {
    return rules
      .filter(rule => rule.color === color)
      .map(rule => rule.containsAmount * countContent(rule.containsColor))
      .reduce((a, b) => a + b, 1);
  }
  return countContent('shiny gold') - 1;
}

function parseData(s: string): BagRule[] {
  return s.split('\n').flatMap(line => {
    const bags = line.split(/contain|,/g);
    const [color] = bags[0].match(/\w+ \w+/);
    const contains = bags.slice(1).map(bs => {
      const [isMatch, count, color] = bs.match(/(\d+) (\w+ \w+)/) || [];
      return isMatch ? { containsAmount: +count, containsColor: color } : null;
    }).filter(Boolean);
    return contains.map(c => ({...c, color}));
  })
}