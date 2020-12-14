export default function day10(data: string) {
  const joltages = parseData(data);
  
  const answer1 = part1(joltages);
  const answer2 = part2(joltages);

  console.log('===== Day 10 =====')
  console.log('The checksum for using the adapters is', answer1);
  console.log('The number of ways to arrange the adapters is', answer2);
}

function parseData(data: string): number[] {
  const ns = data.split('\n').map(Number).sort((a, b) => a - b);
  ns.unshift(0);  // Add outlet
  ns.push(ns[ns.length-1] + 3); // Add built-in adapter
  return ns;
}

function part1(joltages: number[]): number {
  const dc = differenceCounts(joltages);
  return dc[1] * dc[3];
}

function differenceCounts(ns: number[]): number[] {
  const diffs = [0, 0, 0, 0];
  ns.forEach((n, i) => {
    if (i > 0) diffs[n - ns[i-1]]++;
  });
  return diffs;
}

function part2(joltages: number[]): number {
  const groups = groupContiguous(joltages);
  console.log(groups);
  const arrangements = groups.map(g => g.length).map(waysToArrangeItems);
  return arrangements.reduce((a, b) => a * b, 1);
}

function groupContiguous(joltages: number[]): number[][] {
  const groups = [];
  let group = [];
  joltages.forEach((n, i) => {
    if (n - joltages[i-1] === 3) {
      groups.push(group);
      group = [n];
    } else {
      group.push(n);
    }
  });
  groups.push(group);
  return groups;
}

// This is the formula for the Tribonacci numbers, starting with 0, 1, 1
// f[n] = f[n-1] + f[n-2] + f[n-3]
export function waysToArrangeItems(count): number {
  if (count <= 0) return 0; // No way to arrange zero adapters
  if (count === 1) return 1; // Only one way to arrange one adapter
  return waysToArrangeItems(count-1) +
    waysToArrangeItems(count-2) +
    waysToArrangeItems(count-3);
}