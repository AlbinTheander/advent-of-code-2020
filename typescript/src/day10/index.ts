export default function day10(data: string) {
  const ns = data.split('\n').map(Number);

  ns.sort((a, b) => a - b);
  ns.unshift(0);
  ns.push(ns[ns.length-1] + 3);
  
  const diffs = [0, 0, 0, 0];
  console.log(ns);

  ns.forEach((n, i) => {
    if (i > 0) diffs[n - ns[i-1]]++;
  })

  console.log(diffs);
  console.log(diffs[1] * diffs[3])

  const groups = [];
  let group = [];
  ns.forEach((n, i) => {
    if (n - ns[i-1] === 3) {
      groups.push(group);
      group = [n];
    } else {
      group.push(n);
    }
  });
  groups.push(group);
  console.log(groups);
  console.log(Math.max(...groups.map(g => g.length)));

  const tribonaccis = [0, 1, 1, 2, 4, 7]
  const sum = groups.map(g => tribonaccis[g.length]).reduce((a, b) => a * b, 1);
  console.log(sum);
}


export function countWays(from: number, to: number): number {
  if (from === to) return 1;
  if (from > to) return 0;
  return countWays(from + 1, to) + countWays(from + 2, to) + countWays(from + 3, to);
}