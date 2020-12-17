export default function day09(data: string) {
  const ns = data.split('\n').map(Number);

  const answer1 = part1(ns, 25);  
  const answer2 = part2(answer1, ns);

  console.log('===== Day 9 =====')
  console.log('The first wekness found is', answer1);
  console.log('The sum of min and max of the secret sequence is', answer2);
}

function part1(ns: number[], preamble: number): number {
  let i = preamble;
  while (true) {
    const range = ns.slice(i-preamble, i);
    const hasSum = findSum(ns[i], range);
    if (!hasSum) return ns[i];
    i++;
  }
}

function findSum(n: number, ns: number[]): boolean {
  for (let i = 0; i < ns.length; i++) {
    for(let j = 0; j < ns.length; j++) {
      if (i !== j && ns[i] + ns[j] === n) return true;
    }
  }
  return false;
}

function part2(target: number, ns: number[]): number {
  const range = findContiguousSum(target, ns);
  return Math.min(...range) + Math.max(...range);
}

function findContiguousSum(n: number, ns: number[]): number[] {
  for(let i = 0; i < ns.length; i++) {
    let sum = 0;
    let j = i;
    while (sum < n) {
      sum += ns[j];
      j++;
    }
    if (sum === n) return ns.slice(i, j);
  }
  return [];
}