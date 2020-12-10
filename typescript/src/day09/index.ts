export default function day09(data: string) {
  const ns = data.split('\n').map(Number);

  let found = -1;
  let preamble = 25;
  let i = preamble;
  while (found < 0) {
    const range = ns.slice(i-preamble, i);
    console.log(ns[i], range);
    const isThere = findSum(ns[i], range);
    if (!isThere) found = i;
    i++
  }

  console.log(ns[found]);
  const list = findList(ns[found], ns);

  console.log(list);
  console.log(Math.min(...list) + Math.max(...list));
}


function findSum(n: number, ns: number[]): boolean {
  for (let i = 0; i < ns.length; i++) {
    for(let j = 0; j < ns.length; j++) {
      if (i !== j && ns[i] + ns[j] === n) return true;
    }
  }
  return false;
}

function findList(n: number, ns: number[]): number[] {
  for(let i = 0; i < ns.length; i++) {
    let sum = 0;
    let j = i;
    while (sum < n) {
      sum += ns[j];
      j++;
    }
    console.log('Tried', ns[i], '->', ns[j], ': ', sum);
    if (sum === n) return ns.slice(i, j);
  }
  return [];
}