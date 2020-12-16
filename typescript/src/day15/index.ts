export default function day15(data: string) {
  const ns = data.split(',').map(Number);
  console.log(ns);

  const latest = new Map<number, number>();

  let turn = 1;
  let current = ns.pop();
  while(turn <= ns.length) {
    latest.set(ns[turn-1], turn);
    turn++;
  }

  while(turn < 30000000) {
    let next = 0;
    if (latest.has(current)) {
      next = turn - latest.get(current);
    }
    // console.log('I got', current, latest);
    // console.log('I say', next);
    if (turn % 1000000 === 0) console.log(turn, current);
    latest.set(current, turn);
    current = next;
    turn++;
  }
  console.log(current);
}