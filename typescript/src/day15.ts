export default function day15(data: string) {
  const ns = data.split(',').map(Number);
  const answer1 = playGame(ns, 2020);
  const answer2 = playGame(ns, 30000000);

  console.log('===== Day 15 =====');
  console.log('After 2020 rounds, the current number is', answer1);
  console.log('After 30000000 rounds, the current number is', answer2);
}

function playGame(startingNumbers: number[], rounds: number): number {
  const latest = new Map<number, number>();

  let turn = 1;
  while(turn <= startingNumbers.length-1) {
    latest.set(startingNumbers[turn-1], turn);
    turn++;
  }

  let current = startingNumbers[startingNumbers.length-1];
  while(turn < rounds) {
    let next = 0;
    if (latest.has(current)) {
      next = turn - latest.get(current);
    }
    latest.set(current, turn);
    current = next;
    turn++;
  }

  return current;
}