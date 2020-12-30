export default function day23(data: string) {
  const answer1 = part1(data);
  const answer2 = part2(data);
  console.log("===== Day 23 =====");
  console.log("In the first game, the result is", answer1);
  console.log("In the second game, the result is", answer2);
}

function part1(data: string): string {
  let { cups, current } = createCups(data);

  for (let i = 0; i < 100; i++) {
    current = makeMove(cups, current);
  }
  return toList(cups, 1).slice(1).join('');
}

function part2(data: string): number {
  let { cups, current } = createCups(data, true);
  for(let i = 0; i < 1e7; i++) {
    current = makeMove(cups, current);
  }
  const n1 = cups[1];
  const n2 = cups[cups[1]];
  return n1 * n2;
}

export function makeMove(cups: number[], current: number): number {
  const len = cups.length-1;
  const triplet1 = cups[current];
  const triplet2 = cups[triplet1];
  const triplet3 = cups[triplet2];
  const triplet = [triplet1, triplet2, triplet3];

  // Find the cup to place the triplet at. It's the first number less than
  // the current cup, that is not in the triplet, looping around if we reach 1.
  let cupBefore = current === 1 ? len : (current - 1);
  while (triplet.includes(cupBefore)) cupBefore = cupBefore === 1 ? len : (cupBefore - 1);

  // Change the linked list
  // Before:
  //      current -> triplet1
  //      triplet1 -> triplet2
  //      triplet2 -> triplet3
  //      triplet3 -> after_triplet
  //      cupBefore -> after_cup_before
  // After:
  //      current -> after_triplet
  //      triplet1 -> triplet2
  //      triplet2 -> triplet3
  //      triplet3 -> after_cup_before
  //      cupBefore -> triplet1
  cups[current] = cups[triplet3];
  cups[triplet3] = cups[cupBefore];
  cups[cupBefore] = triplet1;

  return cups[current];
}

export function toList(cups: number[], current: number): number[] {
  let result = [];
  let c = current;
  do {
    result.push(c);
    c = cups[c];
  } while (c !== current);
  return result;
}

/*
 * The cups is represented as an array, where each entry contains the index of the
 * next cup. But first we make the cups 0-based to make things a bit easier when moving
 * them around.
 * 
 * For example, if the cups are '3 1 2 4', they are first made 0-based as
 * '2 0 1 3'. Then the array would be [1, 3, 0, 2]. At index 2, we have the value
 * 0, meaning that the cup after 2 is 0. Doing it again, we see that the cup after
 * 0 is 1. (Or with the original 1-based cups: the cup after 3 is 1 and the cup after
 * 1 is 2)
 */
export function createCups(data: string, millionCups = false): { cups: number[]; current: number } {
  const cups = Array(9).fill(0);
  const firstCup = +data[0];
  const lastCup = +data.slice(-1);
  data
    .split("")
    .map(Number)
    .forEach((c, i, ns) => (cups[c] = ns[i + 1]));
  cups[lastCup] = firstCup;
  if (millionCups) {
    for(let i = 10; i <= 1e6; i++) {
      cups[i] = i+1;
    }
    cups[lastCup] = 10;
    cups[1e6] = firstCup;
  }

  return { cups, current: firstCup };
}