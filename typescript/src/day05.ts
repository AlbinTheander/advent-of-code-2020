export default function day5(data: string) {
  const boardingPasses = data.split('\n');
  const seats = boardingPasses.map(getRow)
  const max = Math.max(...seats);
  const mySeat = findSingleHole(seats);
  console.log("===== Day 5 =====");
  console.log("The highest seat number is", max);
  console.log("The number of my seat is", mySeat);
}

function getRow(s: String): number {
  const binary = s.replace(/[FL]/g, '0').replace(/[BR]/g, '1');
  return parseInt(binary, 2);
}

function findSingleHole(seats: number[]): number {
  const seatSet = new Set(seats);
  for(let s = 0; s < Math.max(...seats); s++) {
    if (seatSet.has(s-1) && seatSet.has(s+1) && !seatSet.has(s)) return s;
  }
  return -1;
}

