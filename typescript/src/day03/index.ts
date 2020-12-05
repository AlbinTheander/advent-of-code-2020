import fs from 'fs';

export default function day3(data: string) {
  const map = parseData(data);
  const answer1 = countTrees(map, 3, 1);
  const answer2 = countManySlopes(map);
  console.log('====== Day 3 ======')
  console.log('Barraging down the hill, I encounter', answer1, 'trees');
  console.log('Trying different slopes, I get the magical number', answer2);
}

function parseData(s) {
  return s.split('\n');
}

function countManySlopes(map: string[]) {
  return [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
    .map(([dx, dy]) => countTrees(map, dx, dy))
    .reduce((a, b) => a * b, 1);
}

function countTrees(map: string[], dx: number, dy: number): number {
  let x = 0;
  let y = 0;
  let count = 0;

  while(y < map.length) {
    if (map[y][x % map[0].length] === '#') count++;
    y += dy;
    x += dx;
  }

  return count;
}