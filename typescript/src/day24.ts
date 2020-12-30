import BetterSet from "./util/BetterSet";

type Point = [number, number];

const pointToString = ([x, y]) => x + '_' + y;

export default function day24(data: string) {
  const paths = parseData(data);
  const blacks1 = part1(paths);
  const pattern = part2(blacks1);

  console.log('===== Day 24 =====');
  console.log('The number of black tiles after the first flipping is', blacks1.size);
  console.log('After 100 days, the tiling has', pattern.size, 'black tiles');
}

function part1(paths: string[][]): BetterSet<Point> {
  const black = new BetterSet<Point>(pointToString);

  paths.forEach(path => {
    const coord = follow([0, 0], path);
    if (black.has(coord)) 
      black.delete(coord);
    else
      black.add(coord);
  });

  return black;
}

function part2(blacks: BetterSet<Point>): BetterSet<Point> {
  for(let i = 0; i < 100; i++) {
    blacks = doTheFlip(blacks);
  }
  return blacks;
}

function doTheFlip(blacks: BetterSet<Point>): BetterSet<Point> {
  const result = new BetterSet<Point>(pointToString);
  
  const toCheck = new BetterSet<Point>(pointToString);
  blacks.forEach(p => {
    toCheck.add(p);
    neighbors(p).forEach(np => toCheck.add(np));
  });

  toCheck.forEach(p => {
    const blackNeighbors = neighbors(p).filter(np => blacks.has(np)).length;
    if (blacks.has(p)) {
      if (blackNeighbors !== 0 && blackNeighbors <= 2) result.add(p);
    } else {
      if (blackNeighbors === 2) result.add(p);
    }
  })

  return result;
}

const DS = [
  // Even ys
  {
    e: [1, 0],
    se: [0, 1],
    sw: [-1, 1],
    w: [-1, 0],
    nw: [-1, -1],
    ne: [0, -1],
  },
  // Odd ys
  {
    e: [1, 0],
    se: [1, 1],
    sw: [0, 1],
    w: [-1, 0],
    nw: [0, -1],
    ne: [1, -1],
  },
];

export function follow(start: Point, path: string[]): Point {
  return path.reduce(([x, y], step) => {
    const [dx, dy] = DS[y & 1][step];
    return [x+dx, y+dy];
  }, start);
}

function neighbors(p: Point): Point[] {
  return 'e se sw w nw ne'.split(' ').map(dir => follow(p, [dir]));
}


function parseData(data: string): string[][] {
  const lines = data.split("\n");
  return lines.map((line) => line.match(/se|ne|sw|nw|e|w/g));
}
