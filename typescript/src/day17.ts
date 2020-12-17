import BetterSet from "./util/betterSet";

type Point = number[]; // Any number of dimensions
type Room = BetterSet<Point>

const createRoom = () => new BetterSet<Point>(p => p.join('_'));
const createPoint = (dimensions:number) => (...coords: number[]): Point => 
  Array.from({length: dimensions}, (_, i) => coords[i] || 0);


export default function day17(data: string) {
  let room3D = parseData(data, 3);
  for (let i = 0; i < 6; i++) {
    room3D = evolve(room3D);
  }
  const answer1 = room3D.values().length;

  let room4D = parseData(data, 4);
  for (let i = 0; i < 6; i++) {
    room4D = evolve(room4D);
  }  
  const answer2 = room4D.values().length;

  console.log('===== Day 17 =====');
  console.log('After 6 iterations in 3D, there are', answer1, 'active nodes');
  console.log('After 6 iterations in 4D, there are', answer2, 'active nodes');
}

function evolve(room: Room): Room {
  const toCheck = createRoom();
  room.values().forEach((p) => {
    cube(p).forEach((p1) => toCheck.add(p1));
  });

  const newRoom = createRoom();
  toCheck.values().forEach((p) => {
    const activeNeighborhood = cube(p).filter((p1) => room.has(p1)).length;
    if (room.has(p)) {
      if (activeNeighborhood === 3 || activeNeighborhood === 4) newRoom.add(p);
    } else if (activeNeighborhood === 3) {
      newRoom.add(p);
    }
  });

  return newRoom;
}

export function cube(p: Point): Point[] {
  const ps = [];
  let current = [];
  const generate = (dim: number): void => {
    if (dim >= p.length) {
      ps.push(current.slice())
      return;
    }
    for (let i = -1; i <= 1; i++) {
      current[dim] = p[dim] + i;
      generate(dim+1);
    }
  }
  generate(0);
  return ps;
}

function parseData(data: string, dimensions: number): Room {
  const room = createRoom();
  data.split("\n").forEach((line, y) =>
    line.split('').forEach((ch, x) => {
      if (ch === '#') room.add(createPoint(dimensions)(x, y));
    })
  );
  return room;
}