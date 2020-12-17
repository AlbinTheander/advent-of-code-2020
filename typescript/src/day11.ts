import { count } from "./util/array";

enum Spot {
  empty = '.',
  chair = 'L',
  person = '#'
};
type Room = Spot[][];
type Looker = (room: Room, x: number, y: number, dx: number, dy: number) => Spot;

export default function day11(data: string) {
  let room = data.split('\n').map(line => line.split('')) as Room;

  const answer1 = evolveFromNeighbors(room);
  const answer2 = evolveFromLineOfSight(room);

  console.log('===== Day 11 =====');
  console.log('When the near sighted room stabilizes, there are', answer1, 'people.');
  console.log('When the far sighted room stabilizes, there are', answer2, 'people.');
}
  

function evolveFromNeighbors(room: Room): number {
  let next = nextGeneration(room, look1, 4);
  while (room !== next) {
    room = next;
    next = nextGeneration(room, look1, 4);
  }

  return countOccupied(room);
}

function evolveFromLineOfSight(room: Room): number {
  let next = nextGeneration(room, lookFar, 5);
  while (room !== next) {
    room = next;
    next = nextGeneration(room, lookFar, 5);
  }

  return countOccupied(next);
}

function printRoom(room) {
  console.log();
  console.log(room.map(line => line.join('')).join('\n'));
}

function look1(room: Room, x: number, y: number, dx: number, dy: number): Spot {
  const x1 = x + dx;
  const y1 = y + dy;
  if (y1 < 0 || y1 >= room.length || x1 < 0 || x1 >= room[0].length) return Spot.empty;
  return room[y1][x1];
}

function lookFar(room: Room, x: number, y: number, dx: number, dy: number): Spot {
  const valid = (x, y) => y >= 0 && y < room.length && x >= 0 && x < room[0].length;

  let x1 = x + dx;
  let y1 = y + dy;

  while (valid(x1, y1)) {
    if (room[y1][x1] !== '.') return room[y1][x1];
    x1 += dx;
    y1 += dy;
  }
  return Spot.empty;
}

function nextGeneration(room: Room, looker: Looker, limit: number): Room {
  const newRoom = Array(room.length).fill('').map(() => []);
  let changed = false;

  for(let y = 0; y < room.length; y++)
    for(let x = 0; x < room[0].length; x++) {
      const occupied = [
        [-1, -1], [0, -1], [1, -1],
        [-1,  0],          [1,  0],
        [ -1, 1], [0,  1], [1,  1]]
        .map(([dx, dy]) => looker(room, x, y, dx, dy))
        .filter(seat => seat === Spot.person).length;

      const current = room[y][x];
      if (current === Spot.chair && occupied === 0) {
        newRoom[y][x] = Spot.person;
        changed = true;
      } else if (current === Spot.person && occupied >= limit) {
        newRoom[y][x] = Spot.chair;
        changed = true;
      } else {
        newRoom[y][x] = current;
      }
    }

    return changed ? newRoom : room;
}

function countOccupied(room: Room): number {
  return count(room.toString().split(''), ch => ch === Spot.person);
}