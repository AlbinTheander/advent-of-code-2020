import { count } from "../util/array";

export default function day11(data: string) {
  let room = data.split('\n').map(line => line.split(''));

  let next = nextGeneration(room, look1, 4);
  while (room !== next) {
    room = next;
    next = nextGeneration(room, look1, 4);
  }

  console.log(countOccupied(next));

  next = nextGeneration(room, lookFar, 5);
  while (room !== next) {
    room = next;
    next = nextGeneration(room, lookFar, 5);
  }

  console.log(countOccupied(next));

}

function printRoom(room) {
  console.log();
  console.log(room.map(line => line.join('')).join('\n'));
}

type Looker = (room: string[][], x: number, y: number, dx: number, dy: number) => string

function look1(room, x, y, dx, dy) {
  const x1 = x + dx;
  const y1 = y + dy;
  if (y1 < 0 || y1 >= room.length || x1 < 0 || x1 >= room[0].length) return '.';
  return room[y1][x1];
}

function lookFar(room, x, y, dx, dy) {
  const valid = (x, y) => y >= 0 && y < room.length && x >= 0 && x < room[0].length;

  let x1 = x + dx;
  let y1 = y + dy;

  while (valid(x1, y1)) {
    if (room[y1][x1] !== '.') return room[y1][x1];
    x1 += dx;
    y1 += dy;
  }
  return '.';
}

function nextGeneration(room: string[][], looker: Looker, limit: number): string[][] {
  const newRoom = Array(room.length).fill('').map(() => []);
  let changed = false;

  for(let y = 0; y < room.length; y++)
    for(let x = 0; x < room[0].length; x++) {
      const occupied = [
        [-1, -1], [0, -1], [1, -1],
        [-1,  0],          [1,  0],
        [ -1, 1], [0,  1], [1,  1]]
        .map(([dx, dy]) => looker(room, x, y, dx, dy))
        .filter(seat => seat === '#').length;

      const current = room[y][x];
      if (current === 'L' && occupied === 0) {
        newRoom[y][x] = '#';
        changed = true;
      } else if (current === '#' && occupied >= limit) {
        newRoom[y][x] = 'L';
        changed = true;
      } else {
        newRoom[y][x] = current;
      }
    }

    return changed ? newRoom : room;
}

function countOccupied(room: string[][]): number {
  return count(room.toString().split(''), ch => ch === '#');
}