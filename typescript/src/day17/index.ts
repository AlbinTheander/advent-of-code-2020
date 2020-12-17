export default function day17(data: string) {
  let room = parseData(data);
  // printRoom(room);
  // printRoom(evolve(room));
  // console.log(cube1({ x: 1, y: 1, z: 1 }).filter((p1) => room.has(p1)));
  for (let i = 0; i < 6; i++) {
    room = evolve(room);
  }

  console.log([...room.values()].length);
}

function evolve(room: Set4d): Set4d {
  const toCheck = new Set4d();
  [...room.values()].forEach((p) => {
    cube1(p).forEach((p1) => toCheck.addP(p1));
  });

  const newRoom = new Set4d();
  [...toCheck.values()].forEach((p) => {
    const activeNeighborhood = cube1(p).filter((p1) => room.has(p1)).length;
    if (room.has(p)) {
      if (activeNeighborhood === 3 || activeNeighborhood === 4) newRoom.addP(p);
    } else if (activeNeighborhood === 3) {
      newRoom.addP(p);
    }
  });

  return newRoom;
}

function cube1(p: Point4d): Point4d[] {
  const ps = [];
  for (let x = -1; x <= 1; x++)
    for (let y = -1; y <= 1; y++)
      for (let z = -1; z <= 1; z++)
        for (let w = -1; w <= 1; w++)
          ps.push({ x: p.x + x, y: p.y + y, z: p.z + z, w: p.w + w });
  return ps;
}

function printRoom(room: Set4d) {
  const points = [...room.values()];
  const minX = Math.min(...points.map((p) => p.x));
  const maxX = Math.max(...points.map((p) => p.x));
  const minY = Math.min(...points.map((p) => p.y));
  const maxY = Math.max(...points.map((p) => p.y));
  const minZ = Math.min(...points.map((p) => p.z));
  const maxZ = Math.max(...points.map((p) => p.z));
  const minW = Math.min(...points.map((p) => p.w));
  const maxW = Math.max(...points.map((p) => p.w));

  for (let w = minW; w <= maxW; w++) {
    for (let z = minZ; z <= maxZ; z++) {
      console.log(`Z=${z}, W=${w} X=${minX}..${maxX} Y=${minY}..${maxY}`);
      for (let y = minY; y <= maxY; y++) {
        let line = "";
        for (let x = minX; x <= maxX; x++) {
          line += room.has({ x, y, z, w }) ? "#" : ".";
        }
        console.log(line);
      }
      console.log();
    }
  }
}

function parseData(data: string): Set4d {
  const room = new Set4d();
  data.split("\n").forEach((line, y) =>
    line.split("").forEach((ch, x) => {
      if (ch === "#") room.add(x, y, 0, 0);
    })
  );
  return room;
}

interface Point4d {
  x: number;
  y: number;
  z: number;
  w: number;
}

class Set4d {
  private points = new Map<string, Point4d>();

  private toKey(x: number, y: number, z: number, w: number) {
    return `${x}_${y}_${z}_${w}`;
  }

  addP(p: Point4d) {
    this.points.set(this.toKey(p.x, p.y, p.z, p.w), p);
  }

  add(x, y, z, w) {
    this.points.set(this.toKey(x, y, z, w), { x, y, z, w });
  }

  has(p: Point4d) {
    return this.points.has(this.toKey(p.x, p.y, p.z, p.w));
  }

  values() {
    return this.points.values();
  }

  toString() {
    return this.values().toString();
  }
}
