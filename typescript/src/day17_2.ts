export default function day17(data: string) {
  let room = parseData(data);
  // printRoom(room);
  // printRoom(evolve(evolve(evolve(room))));
  // console.log(cube1({ x: 1, y: 1, z: 1 }).filter((p1) => room.has(p1)));
  for(let i = 0; i < 6; i++) {
    room = evolve(room);
  }

  console.log([...room.values()].length);
}

function evolve(room: Set3d): Set3d {
  const toCheck = new Set3d();
  [...room.values()].forEach((p) => {
    cube1(p).forEach((p1) => toCheck.addP(p1));
  });

  const newRoom = new Set3d();
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

function cube1(p: Point3d): Point3d[] {
  const ps = [];
  for (let x = -1; x <= 1; x++)
    for (let y = -1; y <= 1; y++)
      for (let z = -1; z <= 1; z++)
        ps.push({ x: p.x + x, y: p.y + y, z: p.z + z });
  return ps;
}

function printRoom(room: Set3d) {
  const points = [...room.values()];
  const minX = Math.min(...points.map(p => p.x));
  const maxX = Math.max(...points.map(p => p.x));
  const minY = Math.min(...points.map(p => p.y));
  const maxY = Math.max(...points.map(p => p.y));
  const minZ = Math.min(...points.map(p => p.z));
  const maxZ = Math.max(...points.map(p => p.z));

  for(let z = minZ; z <= maxZ; z++) {
    console.log(`Z=${z} X=${minX}..${maxX} Y=${minY}..${maxY}`)
    for(let y = minY; y <= maxY; y++) {
      let line = '';
      for(let x = minX; x <= maxX; x++) {
        line += (room.has({x, y, z}) ? '#' : '.')
      }
      console.log(line);
    }
    console.log();
  }
}


function parseData(data: string): Set3d {
  const room = new Set3d();
  data.split("\n").forEach((line, y) =>
    line.split("").forEach((ch, x) => {
      if (ch === "#") room.add(x, y, 0);
    })
  );
  return room;
}

interface Point3d {
  x: number;
  y: number;
  z: number;
}

class Set3d {
  private points = new Map<string, Point3d>();

  private toKey(x: number, y: number, z: number) {
    return `${x}_${y}_${z}`;
  }

  addP(p: Point3d) {
    this.points.set(this.toKey(p.x, p.y, p.z), p);
  }

  add(x, y, z) {
    this.points.set(this.toKey(x, y, z), { x, y, z });
  }

  remove(x, y, z) {
    this.points.delete(this.toKey(x, y, z));
  }

  has(p: Point3d) {
    return this.points.has(this.toKey(p.x, p.y, p.z));
  }

  values() {
    return this.points.values();
  }

  toString() {
    return this.values().toString();
  }
}
