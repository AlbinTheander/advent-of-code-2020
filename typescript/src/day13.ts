type Bus = { id: number, offset: number };

export default function day13(data: string) {
  const { timestamp, buses } = parseData(data);

  const answer1 = part1(timestamp, buses);
  const answer2 = part2(buses);
  const { busId, waitTime } = answer1;

  console.log('===== Day 13 =====')
  console.log('Best bus is id', busId, 'after waiting', waitTime, 'seconds. Magic number is', busId * waitTime);
  console.log('The first time the buses go in sequence is', answer2);
}

export function parseData(data: string): { timestamp: number, buses: Bus[] } {
  const lines = data.split('\n');
  const timestamp = +lines[0];
  const buses = lines[1].split(',')
    .map((s, i) => ({ id: +s, offset: (i % +s) }))
    .filter(bus => !isNaN(bus.id));

  return { timestamp, buses };
}

function part1(timestamp: number, buses: Bus[]): { busId: number, waitTime: number} {
  let bus: Bus;
  let waitTime = 0;
  while(!bus) {
    bus = buses.find(b => (timestamp + waitTime) % b.id === 0);
    waitTime++;
  }
  waitTime--;
  return { busId: bus.id, waitTime }
}

export function part2(buses: Bus[]) {
  let n = buses[0].offset;
  let mul = buses[0].id;
  for(let bus of buses.slice(1)) {
    while (n % bus.id !== (bus.id - bus.offset) ) n += mul;
    mul *= bus.id;
  }

  return n;
}