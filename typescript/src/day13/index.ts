export default function day13(data: string) {
  const lines = data.split('\n');
  const timestamp = +lines[0];
  const buses = lines[1].split(',').map(Number).filter(n => !isNaN(n));

  const answer = part2(lines[1]);
  console.log(answer);
  return;
  let bus;
  let wait = 0;
  while(wait < 500) {
    bus = buses.find(b => (timestamp + wait) % b === 0);
    if (bus) break;
    wait++;
  }
  console.log(buses);
  console.log(bus, wait, bus * wait);
}


export function part2(busLine: string) {
  const buses = busLine.split(',')
    .map((s, i) => ({id: +s, offset: (i % +s)}))
    .filter(b => !isNaN(b.id))

  let n = buses[0].offset;
  let mul = buses[0].id;
  for(let bus of buses.slice(1)) {
    console.log('Doing', bus, n, mul);
    while (n % bus.id !== (bus.id - bus.offset) ) n += mul;
    mul *= bus.id;
  }

  return n;
}