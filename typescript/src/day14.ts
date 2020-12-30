type Instruction = { op: 'mask', mask: string, orMask: bigint, andMask: bigint }
                 | { op: 'mem', adr: number, v: bigint };

type Program = Instruction[];
type Memory = Map<number, bigint>;

export default function day14(data: string) {
  const program = parseData(data);
  const answer1 = part1(program);
  const answer2 = part2(program);

  console.log('===== Day 14 =====');
  console.log('The total memory sum after executing the program on a version 1 chip is', answer1);
  console.log('The total memory sum after executing the program on a version 2 chip is', answer2);
}

function parseData(data: string): Program {
  return data.split('\n').map(line => {
    if (line.startsWith('mask')) {
      const [_, mask] = line.split(' = ');
      const orMask = BigInt(parseInt(mask.replace(/X/g, '0'), 2));
      const andMask = BigInt(parseInt(mask.replace(/X/g, '1'), 2));
      return { op: 'mask', mask, andMask, orMask };
    }
    const [n1, n2] = line.match(/[0-9]+/g);
    return { op: 'mem', adr: +n1, v: BigInt(+n2) };
  })
}

function part1(program: Program): bigint {
  const mem = run(program);
  const sum = [...mem.values()].reduce((a, b) => a+b, 0n);
  return sum;
}

function part2(program: Program): bigint {
  const mem = run2(program);
  const sum = [...mem.values()].reduce((a, b) => a+b, 0n);
  return sum;
}

function run(program: Program): Memory {
  const mem = new Map<number, bigint>();
  let andMask = 0xffffffffffn;
  let orMask = 0x0n;
  for(const instr of program) {
    switch(instr.op) {
      case 'mask': andMask = instr.andMask; orMask = instr.orMask; break;
      case 'mem': mem.set(instr.adr, instr.v & andMask | orMask); break;
    }
  }
  return mem;
}

function run2(program: Program): Memory {
  const mem = new Map();
  let mask = '';
  for(const instr of program) {
    switch(instr.op) {
      case 'mask': mask = instr.mask; break;
      case 'mem': write(mem, instr.adr, instr.v, mask); break;
    }
  }
  return mem;
}

function write(mem: Memory, adr: number, v: bigint, mask: string) {
  const adrS = adr.toString(2).padStart(36, '0').split('');
  
  const doIt = (m) => {
    if (m.includes('X')) {
      doIt(m.replace('X', '0'));
      doIt(m.replace('X', '1'));
      return;
    }
    const a = parseInt(adrS.map((b, i) => m[i] === '.' ? b : m[i]).join(''), 2);
    mem.set(a, v);
  }

  doIt(mask.replace(/0/g, '.'));
}