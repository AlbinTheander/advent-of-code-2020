export default function day14(data: string) {
  const program = parseData(data);
  const mem = run2(program);
  const sum = [...mem.values()].reduce((a, b) => a + b, 0n);
  console.log(sum);
}

function parseData(data: string) {
  return data.split('\n').map(line => {
    if (line.startsWith('mask')) {
      const [_, mask] = line.split(' = ');
      const orMask = BigInt(parseInt(mask.replace(/X/g, '0'), 2));
      const andMask = BigInt(parseInt(mask.replace(/X/g, '1'), 2));
      return { instr: 'mask', mask, andMask, orMask };
    }
    const [n1, n2] = line.match(/[0-9]+/g);
    return { instr: 'mem', adr: +n1, v: BigInt(+n2) };
  })
}

function run(program) {
  const mem = new Map();
  let andMask = 0xffffffffff;
  let orMask = 0x0;
  for(const instr of program) {
    switch(instr.instr) {
      case 'mask': andMask = instr.andMask; orMask = instr.orMask; break;
      case 'mem': mem.set(instr.adr, instr.v & andMask | orMask); break;
    }
  }
  return mem;
}

function run2(program) {
  const mem = new Map();
  let mask = '';
  for(const instr of program) {
    switch(instr.instr) {
      case 'mask': mask = instr.mask; break;
      case 'mem': write(mem, instr.adr, instr.v, mask); break;
    }
  }
  return mem;
}

function write(mem, adr, v, mask) {
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