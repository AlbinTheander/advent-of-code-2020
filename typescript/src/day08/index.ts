interface Instruction {
  instr: string;
  arg: number;
}

type Program = Instruction[];

interface GameConsole {
  ip: number;
  acc: number;
  crashed: boolean;
  stopped: boolean;
}

export default function day08(data: string) {
  
  const program: Program = data.split('\n').map(line => {
    const [instr, arg] = line.split(' ');
    return { instr, arg: +arg };
  });

  const answer1 = part1(program);
  const answer2 = part2(program);

  console.log('===== Day08 =====');
  console.log('When the program starts looping, the accumulator is', answer1);
  console.log('After fixing the program, the end result is', answer2);
}

function part1(program: Program): number {
  const machine = runProgram(program);
  return machine.acc;
}

function part2(program: Program): number {
  for(let i = 0; i < program.length; i++) {
    const {instr, arg} = program[i];
    if (instr === 'acc') continue;
    const copy = [...program];
    copy[i] = (instr === 'jmp' ) ? { instr: 'nop', arg } : { instr: 'jmp', arg };
    const { crashed, acc } = runProgram(copy);
    if (!crashed) return acc
  }
  return NaN;
}

function runProgram(program: Program): GameConsole {
  const machine = { ip: 0, acc: 0, crashed: false, stopped: false };
  const rom = [...program];
  
  while(!machine.stopped) {
    const { instr, arg } = rom[machine.ip];
    rom[machine.ip] = { instr: 'brk', arg: 0 };
    switch(instr) { 
      case 'nop': break;
      case 'acc': machine.acc += arg; break;
      case 'jmp': machine.ip += (arg - 1); break;
      case 'brk': machine.crashed = true; break;
      default: throw Error(`Unknown instruction: ${instr}`);
    }

    machine.ip++;
    machine.stopped = machine.crashed || machine.ip < 0 || machine.ip >= rom.length;
  }

  return machine;
}