interface Instruction {
  op: string,
  arg: number
}

interface State1 {
  x: number;
  y: number;
  dx: number;
  dy: number;
}

interface State2 {
  x: number;
  y: number;
  wx: number;
  wy: number;
}

export default function day12(data: string) {
  const instructions = data.split('\n').map(line => ({ op: line[0], arg: +line.slice(1)}));

  const answer1 = part1(instructions);
  const answer2 = part2(instructions);

  console.log('===== Day 12 =====');
  console.log('With the first navigation, the ship ends up', answer1, 'units from home');
  console.log('With the second navigation, it ends up', answer2, 'units from home.')
}

function part1(instructions: Instruction[]) {
  const state = { x: 0, y: 0, dx: 1, dy: 0 }
  const endState = run1(state, instructions);
  return Math.abs(endState.x) + Math.abs(endState.y);
}

function part2(instructions: Instruction[]) {
  const state = { x: 0, y: 0, wx: 10, wy: 1 }
  const endState = run2(state, instructions);
  return Math.abs(endState.x) + Math.abs(endState.y);
}

function run1(state: State1, instructions: Instruction[]) {
  for (let { op, arg} of instructions) {
    switch (op) {
      case 'N': state.y += arg; break;
      case 'E': state.x += arg; break;
      case 'S': state.y -= arg; break;
      case 'W': state.x -= arg; break;
      case 'F': state.x += state.dx * arg; state.y += state.dy * arg; break;
      case 'L': turn1(state, -arg); break;
      case 'R': turn1(state, arg); break;
      default: throw Error('Unknown instruction ' + op + arg)
    }
  }
  return state;
}

function turn1(state: State1, degrees: number) {
  let quarters = (degrees / 90 + 4) % 4;
  let { dx, dy } = state;
  while (quarters > 0) {
    ([dx, dy] = [dy, -dx]);
    quarters--;
  }

  state.dx = dx;
  state.dy = dy;
}

function run2(state: State2, instructions: Instruction[]) {
  for (let { op, arg } of instructions) {
    switch (op) {
      case 'N': state.wy += arg; break;
      case 'E': state.wx += arg; break;
      case 'S': state.wy -= arg; break;
      case 'W': state.wx -= arg; break;
      case 'F': state.x += state.wx * arg; state.y += state.wy * arg; break;
      case 'L': turn2(state, -arg); break;
      case 'R': turn2(state, arg); break;
      default: throw Error('Unknown instruction ' + op + arg)
    }
  }
  return state;
}

function turn2(state: State2, degrees: number) {
  let quarters = (degrees / 90 + 4) % 4;
  let { wx, wy } = state;
  while (quarters > 0) {
    ([wx, wy] = [wy, -wx]);
    quarters--;
  }

  state.wx = wx;
  state.wy = wy;
}

