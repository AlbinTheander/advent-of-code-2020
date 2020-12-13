export default function day12(data: string) {
  const instructions = data.split('\n').map(line => ({instr: line[0], n: +line.slice(1)}));

  const state = { x: 0, y: 0, d: { dy: 0, dx: 1 }}

  const endState = run(state, instructions);

  console.log(endState);
  console.log(Math.abs(endState.x) + Math.abs(endState.y))

  const state2 = { x: 0, y: 0, wx: 10, wy: 1 }
  const endState2 = run2(state2, instructions);
  console.log(endState2);
  console.log(Math.abs(endState2.x) + Math.abs(endState2.y))
}


function run(state, instructions) {
  for (let { instr, n} of instructions) {
    switch (instr) {
      case 'N': state.y += n; break;
      case 'E': state.x += n; break;
      case 'S': state.y -= n; break;
      case 'W': state.x -= n; break;
      case 'F': state.x += state.d.dx * n; state.y += state.d.dy * n; break;
      case 'L': turn(state, -n); break;
      case 'R': turn(state, n); break;
      default: throw Error('Unknown instruction ' + instr + n)
    }
  }
  return state;
}

function turn(state, degrees) {
  let quarters = (degrees / 90 + 4) % 4;
  let { dx, dy } = state.d;
  while (quarters > 0) {
    ([dx, dy] = [dy, -dx]);
    quarters--;
  }

  state.d = {dx, dy};
}

function run2(state, instructions) {
  for (let { instr, n} of instructions) {
    switch (instr) {
      case 'N': state.wy += n; break;
      case 'E': state.wx += n; break;
      case 'S': state.wy -= n; break;
      case 'W': state.wx -= n; break;
      case 'F': state.x += state.wx * n; state.y += state.wy * n; break;
      case 'L': turn2(state, -n); break;
      case 'R': turn2(state, n); break;
      default: throw Error('Unknown instruction ' + instr + n)
    }
  }
  return state;
}

function turn2(state, degrees) {
  let quarters = (degrees / 90 + 4) % 4;
  let { wx, wy } = state;
  while (quarters > 0) {
    ([wx, wy] = [wy, -wx]);
    quarters--;
  }

  state.wx = wx;
  state.wy = wy;
}

