import { count } from './util/array';

interface Entry {
  n1: number;
  n2: number;
  char: string;
  password: string;
}

export default function day2(data: string) {
  const entries = parseData(data);
  const answer1 = count(entries, valid1);
  const answer2 = count(entries, valid2);
  console.log('====== Day 2 ======')
  console.log('The number of valid passwords with the old policy is', answer1);
  console.log('The number of valid passwords with the new policy is', answer2);
}


export function parseData(s: string): Entry[] {
  const lines = s.split('\n');
  return lines.map(line => {
    const [_, n1, n2, char, password] = line.match(/(\d+)-(\d+) (.): (.+)/) || [];
    return { n1: +n1, n2: +n2, char, password }
  })
}

function valid1(entry: Entry): boolean {
  const nrOfChars = count(entry.password.split(''), (ch) => ch === entry.char);
  return nrOfChars >= entry.n1 && nrOfChars <= entry.n2;
}

function valid2(entry: Entry): boolean {
  const xor = (b1, b2) => b1 ? !b2 : b2;
  const { n1, n2, char, password } = entry;
  return xor(password[n1-1] === char, password[n2-1] === char);
}