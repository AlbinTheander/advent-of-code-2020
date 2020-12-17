
type Person = string;
type Group = Person[];

export default function day06(data : string) {
  const groups: Group[] = data.split('\n\n').map(g => g.split('\n'));
  const answersFromAnyone = groups.map(someoneAnswered);
  const answersFromEveryone = groups.map(everyoneAnswered);

  console.log("===== Day 6 =====");
  console.log("The sum of each groups answers is", sumCount(answersFromAnyone));
  console.log("The sum of answers from everyone in the group is", sumCount(answersFromEveryone));
}

function someoneAnswered(group: Group): string[] {
  const result = new Set<string>();
  group.forEach(p => p.split('').forEach(a => result.add(a)));
  return [...result];
}

function everyoneAnswered(group: Group) : string[]{
  const possibleAnswers = [...someoneAnswered(group)];
  const allAnswered = possibleAnswers.filter( answer => group.every(person => person.includes(answer)));
  return allAnswered;
}

function sumCount(as: { length: number }[]): number {
  return as.reduce((sum, a) => sum + a.length, 0);
}