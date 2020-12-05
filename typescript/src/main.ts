import fs from 'fs';

const dayDirs = fs.readdirSync("./src").filter(dir => dir.startsWith('day'));

async function runDay(day: string, input: string = `../data/${day}.txt`) {
  const module = await import(`./${day}`);
  const data = fs.readFileSync(input, 'utf-8')
  module.default(data);
  console.log();
}

function runAll() {
  dayDirs.sort().forEach(async dir => {
    await runDay(dir);
  })
}

function main() {
  const day = process.argv[2];
  const input = process.argv[3];
  if (day) {
    runDay(day, input);
  } else {
    runAll();
  }
}

main();