import fs from 'fs';

const dayDirs = fs.readdirSync("./src").filter(dir => dir.startsWith('day'));

dayDirs.sort().forEach(dir => {
  import(`./${dir}`).then(day => {
    day.default()
    console.log();
  });
})
