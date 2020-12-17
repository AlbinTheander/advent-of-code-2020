import { count } from './util/array';

export default function day04(data: string) {
  const passports = parseData(data);

  const completePassports = count(passports, hasAllFields);
  const validPassports = count(passports, isValid);

  console.log('===== Day 4 =====');
  console.log('There are', completePassports, 'complete passports')
  console.log('There are', validPassports, 'valid passports')
}

export const validators = {
  byr: validYear(1920, 2002),
  iyr: validYear(2010, 2020),
  eyr: validYear(2020, 2030),
  hgt: validHeight,
  hcl: matches(/^#[a-f0-9]{6}$/),
  ecl: matches(/^(amb|blu|brn|gry|grn|hzl|oth)$/),
  pid: matches(/^[0-9]{9}$/),
};


function hasAllFields(passport: String[]): boolean {
  const mandatoryFields = Object.keys(validators);
  const hasField = field => passport.some(f => f.startsWith(field));
  return mandatoryFields.every(hasField);
}

function isValid(passport: String[]): boolean {
  if (!hasAllFields(passport)) return false;
  return passport.every(field => {
    const [key, value] = field.split(':');
    const validator = validators[key];
    return validator ? validator(value) : true;
  })
}

function validHeight(height: String): boolean {
  const [matches, hs, unit] = height.match(/^([0-9]{2,3})(in|cm)$/) || [];
  if (!matches) return false;
  const h = +hs;
  if (unit === 'cm') return h >= 150 && h <= 193;
  return h >= 59 && h <= 76;
}

export function validYear(min, max) {
  return (field: string) => 
    field.length === 4 
    && field >= min
    && field <= max;
}

export function matches(regExp) {
  return (field: String) => regExp.test(field);
}

export function parseData(s): String[][] {
  return s
    .split('\n\n')
    .map(ps => ps.match(/\S{3}:\S+/g))
}