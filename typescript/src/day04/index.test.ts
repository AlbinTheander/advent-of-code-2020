import { validators } from ".";

test('Birth year', () => {
  const byr = validators.byr;
  expect(byr('1920')).toBeTruthy();
  expect(byr('2003')).toBeFalsy();

  expect(byr('1919')).toBeFalsy();
  expect(byr('2021')).toBeFalsy();
  expect(byr('0xff')).toBeFalsy();
  expect(byr('2010a')).toBeFalsy();
});
test('Height', () => {
  const hgt = validators.hgt;
  expect(hgt('150cm')).toBeTruthy();
  expect(hgt('191cm')).toBeTruthy();
  expect(hgt('149cm')).toBeFalsy();
  expect(hgt('194cm')).toBeFalsy();
})

test('Hair color', () => {
  const hair = validators.hcl;
  expect(hair('#765111')).toBeTruthy()
})