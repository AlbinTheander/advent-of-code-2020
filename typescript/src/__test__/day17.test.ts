import { cube } from "../day17"

const pToString = (p: number[]) => p.join('_');

test('cube works in 2d', () => {
  const values = cube([1, 5]);
 
  expect(values).toHaveLength(9);
 
  for(let x = 0; x <= 2; x++)
    for(let y = 4; y <= 6; y++)
      expect(values).toContainEqual([x, y]);
});

test('cube works in 3d', () => {
  const values = cube([1, 5, 12]);

  expect(values).toHaveLength(27);

  for(let x = 0; x <= 2; x++)
    for(let y = 4; y <= 6; y++)
      for(let z = 11; z <= 13; z++)
        expect(values).toContainEqual([x, y, z]);
});