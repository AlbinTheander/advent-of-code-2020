import { reorientTile } from "../day20";

const img = [
  '1234',
  'C..5',
  'B..6',
  'A987',
];

const topfacingRight = [
  'ABC1',
  '9..2',
  '8..3',
  '7654'
];

const bottomFacingRight = [
  '1CBA',
  '2..9',
  '3..8',
  '4567'
];

const leftFacingRight = [
  '4321',
  '5..C',
  '6..B',
  '789A'
];

const topReversedFacingRight = [
  '7654',
  '8..3',
  '9..2',
  'ABC1'
];

const rightReversedFacingRight = [
  'A987',
  'B..6',
  'C..5',
  '1234'
]

const bottomReversedFacingRight = [
  '4567',
  '3..8',
  '2..9',
  '1CBA',
];

const leftReversedFacingRight = [
  '789A',
  '6..B',
  '5..C',
  '4321',
]

test('reorientTile 1', () => {
  expect(reorientTile(({ img } as any), 0).img).toEqual(topfacingRight);
  expect(reorientTile(({ img } as any), 1).img).toEqual(img);
  expect(reorientTile(({ img } as any), 2).img).toEqual(bottomFacingRight);
  expect(reorientTile(({ img } as any), 3).img).toEqual(leftFacingRight);
  expect(reorientTile(({ img } as any), 4).img).toEqual(topReversedFacingRight);
  expect(reorientTile(({ img } as any), 5).img).toEqual(rightReversedFacingRight);
  expect(reorientTile(({ img } as any), 6).img).toEqual(bottomReversedFacingRight);
  expect(reorientTile(({ img } as any), 7).img).toEqual(leftReversedFacingRight);
})