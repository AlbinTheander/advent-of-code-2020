import { count } from "./util/array";

interface Tile {
  id: string;
  img: string[];
  edges: string[];
}

export default function day20(data: string) {
  const tiles = parseTiles(data);
  const edgeMap = new Map<string, Tile[]>();
  tiles.forEach((tile) => {
    tile.edges.forEach((edge) => {
      const edgeTiles = edgeMap.get(edge) || [];
      edgeTiles.push(tile);
      edgeMap.set(edge, edgeTiles);
    });
  });

  // Answer 1
  // const cornerTile = tiles.filter(tile => 4 === count(tile.edges, e => edgeMap.get(e).length === 1));
  // console.log(cornerTile.map(t => +t.id.match(/\d+/g)[0]).reduce((a, b) => a * b, 1));

  const cornerTile = tiles.find(
    (tile) => 4 === count(tile.edges, (e) => edgeMap.get(e).length === 1)
  );
  const image = buildPuzzle(tiles, cornerTile);
  searchForMonsters(image);
}

function searchForMonsters(rawImage: string[]) {
  const MONSTER = [
    '                  # ',
    '#    ##    ##    ###',
    ' #  #  #  #  #  #   '
  ].map(line => line.split(''));
  rawImage = rotate(rawImage, 2);
  console.log(rawImage.join('\n'));
  const image = rawImage.map(line => line.split(''));
  const isMonster = (x, y) => 
    x < image[y].length - MONSTER[0].length &&
    y < image.length - MONSTER.length &&
    MONSTER.every((mline, my) => mline.every((mch, mx) => MONSTER[my][mx] === ' ' || image[y+my][x+mx] === '#'))
  const markMonster = (x, y) => MONSTER.forEach((_, my) => MONSTER[my].forEach((ch, mx) => { if (ch === '#') image[y+my][x+mx] = 'O'}));

  let count = 0;
  for(let y = 0; y < image.length; y++)
    for(let x = 0; x < image[0].length; x++) {
      if (isMonster(x, y)) {
        count++;
        markMonster(x, y);
      }
    }
  console.log(count);
  console.log(image.map(line => line.join('')).join('\n'));
  const hashes = [...image.toString()].filter(ch => ch === '#').length;
  console.log(hashes);
  
}

function buildPuzzle(tiles: Tile[], cornerTile: Tile) {
  let edgeIx = cornerTile.edges.findIndex((e) =>
    tiles.some((t) => t !== cornerTile && t.edges.includes(e))
  );
  reorientTile(cornerTile, edgeIx);
  let belowTile = findTileAt(tiles, cornerTile, 2);
  console.log("Below1", belowTile);
  if (!belowTile[0]) {
    reorientTile(cornerTile, 5);
    console.log(findTileAt(tiles, cornerTile, 2));
  }
  edgeIx = 1;
  const puzzle = [];
  let usedTiles = 0;

  while (usedTiles < tiles.length) {
    let tile = cornerTile;
    if (puzzle.length > 0) {
      [tile, edgeIx] = findTileAt(tiles, puzzle[puzzle.length - 1][0], 2);
      reorientTile(tile, edgeIx);
      edgeIx = 6;
    }
    let line = [];
    do {
      reorientTile(tile, edgeIx);
      line.push(tile);
      const [nextTile, nextEdgeIx] = findTileAt(tiles, tile, 1);
      tile = nextTile;
      edgeIx = oppositeEdge(nextEdgeIx);
    } while (tile);
    usedTiles += line.length;
    puzzle.push(line);
  }

  return joinPuzzle(puzzle);
}

function joinPuzzle(puzzle: Tile[][]): string[] {
  const joined = puzzle.flatMap(line => line[0].img.slice(1,-1).map((_, y) => line.reduce((result, tile) => result + tile.img[y+1].slice(1, -1), '')));
  return joined;
}

function findTileAt(
  tiles: Tile[],
  tile: Tile,
  edgeIdx: number
): [Tile, number] {
  const edge = tile.edges[edgeIdx];
  for (let t of tiles) {
    let nextEdgeIx = t.edges.findIndex((e) => e === edge);
    if (t !== tile && nextEdgeIx >= 0) {
      return [t, nextEdgeIx];
    }
  }
  return [null, -1];
}

function oppositeEdge(id: number): number {
  return [2, 3, 0, 1, 6, 7, 4, 5][id];

  // 0 1 2
  // 3 . 4
  // 5 6 7
}

export function reorientTile(tile: Tile, rightEdgeIx: number) {
  let { img } = tile;
  switch (rightEdgeIx) {
    case 0:
      img = rotate(img, 1);
      break;
    case 1:
      break;
    case 2:
      img = rotate(flip(img), 1);
      break;
    case 3:
      img = rotate(flip(img), 2);
      break;
    case 4:
      img = rotate(flip(img), 3);
      break;
    case 5:
      img = flip(img);
      break;
    case 6:
      img = rotate(img, 3);
      break;
    case 7:
      img = rotate(img, 2);
      break;
  }
  tile.img = img;
  tile.edges = generateEdges(img);
  return tile;
}

export function rotate(img: string[], quarters: number) {
  if (quarters === 0) return img;
  const size = img.length - 1;
  const newImg = img.map((_, y) =>
    img.reduce((result, _, x) => result + img[size - x][y], "")
  );
  return rotate(newImg, quarters - 1);
}

function flip(img: string[]): string[] {
  return img.slice().reverse();
}

function parseTiles(s: string) {
  const tiles = s.split("\n\n").map((tileData) => {
    const [id, ...img] = tileData.split("\n");
    const edges = generateEdges(img);
    return { id, img, edges };
  });
  return tiles;
}

function generateEdges(img: string[]): string[] {
  const r = img[0].length - 1;
  const b = img.length - 1;
  const top = img[0];
  const right = img.map((line) => line[r]).join("");
  const bottom = img[b];
  const left = img.map((line) => line[0]).join("");

  const [topFlipped, rightFlipped, bottomFlipped, leftFlipped] = [
    top,
    right,
    bottom,
    left,
  ].map((l) => l.split("").reverse().join(""));

  return [
    top,
    right,
    bottom,
    left,
    topFlipped,
    rightFlipped,
    bottomFlipped,
    leftFlipped,
  ];
}
