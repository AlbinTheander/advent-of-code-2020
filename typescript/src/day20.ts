import { count } from "./util/array";

interface Tile {
  id: string;
  img: string[];
  edges: string[];
}

const TOP = 0;
const RIGHT_EDGE = 1;
const BOTTOM_EDGE = 2;
const LEFT_EDGE = 3;
const FLIPPED_TOP_EDGE = 4;
const FLIPPED_RIGHT_EDGE = 5;
const FLIPPED_BOTTOM_EDGE = 6;
const FLIPPED_LEFT_EDGE = 7;

export default function day20(data: string) {
  const tiles = parseTiles(data);
  
  const answer1 = part1(tiles);
  const answer2 = part2(tiles);

  console.log('===== Day 20 =====');
  console.log('The product of the corner tile ids is', answer1);
  console.log('The number of hashes that are not part of any sea monster is', answer2);
}

function part1(tiles: Tile[]): number {
  const cornerTiles = getCornerTiles(tiles);
  return cornerTiles.map(t => +t.id.match(/\d+/g)[0]).reduce((a, b) => a * b, 1);
}

function part2(tiles: Tile[]): number {
  const image = buildPuzzle(tiles);
  // The rotation here is hard coded for my input. You should probably try every symmetri
  const hashesWithoutMonsters = searchForMonsters(rotate(image, 2));

  return hashesWithoutMonsters;
}

function getCornerTiles(tiles: Tile[]): Tile[] {
  const allEdges = tiles.flatMap(t => t.edges);
  const sideEdges = allEdges.filter(e => allEdges.indexOf(e) === allEdges.lastIndexOf(e));
  // It's a corner tile if it has 4 edges (2 edges + their flipped versions) that are side edges.
  return tiles.filter(tile => 4 === count(tile.edges, e => sideEdges.includes(e)));
}

function searchForMonsters(rawImage: string[]) {
  const MONSTER = [
    '                  # ',
    '#    ##    ##    ###',
    ' #  #  #  #  #  #   '
  ].map(line => line.split(''));

  const image = rawImage.map(line => line.split(''));

  const isMonster = (x: number, y: number): boolean => 
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
  const hashes = [...image.toString()].filter(ch => ch === '#').length;
  return hashes;
}

function buildPuzzle(tiles: Tile[]) {
  const cornerTile = getCornerTiles(tiles)[0];
  // Start with finding an edge on the corner tile that can connect to another piece.
  // this could be the piece below or the piece to the right
  let edgeIx = cornerTile.edges.findIndex((e) =>
    tiles.some((t) => t !== cornerTile && t.edges.includes(e))
  );
  // Reorient the tile so the edge is the right edge.
  reorientTile(cornerTile, edgeIx);
  // Try to find the tile below.
  let belowTile = findTileAt(tiles, cornerTile, BOTTOM_EDGE);
  // If we didn't find a tile below, the first edge we found should be the bottom one,
  // reorient the corner tile again.
  if (!belowTile[0]) {
    reorientTile(cornerTile, FLIPPED_RIGHT_EDGE);
  }
  // Now the corner tile should be oriented so there is a tile that can connect to the right
  // side and another tile that can connect below.
  edgeIx = 1;
  const puzzle = [];
  let usedTiles = 0;

  // Start building at the corner
  let tile = cornerTile;
  while (usedTiles < tiles.length) {
    // If this isn't the first line, find the leftmost piece for the next line by
    // finding the piece below the leftmost of the current line
    if (puzzle.length > 0) {
      [tile, edgeIx] = findTileAt(tiles, puzzle[puzzle.length - 1][0], BOTTOM_EDGE);
      reorientTile(tile, edgeIx);
      edgeIx = 6;
    }
    let line = [];
    do {
      reorientTile(tile, edgeIx);
      line.push(tile);
      const [nextTile, nextEdgeIx] = findTileAt(tiles, tile, RIGHT_EDGE);
      tile = nextTile;
      edgeIx = oppositeEdge(nextEdgeIx);
    } while (tile);
    usedTiles += line.length;
    puzzle.push(line);
  }

  return joinPuzzle(puzzle);
}

// Takes a 2D array of tiles and join them to one big image, removing the 1-pixel edge of each tile.
function joinPuzzle(puzzle: Tile[][]): string[] {
  const joined = puzzle.flatMap(line => line[0].img.slice(1,-1).map((_, y) => line.reduce((result, tile) => result + tile.img[y+1].slice(1, -1), '')));
  return joined;
}

// Finds a tile that matches the given edge of the given tile. (That is not the same tile.)
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

// Gets the index of the opposite edge. The opposite of top is bottom and
// the opposite of rightFlipped is leftFlipped, etc.
function oppositeEdge(id: number): number {
  return [2, 3, 0, 1, 6, 7, 4, 5][id];
}

// Reorients the tile so that the edge will end up as the right edge.
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

/**
 * Rotates an image a number of 90° turns clockwise
 */
export function rotate(img: string[], quarters: number): string[] {
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

function parseTiles(s: string): Tile[] {
  const tiles = s.split("\n\n").map((tileData) => {
    const [id, ...img] = tileData.split("\n");
    const edges = generateEdges(img);
    return { id, img, edges };
  });
  return tiles;
}

/**
 * The edges are represented by the string along that edge in the following
 * directions:
 *      
 *      ---->
 *     +-----+ 
 *   | |     | |
 *   | |     | |
 *   v |     | v
 *     +-----+
 *      ---->
 * 
 * The flipped directions go in the opposite directions.
 */
function generateEdges(img: string[]): string[] {
  const maxX = img[0].length - 1;
  const maxY = img.length - 1;
  const top = img[0];
  const right = img.map((line) => line[maxX]).join("");
  const bottom = img[maxY];
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
