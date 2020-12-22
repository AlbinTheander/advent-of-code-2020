import fs from 'fs';

type Deck = number[];

export default function day22(data: string) {
  const [deck1, deck2] = parseDecks(data);

  const answer1 = part1(deck1, deck2);
  const answer2 = part2(deck1, deck2);

  console.log('===== Day 22 =====');
  console.log('The winner of the normal game has the score', answer1);
  console.log('The winner of the recursive game has the score', answer2);
}

function part1(deck1: Deck, deck2: Deck): number {
  const winningDeck = playCombat(deck1, deck2);
  const score = scoreCombat(winningDeck);
  return score;
}

function part2(deck1: Deck, deck2: Deck): number {
  const { deck } = playRecursiveCombat(deck1.slice(), deck2.slice());
  const score = scoreCombat(deck);
  return score;
}

function playCombat(originalDeck1: Deck, originalDeck2: Deck): Deck {
  const [deck1, deck2] = [originalDeck1, originalDeck2].map(d => d.slice());
  while(deck1.length > 0 && deck2.length > 0) {
    const c1 = deck1.shift();
    const c2 = deck2.shift();
    if (c1 > c2)
      deck1.push(c1, c2);
    else
      deck2.push(c2, c1);
  }

  return deck1.length > 0 ? deck1 : deck2;
}

function playRecursiveCombat(deck1: Deck, deck2: Deck): { winner: number, deck: Deck } {
  const usedDecks = new Set<string>();
  while(deck1.length > 0 && deck2.length > 0) {
    const deckHash = [deck1, -1, deck2].join(' ');
    if (usedDecks.has(deckHash)) { 
      return { winner: 1, deck: deck1 }; 
    }
    usedDecks.add(deckHash);
    const c1 = deck1.shift();
    const c2 = deck2.shift();
    if (c1 <= deck1.length && c2 <= deck2.length) {
      let { winner } = playRecursiveCombat(deck1.slice(0, c1), deck2.slice(0, c2));
      if (winner === 1)
        deck1.push(c1, c2);
      else
        deck2.push(c2, c1);
    } else if (c1 > c2)
      deck1.push(c1, c2);
    else
      deck2.push(c2, c1);
  }

  if (deck1.length > 0) {
    return {winner: 1, deck: deck1 };
  } else {
    return { winner: 2, deck: deck2 };
  }
}

function scoreCombat(deck: Deck): number {
  return deck.slice().concat(0).reverse().reduce((sum, c, i) => sum + c * i, 0);
}

function parseDecks(data: string): [Deck, Deck] {
  return data
    .split("\n\n")
    .map((deckData) => deckData.split("\n").slice(1).map(Number)) as [
    Deck,
    Deck
  ];
}