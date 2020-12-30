export default function day25(data: string) {
  const [cardPK, doorPK] = data.split('\n').map(Number);

  let cardLoops = 0;
  let subject = 1;
  while (subject !== cardPK) {
    cardLoops++;
    subject = (subject * 7) % 20201227;
  }

  let key = 1;
  for(let i = 0; i < cardLoops; i++) {
    key = (key * doorPK) % 20201227;
  }

  console.log('===== Day 25 =====');
  console.log('The encryption key for the handshake is', key);
}