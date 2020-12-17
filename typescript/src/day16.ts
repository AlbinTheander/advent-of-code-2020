export default function day16(data:string) {
  const { fields, myTicket, nearByTickets} = parseData(data);

  const matchField = (x: number, field) => 
    (field.from1 <= x && x <= field.to1) ||
    (field.from2 <= x && x <= field.to2)
  const matchAnyField = (x: number) => fields.some(field => matchField(x, field));

  const allValues = nearByTickets.reduce((a, b) => a.concat(b), []);
  const errorRate = allValues.filter(v => !matchAnyField(v)).reduce((a,b) => a+ b, 0);
  console.log(errorRate);

  const validTickets = nearByTickets.filter(t => t.every(v => matchAnyField(v)));

  const fieldValues = myTicket.map((_, i) => validTickets.map(t => t[i]));
  
  
  const possibleFields = fieldValues
  .map(vs => fields.filter(f => vs.every(v => matchField(v, f))))
  .map((fields, i) => ({ index: i, fields }))
  .sort((pf1, pf2) => pf1.fields.length - pf2.fields.length)
  
  const orderedFields = [];
  possibleFields.forEach(pf => {
    const field = pf.fields.find(f => !orderedFields.includes(f));
    orderedFields[pf.index] = field;
  })
  const result = orderedFields.reduce(
    (prod, f, i) => f.name.startsWith('departure') ? (prod * myTicket[i]) : prod, 1);

  console.log(result);
}

function parseData(data: string) {
  const [fieldData, myTicketData, nearByTicketData] = data.split('\n\n');

  const fields = fieldData.split('\n').map(line => {
    const [_, name, from1, to1, from2, to2] = line.match(/(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)/);
    return {name, from1: +from1, to1: +to1, from2: +from2, to2: +to2}
  });

  const ticketValues = line => line.split(',').map(Number);
  const myTicket = ticketValues(myTicketData.split('\n')[1]);

  const nearByTickets = nearByTicketData.split('\n').slice(1).map(ticketValues);

  return { fields, myTicket, nearByTickets };
}