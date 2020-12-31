type Field = {
  name: string;
  from1: number;
  to1: number;
  from2: number;
  to2: number;
};
type Ticket = number[];

export default function day16(data: string) {
  const { fields, myTicket, nearbyTickets } = parseData(data);

  const answer1 = part1(nearbyTickets, fields);
  const answer2 = part2(myTicket, nearbyTickets, fields);

  console.log('===== Day 16 =====');
  console.log('The error rate for the tickets is', answer1);
  console.log('The product of all the departure values on my ticket is', answer2);
}

function part1(nearbyTickets: Ticket[], fields: Field[]): number {
  const allValues = nearbyTickets.flat();
  const errorRate = allValues.filter(v => !isValueValidForAnyField(v, fields)).reduce((a, b) => a + b, 0);
  return errorRate;
}

function part2(myTicket: Ticket, nearbyTickets: Ticket[], fields: Field[]) {
  const validTickets = nearbyTickets.filter(t => t.every(v => isValueValidForAnyField(v, fields)));

  // Collect all values for each field position from the nearby tickets
  const fieldValues: number[][] = myTicket.map((_, i) => validTickets.map((t) => t[i]));

  // Create a list of all field positions, including all fields that are eligeble for that position,
  // sorted by how many fields are eligeble for that position
  const possibleFields: { fieldIndex: number, fields: Field[] }[] = fieldValues
    .map((vs) => fields.filter((f) => vs.every((v) => isValueValidForField(v, f))))
    .map((fields, i) => ({ fieldIndex: i, fields }))
    .sort((pf1, pf2) => pf1.fields.length - pf2.fields.length);

  // This array will contain the fields in the same order as on the tickets
  const ticketFields = [];
  possibleFields.forEach((pf) => {
    const field = pf.fields.find((f) => !ticketFields.includes(f));
    ticketFields[pf.fieldIndex] = field;
  });

  // Now, multiply the values on my ticket, for all the fields whose name
  // starts with "departure"
  const result = ticketFields.reduce(
    (prod, f, i) =>
      f.name.startsWith("departure") ? prod * myTicket[i] : prod,
    1
  );

  return result;
}

function isValueValidForField(value: number, field: Field) {
  return (
    (field.from1 <= value && value <= field.to1) ||
    (field.from2 <= value && value <= field.to2)
  );
}

function isValueValidForAnyField(value: number, fields: Field[]): boolean {
  return fields.some(field => isValueValidForField(value, field));
}

function parseData(
  data: string
): { fields: Field[]; myTicket: Ticket; nearbyTickets: Ticket[] } {
  const [fieldData, myTicketData, nearByTicketData] = data.split("\n\n");

  const fields = fieldData.split("\n").map((line) => {
    const [_, name, from1, to1, from2, to2] = line.match(
      /(.*): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)/
    );
    return { name, from1: +from1, to1: +to1, from2: +from2, to2: +to2 };
  });

  const ticketValues = (line) => line.split(",").map(Number);
  const myTicket = ticketValues(myTicketData.split("\n")[1]);

  const nearbyTickets = nearByTicketData.split("\n").slice(1).map(ticketValues);

  return { fields, myTicket, nearbyTickets };
}
