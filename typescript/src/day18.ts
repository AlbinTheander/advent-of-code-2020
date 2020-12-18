export default function day18(data: string) {
  const lines = data.split('\n');
  
  const sum1 = lines.map(basic).reduce((a, b) => +a + +b, 0);
  const sum2  = lines.map(advanced).reduce((a, b) => +a + +b, 0);
  
  console.log('===== Day 18 =====');
  console.log('The sum of the basic homework problems is', sum1);
  console.log('The sum of the advanced homework problems is', sum2);
}


export function basic(s: string) {
  let expr = s;
  if (/^\S+ . \S+$/.test(expr)) return eval(expr).toString();

  while(expr.includes('('))
    expr = expr.replace(/\([^()]*\)/, e => basic(e.slice(1, -1)));

  while(/[+*]/.test(expr))
    expr = expr.replace(/\S+ . \S+/, advanced);

  return expr;
}

export function advanced(s: string) {
  let expr = s;
  if (/^\S+ . \S+$/.test(expr)) return eval(expr).toString();

  while(expr.includes('('))
    expr = expr.replace(/\([^()]*\)/, e => advanced(e.slice(1, -1)));

  while(expr.includes('+'))
    expr = expr.replace(/\S+ \+ \S+/, advanced);

  while(expr.includes('*'))
    expr = expr.replace(/\S+ \* \S+/, advanced);

  return expr;
}