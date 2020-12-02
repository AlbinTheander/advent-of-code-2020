export function count<T>(xs: T[], pred: (x: T) => boolean): number {
  return xs.filter(pred).length;
}