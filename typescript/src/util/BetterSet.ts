export default class BetterSet<T> {
  private ts = new Map<string, T>();
  private keyFn: (t: T) => string

  constructor(keyFn: (t: T) => string) {
    this.keyFn = keyFn;
  }

  add(t: T) {
    const key = this.keyFn(t);
    this.ts.set(key, t);
  }

  has(t: T) {
    const key = this.keyFn(t);
    return this.ts.has(key);
  }

  values() {
    return Array.from(this.ts.values());
  }
}
