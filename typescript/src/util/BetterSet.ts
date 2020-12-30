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

  delete(t: T) {
    const key = this.keyFn(t);
    this.ts.delete(key);
  }

  has(t: T) {
    const key = this.keyFn(t);
    return this.ts.has(key);
  }

  get size(): number {
    return this.ts.size;
  }

  values() {
    return Array.from(this.ts.values());
  }

  forEach(f: (t: T) => void) {
    this.ts.forEach(f);
  }

}
