type Allergen = string;
type Ingredient = string;
type Food = { ingredients: Ingredient[]; allergens: Allergen[] };

export default function day20(data: string) {
  const foods = parseFoods(data);

  const answer1 = ingredientsWithoutAllergens(foods);
  const answer2 = ingredientsWithAllergens(foods);

  console.log('===== Day 21 =====');
  console.log('The number of unused ingredients are', answer1);
  console.log('The ingredients with allergens are', answer2);
}

function ingredientsWithoutAllergens(foods: Food[]): number {
  const possibleIngredients = findPossibleIngredientsPerAllergen(foods);
  const allIngredients = foods.flatMap((f) => f.ingredients);
  const usedIngredients = [...possibleIngredients.values()].flatMap((s) => [
    ...s,
  ]);
  const notUsed = allIngredients.filter((i) => !usedIngredients.includes(i));

  return notUsed.length;
}

function ingredientsWithAllergens(foods: Food[]): string {
  const possibleIngredients = findPossibleIngredientsPerAllergen(foods);
  const nrOfAllergens = possibleIngredients.size;

  const allergenIngredients: { allergen: string; ingredient: string }[] = [];
  while (allergenIngredients.length < nrOfAllergens) {
    // allergen with one possible ingredient
    const allergen = [...possibleIngredients.keys()].find(
      allergen => possibleIngredients.get(allergen).size === 1
    );
    const ingredient = possibleIngredients.get(allergen).values().next().value;

    allergenIngredients.push({ allergen, ingredient });
    possibleIngredients.forEach((s) => s.delete(ingredient));
  }

  const sortedIngredients = allergenIngredients
    .sort((ai1, ai2) => ai1.allergen.localeCompare(ai2.allergen))
    .map((ai) => ai.ingredient);

  return sortedIngredients.join(',');
}

function findPossibleIngredientsPerAllergen(
  foods: Food[]
): Map<Allergen, Set<Ingredient>> {
  const pi = new Map<string, Set<string>>(); // pi = possible ingredients

  foods.forEach(({ ingredients, allergens }) => {
    allergens.forEach((a) => {
      pi.set(a, union(new Set(ingredients), pi.get(a)));
    });
  });

  return pi;
}

function union(s1: Set<string>, s2?: Set<string>): Set<string> {
  if (!s2) return s1;
  const s = new Set<string>();
  s1.forEach((a) => {
    if (s2.has(a)) s.add(a);
  });
  return s;
}

function parseFoods(data: string): Food[] {
  const foods = data.split("\n").map((line) => {
    const [ingredientData, allergenData] = line.split(" (contains ");
    const ingredients = ingredientData.match(/[a-z]+/gi);
    const allergens = allergenData.match(/[a-z]+/gi);
    return { ingredients, allergens };
  });
  return foods;
}
