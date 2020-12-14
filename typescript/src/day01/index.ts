export default function day1(data: string) {
  const nums = data.split("\n").map(Number);
  const prod1 = part1(nums);
  const prod2 = part2(nums);
  console.log('====== Day 1 ======')
  console.log('The product of the first pair is', prod1);
  console.log('The product of the second triplet is', prod2);
}

function part1(nums: number[]): number {
  for (let i = 0; i < nums.length; i++) {
    for (let j = 0; j < nums.length; j++) {
      if (i !== j && nums[i] + nums[j] === 2020) return nums[i] * nums[j];
    }
  }
  return -1;
}

function part2(nums: number[]): number {
  for (let i = 0; i < nums.length; i++) {
    for (let j = 0; j < nums.length; j++) {
      for (let k = 0; k < nums.length; k++) {
        if (i !== j && i !== k && j !== k && nums[i] + nums[j] + nums[k] === 2020) return nums[i] * nums[j] * nums[k];
      }
    }
  }
  return -1;
}
