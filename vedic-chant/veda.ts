import { readLines } from "https://deno.land/std/io/buffer.ts";

enum Category {
  LongVowel = "A",
  ShortVowel = "a",
  NoVowel = "0",
  Consonant = "C",
  Low = "-",
  High = "+",
  Anusvara = "M",
  Visarga = "H",
  Space = "_",
  Pause = " ",
  One = "1",
}

function category(devanagari: string): Category {
  switch (devanagari) {
    case "ि":
    case "ु":
    case "अ":
    case "इ":
    case "उ":
    case "ृ":
    case "ऋ":
    case "ॢ":
    case "ऌ":
      return Category.ShortVowel;
    case "ी":
    case "ू":
    case "ो":
    case "े":
    case "ा":
    case "ई":
    case "ऊ":
    case "ओ":
    case "ए":
    case "औ":
    case "ऐ":
    case "ै":
    case "ौ":
    case "ॄ":
    case "ॣ":
    case "ॠ":
    case "ॡ":
      return Category.LongVowel;
    case "॒":
      return Category.Low;
    case "॑":
      return Category.High;
    case "ं":
    case "म":
    case "ँ":
      return Category.Anusvara;
    case "्":
      return Category.NoVowel;
    case " ":
    case " ":
    case "ऽ":
      return Category.Space;
    case "\n":
      return Category.Pause;
    case "१":
      return Category.One;
    case "ः":
      return Category.Visarga;
    default:
      return Category.Consonant;
  }
}

for await (const line of readLines(Deno.stdin)) {
  const tokenized = [...line]
    .map(category)
    .join("")
    .replace(/C([^a0A])/g, (x) => `Ca${x[1]}`)
    .replace(/M([^a0A_])/g, (x) => `Ma${x[1]}`)
    .replace(/M([aA])/g, (x) => `C${x[1]}`)
    .replace(/C0/g, "C")
    .replace(/M0/g, "M");

  console.error(tokenized);

  const vowels = tokenized.match(/[Aa]1?[+-]*M?/g);

  for (const vowel of vowels) {
    if (vowel == "a-") {
      console.log("246 250");
    } else if (vowel == "a") {
      console.log("275 250");
    } else if (vowel == "a+") {
      console.log("296 250");
    } else if (vowel == "A-" || vowel == "aM-" || vowel == "AM-") {
      console.log("246 1000");
    } else if (vowel == "A" || vowel == "aM" || vowel == "AM") {
      console.log("275 1000");
    } else if (vowel == "A+" || vowel == "aM+" || vowel == "AM+") {
      console.log("275 500");
      console.log("296 1000");
    } else if (vowel == "a1-+" || vowel == "A1-+") {
      console.log("296 250");
      console.log("275 1000");
    }
    // console.log("0 50");
  }
  console.log("0 200");
}
