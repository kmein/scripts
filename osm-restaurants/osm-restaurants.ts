#!/usr/bin/env -S deno run --unstable --allow-env --allow-read --allow-write --allow-net
import { createHash } from "https://deno.land/std@0.165.0/node/crypto.ts";
import { homedir } from "https://deno.land/std@0.165.0/node/os.ts";
import { join } from "https://deno.land/std@0.165.0/path/mod.ts";
import {
  readFileSync,
  mkdirSync,
  statSync,
  writeFileSync,
  existsSync,
} from "https://deno.land/std@0.165.0/node/fs.ts";

import openingHours from "npm:opening_hours@^3.8.0";
import yargs from "https://deno.land/x/yargs/deno.ts";
import { Arguments } from "https://deno.land/x/yargs/deno-types.ts";

const CACHE_AGE = 12 * 60 * 60 * 1000;

interface OsmNode {
  lat: number;
  lon: number;
  tags: { [tag: string]: string };
}

const md5 = (text: string) => createHash("md5").update(text).digest("hex");

const getCachePath = (query: string) => {
  const cacheDirectory =
    Deno.env.get("CACHE_DIR") ||
    join(homedir() || ".", ".cache", "osm-restaurants");
  if (!existsSync(cacheDirectory)) mkdirSync(cacheDirectory);
  return join(cacheDirectory, md5(query) + ".json");
};

const randomElement = (array: any[]): any =>
  array[Math.floor(array.length * Math.random())];

const argv = yargs(Deno.args)
  .option("checkOpen", {
    alias: "o",
    default: true,
    type: "boolean",
    description: "Only show restaurants that are open",
  })
  .option("radius", {
    alias: "r",
    description: "Radius around the location (metres)",
    type: "number",
    default: 500,
  })
  .option("latitude", {
    description: "The latitude coordinates to search around",
    type: "number",
    demandOption: true,
  })
  .option("longitude", {
    description: "The latitude coordinates to search around",
    type: "number",
    demandOption: true,
  })
  .option("endpoint", {
    description: "The Overpass API endpoint",
    type: "string",
    default: "http://overpass-api.de/api/interpreter",
  })
  .option("country", {
    description: "The country you are in (two letter code)",
    type: "string",
    default: "de",
  })
  .help()
  .alias("help", "h").argv;

const overpassQuery = `
  [out:json];
  (
    node(around:${argv.radius},${argv.latitude},${argv.longitude})[amenity=fast_food];
    node(around:${argv.radius},${argv.latitude},${argv.longitude})[amenity=restaurant];
  );
  out;
`;

function restaurantOpen(restaurant: OsmNode): boolean {
  if ("opening_hours" in restaurant.tags)
    try {
      return new openingHours(restaurant.tags.opening_hours, {
        lat: restaurant.lat,
        lon: restaurant.lon,
        address: {
          country_code:
            "addr:country" in restaurant.tags
              ? restaurant.tags["addr:country"].toLowerCase()
              : argv.country,
        },
      }).getState();
    } catch {
      return true;
    }
  else return true;
}

const withRestaurants = (callback: (restaurants: OsmNode[]) => any) => {
  const cachePath = getCachePath(overpassQuery);
  if (
    existsSync(cachePath) &&
    +new Date() - statSync(cachePath).mtime < CACHE_AGE
  )
    callback(JSON.parse(readFileSync(cachePath)));
  else {
    console.error("fetching", overpassQuery);
    return fetch(argv.endpoint, {
      method: "POST",
      body: overpassQuery,
    })
      .then((response) => response.json())
      .then((responseJson) => {
        const restaurants = responseJson.elements;
        writeFileSync(cachePath, JSON.stringify(restaurants));
        callback(restaurants);
      })
      .catch(console.error);
  }
};

withRestaurants((restaurants) => {
  const randomOpenRestaurant = randomElement(
    argv.checkOpen ? restaurants.filter(restaurantOpen) : restaurants
  );

  console.log(JSON.stringify(randomOpenRestaurant));
});
