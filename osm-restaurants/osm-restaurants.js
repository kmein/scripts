#!/usr/bin/env node
const crypto = require("crypto");
const fs = require("fs");
const os = require("os");
const path = require("path");

const axios = require("axios");
const openingHours = require("opening_hours");
const yargs = require("yargs");

const CACHE_AGE = 12 * 60 * 60 * 1000;

const md5 = (text) => crypto.createHash("md5").update(text).digest("hex");

const getCachePath = (query) => {
  const cacheDirectory =
    process.env.CACHE_DIR ||
    path.join(os.homedir(), ".cache", "osm-restaurants");
  if (!fs.existsSync(cacheDirectory)) fs.mkdirSync(cacheDirectory);
  return path.join(cacheDirectory, md5(query) + ".json");
};

const randomElement = (array) =>
  array[Math.floor(array.length * Math.random())];

const argv = yargs
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

const restaurantOpen = (restaurant) =>
  "opening_hours" in restaurant.tags
    ? new openingHours(restaurant.tags.opening_hours, {
        lat: restaurant.lat,
        lon: restaurant.lon,
        address: {
          country_code:
            "addr:country" in restaurant.tags
              ? restaurant.tags["addr:country"].toLowerCase()
              : argv.country,
        },
      }).getState()
    : true;

const withRestaurants = (callback) => {
  const cachePath = getCachePath(overpassQuery);
  if (
    fs.existsSync(cachePath) &&
    new Date() - fs.statSync(cachePath).mtime < CACHE_AGE
  )
    callback(JSON.parse(fs.readFileSync(cachePath)));
  else {
    console.log("fetching");
    return axios
      .post(argv.endpoint, overpassQuery)
      .then((response) => {
        const restaurants = response.data.elements;
        fs.writeFileSync(cachePath, JSON.stringify(restaurants));
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
