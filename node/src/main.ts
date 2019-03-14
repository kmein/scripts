import createClient, { HafasClient, Location, Journey, Stop, Station } from "hafas-client";
import bvgProfile from "hafas-client/p/bvg";

const client: HafasClient = createClient(bvgProfile, "bvg-cli");

async function main() {
  const journeys = await client.journeys("900000020201", "900000068201", {
    results: 1
  });

  console.log(journeys.journeys[0]);

  const locations = await client.locations("Kaiserin Augusta");

  locations.filter((value) => value.type === "stop").forEach(stop => console.log(stop))
  // locations
  //   .filter((location: Location | Stop | Station) => location.type === "station" || location.type === "stop")
  //   .forEach((stop: Stop | Station) => console.log(stop.id))
}

main()