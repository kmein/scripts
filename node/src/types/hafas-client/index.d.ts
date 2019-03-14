// https://github.com/public-transport/friendly-public-transport-format/blob/1.2.0/spec/readme.md
// https://github.com/public-transport/hafas-client/blob/master/docs/reachable-from.md
/// <reference types="node" />

declare module "hafas-client/p/bvg" {
  const bvgClient: any;
  export default bvgClient;
}

declare module "hafas-client" {
  export type ID<A> = string;

  type IDObject<A> = ID<A> | A;

  type IDObjectArray<A> = ID<A>[] | A[];

  export interface Location {
    type: "location";
    name?: string;
    address?: string;

    // if longitude is present, latitude must also be present. how?
    longitude?: number;
    latitude?: number;
    altitude?: number;
  }

  export interface Station {
    type: "station";
    id: ID<Station>;
    name: string;
    location?: Location;
    regions?: IDObjectArray<Region>;
  }

  export interface Stop {
    type: "stop";
    id: ID<Stop>;
    station: IDObject<Station>;
    location?: Location;
  }

  export interface Region {
    type: "region";
    id: ID<Region>;
    name: string;
    stations: IDObjectArray<Station>;
  }

  export interface Line {
    type: "line";
    id: ID<Line>;
    name: string;
    mode: Mode;
    subMode?: unknown;

    routes?: IDObjectArray<Route>;
    operator?: IDObject<Operator>;
  }

  export interface Route {
    type: "route";
    id: ID<Route>;
    line: IDObject<Line>;
    mode?: Mode;
    subMode?: unknown;
    stops: IDObjectArray<Stop> | IDObjectArray<Station>;
  }

  export interface Schedule {
    type: "schedule";
    id: ID<Schedule>;
    route: IDObject<Route>;
    mode?: Mode;
    subMode?: unknown;
    sequence?: { arrival: number; departure: number }[];
    starts: number[];
  }

  export interface Operator {
    type: "operator";
    id: ID<Operator>;
    name: string;
  }

  export interface Stopover {
    type: "stopover";
    stop: IDObject<Stop> | IDObject<Station>;
    arrival?: string;
    arrivalDelay?: number;
    departure?: string;
    departureDelay?: number;
    departurePlatform?: string;
  }

  export interface Price {
    amount: number;
    currency: string;
  }

  export interface Trip {
    origin: IDObject<Station> | IDObject<Stop> | IDObject<Location>;
    destination: IDObject<Station> | IDObject<Stop> | IDObject<Location>;
    departure: string;
    departureDelay?: number;
    departurePlatform?: string;
    arrival: string;
    arrivalDelay?: number;
    arrivalPlatform?: string;
    stopovers: Stopover[];
    schedule?: IDObject<Schedule>;
    mode?: Mode;
    subMode?: unknown;
    public: boolean;
    operator: IDObject<Operator>;
    price?: Price;
  }

  export interface Journey {
    type: "journey";
    id: ID<Journey>;
    legs: Trip[];
    price?: Price;
  }

  export type Mode =
    | "train"
    | "bus"
    | "watercraft"
    | "taxi"
    | "gondola"
    | "aircraft"
    | "car"
    | "bicycle"
    | "walking";

  export type Accessibility = "none" | "partial" | "complete";

  type JourneyRef = string;

  export interface HafasClient {
    journeys(
      from: IDObject<Station> | Location & { poi: true } | Location,
      to: IDObject<Station> | Location & { poi: true } | Location,
      opt?: Partial<JourneysOptions>
    ): Promise<{
      journeys: Journey[];
      earlierRef: JourneyRef;
      laterRef: JourneyRef;
    }>;

    refreshJourney(
      refreshToken: string,
      opt?: Partial<RefreshJourneyOptions>
    ): Promise<Journey>;

    trip(
      id: ID<Trip>,
      lineName: string,
      opt?: Partial<TripOptions>
    ):
      | Trip & {
          id: ID<Trip>;
        }
      | Trip & { id: ID<Trip>; polyline: any[] };
    // polyline: geojson FeatureCollection of Points

    locations(
      query: string,
      opt?: Partial<LocationsOptions>
    ): Promise<(Stop | Location)[]>;

    stop(
      id: IDObject<Stop> | IDObject<Station>,
      opt?: Partial<StopOptions>
    ): (Stop | Station) & {
      products: { [product: string]: boolean };
      lines: Line[];
    };

    nearby(
      location: Location,
      opt?: Partial<NearbyOptions>
    ): (Location | Stop | Station)[];

    radar(
      compass: { north: number; west: number; south: number; east: number },
      opt?: Partial<RadarOptions>
    ): {
      location: Location;
      line: Line;
      direction: string;
      trip: number;
      nextStopovers: Stopover[];
      frames: {
        origin: IDObject<Station> | IDObject<Stop> | IDObject<Location>;
        destination: IDObject<Station> | IDObject<Stop> | IDObject<Location>;
        t: number;
      }[];
    }[];

    reachableFrom(address: Location, opt?: Partial<ReachableFromOptions>): any;
  }

  export function createClient(name: string): HafasClient;
  export function createClient(profile: any, name: string): HafasClient;

  export default createClient;

  interface JourneysOptions {
    departure: Date;
    arrival: Date;

    earlierThan: JourneyRef;
    laterThan: JourneyRef;

    results: number;
    via: IDObject<Station>;
    stopovers: boolean;
    transfers: number;
    transferTime: number;
    accessibility: Accessibility;
    bike: boolean;
    products: { [product: string]: boolean };
    tickets: boolean;
    polylines: boolean;
    remarks: boolean;

    startWithWalking: boolean;
    language: string;
    scheduledDelays: boolean;
  }

  interface RefreshJourneyOptions {
    stopovers: boolean;
    polylines: boolean;
    tickets: boolean;
    remarks: boolean;
    language: string;
  }

  interface TripOptions {
    stopovers: boolean;
    polyline: false;
    remarks: true;
    language: string;
  }

  interface LocationsOptions {
    fuzzy: boolean;
    results: number;
    stops: boolean;
    addresses: boolean;
    poi: boolean;
    linesOfStops: boolean;
    language: string;
  }

  interface StopOptions {
    linesOfStops: boolean;
    language: string;
  }

  interface NearbyOptions {
    results: number;
    distance: number;
    poi: boolean;
    stops: boolean;
    linesOfStops: boolean;
    language: string;
  }

  interface RadarOptions {
    results: number;
    duration: number;
    frames: number;
    polylines: boolean;
    language: string;
  }

  interface ReachableFromOptions {
    when: Date,
    maxTransfers: number,
    maxDuration: number,
    products: {
      [product: string]: boolean
    }
  }
}
