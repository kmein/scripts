{mkYarnPackage}:
mkYarnPackage {
  name = "osm-restaurants";
  src = ./.;
  packageJson = ./package.json;
  yarnLock = ./yarn.lock;
}
