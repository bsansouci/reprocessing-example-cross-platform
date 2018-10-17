let moveSpeed = 30.;
let moveTime = 0.3;
/* TODO: as you continue to play, this could ramp up */
let slowMoveDivisor = 8.;
let possibleFruits = ["banana", "pineapple", "apple", "coconut", "orange"];

let halfPlayerSize = 32;
let tileSize = 40;
let tileSizef = float_of_int(tileSize);
let halfPlayerSizef = float_of_int(halfPlayerSize);
let autoaimDisengageTime = 0.8;
let enemyAttackDistance = 40.;

type aimingPacketT = {
  x: float,
  y: float,
  points: list((float, float)),
  startAimTime: float,
};

type aimT =
  | Nothing
  | Aiming(aimingPacketT);

type bulletT = {
  x: float,
  y: float,
  vx: float,
  vy: float,
};

type splashT = {
  x: float,
  y: float,
  rotation: float,
  width: float,
  height: float,
};

type vec2 = {
  x: float,
  y: float,
};
let zeroVec = {x: 0., y: 0.};

type enemyT = {
  pos: vec2,
  speed: float,
  error: vec2,
  direction: vec2,
  timeUntilNextAttack: float,
  path: list((int, int)),
};

type soundsT = {enemyDeathSound: Reprocessing.soundT};

type weaponKindT =
  | ShootsBehindYou
  | Pistol
  | Shotgun;

type weaponsT = {
  length: int,
  moveTime: float,
  playerTravelDistance: float,
  bulletSpeed: float,
  kind: weaponKindT,
};

type tileKindT =
  | Floor
  | Wall;

type tileT = {
  kind: tileKindT,
  collision: bool,
};

module AssetMap = Map.Make(String);

type state = {
  x: float,
  y: float,
  time: float,
  health: int,
  aim: aimT,
  enemies: list(enemyT),
  splashes: list(splashT),
  bullets: list(bulletT),
  bg: Reprocessing.imageT,
  cachedBackground: Reprocessing.imageT,
  assetMap: AssetMap.t(Reprocessing.imageT),
  sounds: soundsT,
  prevMouseState: bool,
  currentWeaponIndex: int,
  weapons: array(weaponsT),
  grid: array(array(tileT)),
  deathTime: float,
  experiment: int,
  currentMoveTime: float,
  totalMoveTime: float,
  velocity: vec2,
};

let sp = Printf.sprintf;
let print = Printf.printf;

let cellToString = ((x, y)) =>
  "(" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ")";

let gridWidth = 50;
let gridHeight = 10;

let getCell = (grid, (cellX, cellY)) =>
  if (cellX >= 0 && cellX < gridWidth && cellY >= 0 && cellY < gridHeight) {
    grid[cellX][cellY];
  } else {
    {kind: Wall, collision: true};
  };
