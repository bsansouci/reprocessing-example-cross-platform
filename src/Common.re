let moveSpeed = 30.;
let moveTime = 0.3;
/* TODO: as you continue to play, this could ramp up */
let slowMoveDivisor = 20.;
let possibleFruits = ["banana", "pineapple", "apple", "coconut", "orange"];

let halfPlayerSize = 32;
let tileSize = 40;
let tileSizef = float_of_int(tileSize);
let halfPlayerSizef = float_of_int(halfPlayerSize);
let autoaimDisengageTime = 0.8;
let enemyAttackDistance = 40.;
let globalScale = 0.75;

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
  timeRemaining: float,
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

type enemyKindT =
  | Melee
  | Shooter;

let makeEnemyID = {
  let nextId = ref(0);
  () => {
    let ret = nextId^;
    nextId := ret + 1;
    ret;
  };
};

type enemyT = {
  id: int,
  pos: vec2,
  speed: float,
  error: vec2,
  direction: vec2,
  timeUntilNextAttack: float,
  forcefullyMovedTimer: float,
  path: list((int, int)),
  kind: enemyKindT,
  bullets: list(bulletT),
  bulletSpeed: float,
  bulletDamage: int,
  bulletLifeSpan: float,
  weaponRange: float,
  isDead: bool,
};

type soundsT = {
  enemyDeathSound: Reprocessing.soundT,
  playerShotSound: Reprocessing.soundT,
};

type weaponKindT =
  | ShootsBehindYou
  | Pistol
  | Shotgun;

type weaponsT = {
  length: int,
  moveTime: float,
  playerTravelDistance: float,
  bulletLifeSpan: float,
  bulletSpeed: float,
  kind: weaponKindT,
};
type powerupKindT =
  | Armor
  | ArmorShootsInCircle
  | ArmorLaser
  | ArmorFreeze;
type powerupT = {
  time: float,
  kind: powerupKindT,
};

type tileKindT =
  | Floor
  | Wall
  | Door
  | Powerup(powerupT);

type tileT = {
  kind: tileKindT,
  collision: bool,
};

module AssetMap = Map.Make(String);

type state = {
  x: float,
  y: float,
  realTime: float,
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
  currentLevel: int,
  currentPowerups: list(powerupT),
  maxScore: int,
  score: int,
  timeSinceLastSpawned: float,
  touches: list((Reprocessing.Events.touchT, float)),
  didTap: bool,
};

let sp = Printf.sprintf;
let print = Printf.printf;

let cellToString = ((x, y)) =>
  "(" ++ string_of_int(x) ++ ", " ++ string_of_int(y) ++ ")";

let gridWidth = 50;
let gridHeight = 30;

let getCell = (grid, (cellX, cellY)) =>
  if (cellX >= 0 && cellX < gridWidth && cellY >= 0 && cellY < gridHeight) {
    grid[cellX][cellY];
  } else {
    {kind: Wall, collision: true};
  };

let setCell = (grid, (cellX, cellY), value) =>
  if (cellX >= 0 && cellX < gridWidth && cellY >= 0 && cellY < gridHeight) {
    grid[cellX][cellY] = value;
  };
