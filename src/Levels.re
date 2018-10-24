open Common;

let blank = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};

let map1 = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxx000000000100000x00000000000000000000000x
xxxxxxxxxx000000000001000x00000000000000000000000x
x000000000000000000000000x00000000000000000000000x
xxxxxxxxxx000000000000000x00000000000000000000000x
xxxxxxxxxx000000000000100x00000000000000000000000x
xxxxxxxxxx000000000000100x00000000000000000000000x
xxxxxxxxxx000000000001000x00000000000000000000000x
xxxxxxxxxx000000000100000x00000000000000000000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};
let startPos1 = (11, 3);

let map2 = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x000000000020000000000000x00000000000000000000000x
x000020000000002000000000x00000000000000000000000x
x000000000000000000000000x00000000000000000000000x
x002000000000000002000000x00000000000000000000000x
x000000000000000000000000x00000000000000000000000x
x000002000000002000000000x00000000000000000000000x
x000000000000000000000000x00000000000000000000000x
x000000000020000000000000x00000000000000000000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};
let startPos2 = (11, 5);

let map3 = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x000000000000000000000000x00000000000000000000000x
x000000000000000000000000x00000000000000000000000x
x000000000000000000000000xxxxxxxxxxx0000000000000x
x0000000000000000000000000011110000x0000000000000x
x0000000000000000000000000011110000x0000000000000x
x000000000000000000000000xxxxxxxxxxx0000000000000x
x000000000000000000000000x00000000000000000000000x
x000000000000000000000000x00000000000000000000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};
let startPos3 = (20, 5);

let map4 = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x001111000x00000000000000x00000000000000000000000x
x000000000x10000000000000x00000000000000000000000x
x0000000000x1000000000000xxxxxxxxxxx0000000000000x
x0000000000x1000000000000xx0000000000000000000000x
x00000000000x000000000000xx0000000000000000000000x
x00000000000x000000000220xxxxxxxxxxx0000000000000x
x000000000001000000000220x00000000000000000000000x
x000000000001000000000220x00000000000000000000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};
let startPos4 = (3, 8);

let map5 = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x0011100d0000000000d00000d000200x0000000000000000x
x0000000d2000000000d00020d000000x0200000000000000x
x0000000x0000000000x00000xxddxxxx0000000000000000x
x0000000xxxxxxxddxxx00000d000001d000000d000000000x
xxxxddxxx2000000000d00000d001000d020000d012000010x
x0000000xxxxxx00000d00000x000000d00xxxxxxxddxxxxxx
x0000000d1000000000x00020x000000x0020000000000020x
x0000000d0002000000x00020x000000x0020001000001020x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};
let startPos5 = (3, 8);

let map6 = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x000220000020000000001010d0000000002d000000000000x
x000000000020000000001010d0000000002d000000000000x
x000000000xxxxxxxddxxxxxxxxxxxxxxxxxxxxx000000000x
x000000000xxx002000020000xx010020000100d000000000x
xxxxddxxxxxxx000000000000xx010000020100d000000000x
x00000000000x000000000000xxxxxxxxxxxxxxxxxxxxxxddx
x00000000000x000000000020x00000000000000000000000x
x00000000000x000000000020x00022222200000000000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};
let startPos6 = (3, 8);

let map7 = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x0000000000000x0000000000000000000000000000000000x
x0000000000000x0000000000000000000000000000000000x
x0000000000000x0000000000000000000000000000000000x
x0000000000000d0000000000000000000000000000000000x
x0000000000000d0000000000000000000000000000000000x
x0000000000000x0000000000000000000000000000000000x
xxxxxxxddxxxxxxxxxddxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x0000000000000x0000000000000d00000000000000000000x
x0000000000000x0000000000000d00000000000000000000x
x0000000000000d0000000000000x00000000000000000000x
x0000000000000d0000000000000x00000000000000000000x
x0000000000000xxxxxxxxxxxxxxx00000000000000000000x
x0000000000000x0000000000000000000000000000000000x
x0000000000000x0000000000000000000000000000000000x
xxxxxxxxddxxxxx0000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};
let startPos7 = (10, 6);

let map8 = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x000000000000000000000000x00000000000000000000000x
x000000000000000000000000x00000000000000000000000x
x000000000000000000000000000000000000xx0000000000x
x000000000000000000000000000000000000000000000000x
x0000000000000000000000000xxxxxxx0000000000000000x
x000000xx00xx000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x0000000000000x0000000000000000000000x00000000000x
x0000000000000x0000000000000000000000x00000000000x
x0000000000000x0000000000000000000000x00000000000x
x0000000000000xxxxx00000000000000xxxxx00000000000x
x0000xxx00000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000x000000000000xxx00000000000000x
x000000000x00000000x000000000000x0000000000000000x
x000000000x00000000x00000000000000000000000000000x
x000000000x00000000000000000000000000000000000000x
x000xxxxxxxxxx000000xxx000000000xxxxx000000000000x
x000000000000000000000x000000000x0000000000000000x
x000000000000000000000x000000000x0000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x0000000x0000000000000x00000000000000000000000000x
x0000000x0000000000000x000000000000xxxxxx00000000x
x0000000x00000000xxxxxx00000000000000000000000000x
x000000000000000000000000000000000000000000000000x
x000000000000000000000000000000000000000000000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};
let startPos8 = (10, 6);

let levels = [|
  (startPos1, map1),
  (startPos2, map2),
  (startPos3, map3),
  (startPos4, map4),
  (startPos5, map5),
  (startPos6, map6),
  (startPos7, map7),
  (startPos8, map8),
|];

let numberOfLevels = Array.length(levels);

let parseMap = map => {
  let grid =
    Array.make_matrix(
      gridWidth,
      gridHeight,
      {collision: false, kind: Floor},
    );

  let i = ref(0);
  /* I think the string starts with a newline. */
  let j = ref(-1);
  let enemies = ref([]);
  for (k in 0 to String.length(map) - 1) {
    if (map.[k] == '\n') {
      i := 0;
      j := j^ + 1;
    } else {
      let cell =
        switch (map.[k]) {
        | 'x' => {collision: true, kind: Wall}
        | 'd' => {collision: true, kind: Door}
        | '0' => {collision: false, kind: Floor}
        | '1' =>
          enemies :=
            [
              {
                id: makeEnemyID(),
                pos: {
                  x: float_of_int(i^) *. tileSizef +. tileSizef /. 2.,
                  y: float_of_int(j^) *. tileSizef +. tileSizef /. 2.,
                },
                direction: {
                  x: 0.,
                  y: 0.,
                },
                speed: 120.,
                error: {
                  x: 0.,
                  y: 0.,
                },
                timeUntilNextAttack: 0.,
                forcefullyMovedTimer: 0.,
                path: [],
                kind: Melee,
                bullets: [],
                bulletSpeed: 0.,
                bulletDamage: 0,
                weaponRange: 0.,
                isDead: false,
                bulletLifeSpan: 2.,
              },
              ...enemies^,
            ];

          {collision: false, kind: Floor};
        | '2' =>
          enemies :=
            [
              {
                id: makeEnemyID(),
                pos: {
                  x: float_of_int(i^) *. tileSizef +. tileSizef /. 2.,
                  y: float_of_int(j^) *. tileSizef +. tileSizef /. 2.,
                },
                direction: {
                  x: 0.,
                  y: 0.,
                },
                speed: 120.,
                error: {
                  x: 0.,
                  y: 0.,
                },
                timeUntilNextAttack: 0.,
                forcefullyMovedTimer: 0.,
                path: [],
                kind: Shooter,
                bullets: [],
                bulletSpeed: 300.,
                bulletDamage: 100,
                weaponRange: 200.,
                isDead: false,
                bulletLifeSpan: 2.,
              },
              ...enemies^,
            ];

          {collision: false, kind: Floor};
        | _ => {collision: false, kind: Floor}
        };
      grid[i^][j^] = cell;
      i := i^ + 1;
    };
  };
  (grid, enemies^);
};
