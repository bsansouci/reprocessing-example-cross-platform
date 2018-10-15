let _DEBUG = 1;

let _NUMBER_OF_EXPERIMENTS = 2;

let _JOYSTICK = 2;

open Reprocessing;
open Common;

let drawWithRotation = (img, ~pos as (x, y), ~height, ~width, ~rot, env) => {
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.rotate(rot, env);
  Draw.translate(~x=width /. (-2.), ~y=height /. (-2.), env);
  Draw.imagef(img, ~pos=(0., 0.), ~height, ~width, env);
  Draw.popMatrix(env);
};

let dropLast = li => {
  let rec loop = (newList, tailOldList) =>
    switch (tailOldList) {
    | [] => failwith("dropLast encountered an empty list")
    | [el] => newList
    | [el, ...rest] => loop([el, ...newList], rest)
    };
  loop([], li);
};

let initList = (size, func) => {
  let rec loop = (sizeLeft, li) =>
    if (sizeLeft == 0) {
      li;
    } else {
      loop(sizeLeft - 1, [func(), ...li]);
    };
  loop(size, []);
};

let splitListAt = (li, index) => {
  let rec loop = (headList, tailList, i) =>
    switch (tailList) {
    | [] =>
      if (i == index) {
        (List.rev(headList), tailList);
      } else {
        invalid_arg("index was too big for the given list");
      }
    | [el, ...restOfTail] =>
      if (i == index) {
        (List.rev(headList), tailList);
      } else {
        loop([el, ...headList], restOfTail, i + 1);
      }
    };
  loop([], li, 0);
};

let bulletCollidesWithEnemies = ({x, y}: bulletT, enemies) => {
  let rec loop = (enemiesRemaining, index) =>
    switch (enemiesRemaining) {
    | [] => None
    | [{pos: {x: sx, y: sy}}, ...restOfPineapples] =>
      if (Utils.distf(~p1=(sx, sy), ~p2=(x, y)) < 30.) {
        Some(index);
      } else {
        loop(restOfPineapples, index + 1);
      }
    };
  loop(enemies, 0);
};

let loadAssetMap = (env, possibleFruits) => {
  let files: list((string, string)) =
    List.flatten(
      List.map(
        fruit =>
          List.map(
            suf => (
              "./assets/" ++ fruit ++ suf ++ "_small.png",
              fruit ++ suf,
            ),
            ["", "_half_1", "_half_2"],
          ),
        possibleFruits,
      ),
    );
  let files = [
    ("./assets/splash_red_small.png", "splash_red"),
    ("./assets/robot1_small.png", "robot1"),
    ("./assets/player_small.png", "player"),
    ...files,
  ];
  List.fold_left(
    (assetMap, (filename, name)) =>
      AssetMap.add(name, Draw.loadImage(~filename, env), assetMap),
    AssetMap.empty,
    files,
  );
};

let getVisibleEnemies = (state, env) => {
  let padding = 0.;
  let w = float_of_int(Env.width(env));
  let h = float_of_int(Env.height(env));
  List.filter(
    ({pos: {x, y}}) =>
      abs_float(state.x -. x) < w
      /. 2.
      +. padding
      && abs_float(state.y -. y) < h
      /. 2.
      +. padding,
    state.enemies,
  );
};

let bulletIsOutOfRange = (state, bullet: bulletT, env) => {
  let padding = (-10.);
  let screenCoordX = state.x -. bullet.x;
  let screenCoordY = state.y -. bullet.y;
  screenCoordX > float_of_int(Env.width(env))
  -. padding
  && screenCoordX < padding
  && screenCoordY > float_of_int(Env.height(env))
  -. padding
  && screenCoordY < padding;
};

let minFruitDistanceForAimAssist = 100.;
let aimAssist = (state, dx, dy, mag, env) => {
  let (newdx, newdy, cosangle) =
    List.fold_left(
      ((closestX, closestY, closestDot), {pos: {x, y}}) => {
        let (vecToFruitX, vecToFruitY) = (state.x -. x, state.y -. y);
        let distFromPlayerToFruit =
          sqrt(vecToFruitX *. vecToFruitX +. vecToFruitY *. vecToFruitY);
        let cosangle =
          vecToFruitX
          /. distFromPlayerToFruit
          *. dx
          /. mag
          +. vecToFruitY
          /. distFromPlayerToFruit
          *. dy
          /. mag;
        if (cosangle > closestDot) {
          (vecToFruitX, vecToFruitY, cosangle);
        } else {
          (closestX, closestY, closestDot);
        };
      },
      (dx, dy, 0.98),
      getVisibleEnemies(state, env),
    );

  let distFromPlayerToFruit = sqrt(newdx *. newdx +. newdy *. newdy);
  if (distFromPlayerToFruit > minFruitDistanceForAimAssist) {
    (newdx, newdy, cosangle);
  } else {
    (dx, dy, 0.98);
  };
};

let easeInOutCubic = t =>
  if (t < 0.5) {
    4. *. t *. t *. t;
  } else {
    (t -. 1.) *. (2. *. t -. 2.) *. (2. *. t -. 2.) +. 1.;
  };

let (+/) = Filename.concat;

let gridMap = {|
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
x000000000x001000000x000100000x000000000x00000000x
x000000100xxxx000000x000000000x000000000x00000000x
x0000000000000000000x000100000x000000000x00000000x
xxxxxxxx000000000000x000000000x000000000x00000000x
x001000000xxxx000000x000000000x000000000x00000000x
x000000010x000100000x001000000x000000000x00000000x
x000000000x000000000x000000000x000000000x00000000x
x001000000x0000000000000000000x000000000x00000000x
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
|};

let gridWidth = 50;
let gridHeight = 10;
let parseMap = map => {
  let grid =
    Array.make_matrix(
      gridWidth,
      gridHeight,
      {collision: false, kind: Floor},
    );
  /*for (i in 0 to (gridHeight - 1)) {
      grid[0][i] = {collision: true, kind: Wall};
    };
    for (i in 0 to (gridWidth - 1)) {
      grid[i][0] = {collision: true, kind: Wall};
    };
    for (i in 0 to (gridHeight - 1)) {
      grid[gridWidth - 1][i] = {collision: true, kind: Wall};
    };
    for (i in 0 to (gridWidth - 1)) {
      grid[i][gridHeight - 1] = {collision: true, kind: Wall};
    };*/

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
        | '0' => {collision: false, kind: Floor}
        | '1' =>
          enemies :=
            [
              {
                pos: {
                  x: float_of_int(i^) *. tileSizef,
                  y: float_of_int(j^) *. tileSizef,
                },
                speed: 50.,
                error: {
                  x: 0.,
                  y: 0.,
                },
                timeUntilNextAttack: 0.,
                path: [],
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

let setup = (size, assetDir, env) => {
  switch (size) {
  | `InitialSize => ()
  | `FullScreen =>
    Env.size(
      ~width=Env.displayWidth(env),
      ~height=Env.displayHeight(env),
      env,
    )
  | `Normal => Env.size(~width=375, ~height=667, env)
  };
  Random.init(0);

  let (grid, enemies) = parseMap(gridMap);

  {
    x: 5. *. tileSizef,
    y: 2. *. tileSizef,
    time: 0.,
    aim: Nothing,
    health: 100,
    enemies,
    bg: Draw.loadImage(~filename=assetDir +/ "background.png", env),
    cachedBackground:
      Draw.createImage(~width=100 * tileSize, ~height=100 * tileSize, env),
    assetMap: loadAssetMap(env, possibleFruits),
    bullets: [],
    splashes: [],
    experiment: 0,
    prevMouseState: false,
    sounds: {
      enemyDeathSound: Env.loadSound(assetDir +/ "enemyDeathSound.wav", env),
    },
    currentWeaponIndex: 0,
    deathTime: 0.,
    weapons: [|
      {
        length: 5,
        moveTime: 0.3,
        playerTravelDistance: 40.,
        bulletSpeed: 600.,
        kind: Pistol,
      },
      {
        length: 10,
        moveTime: 0.6,
        playerTravelDistance: 40.,
        bulletSpeed: 400.,
        kind: Shotgun,
      },
      {
        length: 5,
        moveTime: 0.3,
        playerTravelDistance: 40.,
        bulletSpeed: 200.,
        kind: ShootsBehindYou,
      },
    |],
    grid,
  };
};

let draw = (state, env) => {
  let realdt = Env.deltaTime(env);

  let dt =
    switch (state.aim) {
    | Moving(_, _, mt, maxTime) =>
      if (mt < 0.01) {
        realdt
        /. Utils.lerpf(
             ~value=
               Utils.norm(~value=mt, ~low=0., ~high=0.05),
             ~low=slowMoveDivisor,
             ~high=1.,
           );
      } else if (mt >= 0.01 && mt < maxTime -. 0.05) {
        realdt;
      } else {
        realdt
        /. Utils.lerpf(
             ~value=
               easeInOutCubic(Utils.norm(~value=mt, ~low=maxTime -. 0.05, ~high=maxTime)),
             ~low=1.,
             ~high=slowMoveDivisor,
           );
      }
    | _ => realdt /. slowMoveDivisor
    };

  let windowW = Env.width(env);
  let windowWf = float_of_int(windowW);
  let windowH = Env.height(env);
  let windowHf = float_of_int(windowH);

  let halfWindowW = windowW / 2;
  let halfWindowWf = float_of_int(halfWindowW);
  let halfWindowH = windowH / 2;
  let halfWindowHf = float_of_int(halfWindowH);
  let (playerXScreen, playerYScreen) = (halfWindowW, halfWindowH);
  let (playerXScreenf, playerYScreenf) = (
    float_of_int(playerXScreen),
    float_of_int(playerYScreen),
  );
  let state = {
    ...state,
    time: state.time +. realdt,
    deathTime: max(0., state.deathTime -. realdt),
  };

  let pathfinder = Pathfinder.make(state.grid);

  /* ======= EVENTS + UPDATES ======= */
  let (mx, my, mx2, my2, numOfTouches) =
    switch (Env.changedTouches(env)) {
    | [] =>
      let (mx, my) = Env.mouse(env);
      (float_of_int(mx), float_of_int(my), 0., 0., 1);
    | [{x, y}] => (x, y, 0., 0., 1)
    | [{x, y}, {x: x2, y: y2}, ...rest] => (
        x,
        y,
        x2,
        y2,
        2 + List.length(rest),
      )
    };

  let visibleEnemies = getVisibleEnemies(state, env);

  /* Bullet collision detection and response */
  let state =
    List.fold_left(
      (state, {x, y, vx, vy} as bullet: bulletT) =>
        if (bulletIsOutOfRange(state, bullet, env)) {
          state;
        } else {
          switch (bulletCollidesWithEnemies(bullet, state.enemies)) {
          | None => {
              ...state,
              bullets: [
                {...bullet, x: x +. vx *. dt, y: y +. vy *. dt},
                ...state.bullets,
              ],
            }
          | Some(index) =>
            Env.playSound(
              state.sounds.enemyDeathSound,
              ~volume=1.0,
              ~loop=false,
              env,
            );
            switch (splitListAt(state.enemies, index)) {
            | (headList, [{pos: {x: px, y: py}}, ...restOfTail]) => {
                ...state,
                enemies: headList @ restOfTail,
                splashes: [
                  {
                    x: px,
                    y: py,
                    width: Utils.randomf(~min=40., ~max=64.),
                    height: Utils.randomf(~min=40., ~max=64.),
                    rotation: Utils.randomf(~min=0., ~max=Constants.pi),
                  },
                  ...state.splashes,
                ],
              }
            | _ => failwith("splitListAt returned something strange")
            };
          };
        },
      {...state, bullets: []},
      state.bullets,
    );
  let (playerCellX, playerCellY) = (
    int_of_float(floor(state.x /. tileSizef)),
    int_of_float(floor(state.y /. tileSizef)),
  );
  let state = {
    ...state,
    enemies:
      List.map(
        ({pos: {x, y}, speed, error, timeUntilNextAttack} as enemy) => {
          let (cellX, cellY) = (
            int_of_float(floor(x /. tileSizef)),
            int_of_float(floor(y /. tileSizef)),
          );
          if (cellX >= 0
              && cellX < gridWidth
              && cellY >= 0
              && cellY < gridHeight) {
            let cell = state.grid[cellX][cellY];
            if (!cell.collision) {
              let path =
                Pathfinder.pathfind(
                  pathfinder,
                  (playerCellX, playerCellY),
                  (cellX, cellY),
                );
              switch (path) {
              | [] => enemy
              | [_]
              | [_, _] =>
                let (dx, dy) = (state.x -. x, state.y -. y);
                let mag = sqrt(dx *. dx +. dy *. dy);
                {
                  ...enemy,
                  pos: {
                    x: x +. dx /. mag *. speed *. dt,
                    y: y +. dy /. mag *. speed *. dt,
                  },
                  timeUntilNextAttack: max(0., timeUntilNextAttack -. dt),
                };
              | [_, next, nextnext, ...rest] =>
                let cellToWorld = ((x, y)) => (
                  float_of_int(x) *. tileSizef,
                  float_of_int(y) *. tileSizef,
                );
                let (nx1, ny1) = cellToWorld(next);
                let (dx1, dy1) = (nx1 -. x, ny1 -. y);
                let nextCellMag = sqrt(dx1 *. dx1 +. dy1 *. dy1);

                let (currentCellx, currentCelly) =
                  cellToWorld((cellX, cellY));
                let (currentCelldx, currentCelldy) = (
                  currentCellx -. x,
                  currentCelly -. y,
                );
                let currentCellMag =
                  sqrt(
                    currentCelldx
                    *. currentCelldx
                    +. currentCelldy
                    *. currentCelldy,
                  );

                let ratio = currentCellMag /. (currentCellMag +. nextCellMag);

                let (nx2, ny2) = cellToWorld(nextnext);
                let (dx2, dy2) = (nx2 -. nx1, ny2 -. ny1);
                let (destX, destY) = (
                  nx1 +. dx2 *. ratio,
                  ny1 +. dy2 *. ratio,
                );
                let (destRelX, destRelY) = (destX -. x, destY -. y);
                let mag = sqrt(destRelX *. destRelX +. destRelY *. destRelY);
                let (a, b) = (nx1 -. x, ny1 -. y);
                let c = sqrt(a *. a +. b *. b);
                {
                  ...enemy,
                  pos: {
                    x: x +. destRelX /. mag *. speed *. dt,
                    y: y +. destRelY /. mag *. speed *. dt,
                  },
                  timeUntilNextAttack: max(0., timeUntilNextAttack -. dt),
                };
              };
            } else {
              enemy;
            };
          } else {
            enemy;
          };
        },
        state.enemies,
      ),
  };

  let state =
    List.fold_left(
      ({health} as innerState, {pos: {x, y}, timeUntilNextAttack} as enemy) =>
        if (timeUntilNextAttack <= 0.) {
          let dx = state.x -. x;
          let dy = state.y -. y;
          let mag = sqrt(dx *. dx +. dy *. dy);
          if (mag < enemyAttackDistance) {
            {
              ...innerState,
              health: health - 10,
              enemies:
                List.map(
                  e => e == enemy ? {...e, timeUntilNextAttack: 1.} : e,
                  state.enemies,
                ),
            };
          } else {
            innerState;
          };
        } else {
          innerState;
        },
      state,
      state.enemies,
    );

  let (devButtonX, devButtonY) = (60., 60.);
  let (swapWeaponsButtonWidth, swapWeaponsButtonHeight) = (64., 64.);
  let (swapWeaponsButtonX, swapWeaponsButtonY) = (
    float_of_int(playerXScreen - halfPlayerSize),
    float_of_int(playerYScreen - halfPlayerSize),
  );
  let state =
    if (numOfTouches > 1) {
      state;
    } else {
      state;
    };
  let (state, didTapOnSwapButton) =
    switch (state.aim, Env.mousePressed(env)) {
    | (Aiming({x: sx, y: sy, points, startAimTime}), false)
        when state.prevMouseState =>
      /* Self induced difficulty here. I wanted to allow swipes on the button, just in case you'd  accidentally swipe on the button. So on top of checking if you released your finger on the button we also check if you were aiming and how far you moved when aiming. If the max displacement from the start point is beyond a certain threshold, we don't consider this a tap. */
      let maxDeltaFromStartPoint =
        List.fold_left(
          (maxDelta, (x, y)) => {
            let dx = x -. sx;
            let dy = y -. sy;
            let mag = sqrt(dx *. dx +. dy *. dy);
            if (mag > maxDelta) {
              mag;
            } else {
              maxDelta;
            };
          },
          0.,
          points,
        );
      if (state.prevMouseState
          && mx > swapWeaponsButtonX
          && mx < swapWeaponsButtonX
          +. swapWeaponsButtonWidth
          && my > swapWeaponsButtonY
          && my < swapWeaponsButtonY
          +. swapWeaponsButtonHeight
          && state.time
          -. startAimTime < 0.2
          && maxDeltaFromStartPoint < 10.) {
        (
          {
            ...state,
            currentWeaponIndex:
              (state.currentWeaponIndex + 1) mod Array.length(state.weapons),
          },
          true,
        );
      } else {
        (state, false);
      };
    | (_, false) when state.prevMouseState =>
      if (mx > swapWeaponsButtonX
          && mx < swapWeaponsButtonX
          +. swapWeaponsButtonWidth
          && my > swapWeaponsButtonY
          && my < swapWeaponsButtonY
          +. swapWeaponsButtonHeight) {
        (
          /* There's a bug where if you start a gesture while Moving, this will interpret a quick
             swipe as a tap because we don't allow for interruptible animations right now. */
          {
            ...state,
            currentWeaponIndex:
              (state.currentWeaponIndex + 1) mod Array.length(state.weapons),
          },
          true,
        );
      } else {
        (state, false);
      }
    | _ => (state, false)
    };

  let (aimStartXf, aimStartYf) = (windowWf -. 120., windowHf -. 120.);
  let state =
    if (state.prevMouseState
        && !Env.mousePressed(env)
        && mx < devButtonX
        && my < devButtonY) {
      {
        ...state,
        experiment: (state.experiment + 1) mod (_NUMBER_OF_EXPERIMENTS + 1),
      };
    } else {
      switch (state.aim, Env.mousePressed(env)) {
      | (Aiming({x: sx, y: sy, points} as aiming), true) =>
        if (state.experiment == _JOYSTICK) {
          let dx = aimStartXf -. mx;
          let dy = aimStartYf -. my;
          let mag = sqrt(dx *. dx +. dy *. dy);
          let (x, y) =
            if (mag > 30.) {
              (state.x -. dx *. realdt, state.y -. dy *. realdt);
            } else {
              (state.x, state.y);
            };
          {
            ...state,
            x,
            y,
            aim:
              Aiming({
                ...aiming,
                x: sx,
                y: sy,
                points: [(mx, my), ...points],
              }),
          };
        } else {
          let dx = sx -. mx;
          let dy = sy -. my;
          let aimCap = 50.;
          let mag = sqrt(dx *. dx +. dy *. dy);
          let (sx, sy) =
            if (mag > aimCap) {
              (mx +. dx /. mag *. 50., my +. dy /. mag *. 50.);
            } else {
              (sx, sy);
            };
          {
            ...state,
            aim:
              Aiming({
                ...aiming,
                x: sx,
                y: sy,
                points: [(mx, my), ...points],
              }),
          };
        }
      | (Nothing, false) => state
      | (Nothing, true) => {
          ...state,
          aim: Aiming({x: mx, y: my, points: [], startAimTime: state.time}),
        }
      | (Aiming({x: sx, y: sy, points, startAimTime}), false) =>
        if (!didTapOnSwapButton) {
          let currentWeapon = state.weapons[state.currentWeaponIndex];
          let (sx, sy) =
            if (state.experiment == _JOYSTICK) {
              (aimStartXf, aimStartYf);
            } else {
              (sx, sy);
            };
          let (dx, dy) =
            if (currentWeapon.kind == ShootsBehindYou) {
              (mx -. sx, my -. sy);
            } else {
              (sx -. mx, sy -. my);
            };

          let mag = sqrt(dx *. dx +. dy *. dy);

          let isGestureAFlick = List.length(points) < 10;
          let (dx, dy) =
            if (isGestureAFlick) {
              let numberOfPointsToAverage = 3;
              let firstCoupleOfPoints =
                if (List.length(points) >= numberOfPointsToAverage) {
                  let (firstCoupleOfPoints, _) =
                    splitListAt(points, numberOfPointsToAverage);
                  firstCoupleOfPoints;
                } else {
                  points;
                };

              let (dx, dy) =
                List.fold_left(
                  ((dx, dy), (x, y)) => (
                    dx
                    +. (
                      currentWeapon.kind == ShootsBehindYou ? x -. sx : sx -. x
                    ),
                    dy
                    +. (
                      currentWeapon.kind == ShootsBehindYou ? y -. sy : sy -. y
                    ),
                  ),
                  (dx, dy),
                  firstCoupleOfPoints,
                );

              (
                dx /. float_of_int(numberOfPointsToAverage),
                dy /. float_of_int(numberOfPointsToAverage),
              );
            } else {
              (dx, dy);
            };

          let shouldAssistAim =
            state.time -. startAimTime < autoaimDisengageTime && currentWeapon.kind != Shotgun;
          let (aimAssistdx, aimAssistdy) =
            if (shouldAssistAim) {
              let (dx, dy, _) = aimAssist(state, dx, dy, mag, env);
              (dx, dy);
            } else {
              (dx, dy);
            };
          if (mag > 20.) {
            let mag = sqrt(dx *. dx +. dy *. dy);
            let aimAssistmag =
              sqrt(aimAssistdx *. aimAssistdx +. aimAssistdy *. aimAssistdy);
            let bulletSpeed = currentWeapon.bulletSpeed;
            /* The movement goes in the other direction when the player's holding the
               ShootsBehindYou gun. Everything other than the movement (the aim and bullet) goes in the normal direction. */
            let (destX, destY) =
              if (currentWeapon.kind == ShootsBehindYou) {
                (
                  state.x +. dx /. mag *. currentWeapon.playerTravelDistance,
                  state.y +. dy /. mag *. currentWeapon.playerTravelDistance,
                );
              } else {
                (
                  state.x -. dx /. mag *. currentWeapon.playerTravelDistance,
                  state.y -. dy /. mag *. currentWeapon.playerTravelDistance,
                );
              };
            let (cellX, cellY) = (
              int_of_float(floor(destX /. tileSizef)),
              int_of_float(floor(destY /. tileSizef)),
            );
            let cell = state.grid[cellX][cellY];
            let aim =
              if (cell.collision) {
                Nothing;
              } else if (state.experiment == _JOYSTICK) {
                Nothing;
              } else {
                Moving(
                  (state.x, state.y),
                  (destX, destY),
                  0.,
                  currentWeapon.moveTime,
                );
              };
            let (dirX, dirY) = (
              aimAssistdx /. aimAssistmag,
              aimAssistdy /. aimAssistmag,
            );
            let bullets =
              if (currentWeapon.kind == Shotgun) {
                [
                  {
                    x: state.x -. dirX *. halfPlayerSizef,
                    y: state.y -. dirY *. halfPlayerSizef,
                    vx: -. dirX *. bulletSpeed,
                    vy: -. dirY *. bulletSpeed,
                  },
                  {
                    x: state.x -. dirX *. halfPlayerSizef,
                    y: state.y -. dirY *. halfPlayerSizef,
                    vx:
                      -. (dirX +. Utils.randomf(~min=-0.3, ~max=0.3))
                      *. bulletSpeed,
                    vy:
                      -. (dirY +. Utils.randomf(~min=-0.3, ~max=0.3))
                      *. bulletSpeed,
                  },
                  {
                    x: state.x -. dirX *. halfPlayerSizef,
                    y: state.y -. dirY *. halfPlayerSizef,
                    vx:
                      -. (dirX +. Utils.randomf(~min=-0.3, ~max=0.3))
                      *. bulletSpeed,
                    vy:
                      -. (dirY +. Utils.randomf(~min=-0.3, ~max=0.3))
                      *. bulletSpeed,
                  },
                  {
                    x: state.x -. dirX *. halfPlayerSizef,
                    y: state.y -. dirY *. halfPlayerSizef,
                    vx:
                      -. (dirX +. Utils.randomf(~min=-0.3, ~max=0.3))
                      *. bulletSpeed,
                    vy:
                      -. (dirY +. Utils.randomf(~min=-0.3, ~max=0.3))
                      *. bulletSpeed,
                  },
                  ...state.bullets,
                ];
              } else {
                [
                  {
                    x: state.x -. dirX *. halfPlayerSizef,
                    y: state.y -. dirY *. halfPlayerSizef,
                    vx: -. dirX *. bulletSpeed,
                    vy: -. dirY *. bulletSpeed,
                  },
                  ...state.bullets,
                ];
              };
            {...state, aim, bullets};
          } else {
            {...state, aim: Nothing};
          };
        } else {
          state;
        }

      | (Moving(_prev, (destX, destY), time, maxTime), _)
          when time >= maxTime => {
          ...state,
          aim: Nothing,
          x: destX,
          y: destY,
        }
      | (
          Moving(
            (startX, startY) as start,
            (destX, destY) as dest,
            time,
            maxTime,
          ),
          _,
        ) =>
        let lerpAmt =
          Utils.constrain(~amt=time /. maxTime, ~low=0., ~high=1.);
        {
          ...state,
          aim: Moving(start, dest, time +. dt, maxTime),
          x: Utils.lerpf(~low=startX, ~high=destX, ~value=lerpAmt),
          y: Utils.lerpf(~low=startY, ~high=destY, ~value=lerpAmt),
        };
      };
    };

  /* ======= DRAWING ======= */
  Draw.tint(
    Utils.color(255, state.health + 155, state.health + 155, 255),
    env,
  );
  if (state.experiment == _DEBUG) {
    Draw.background(Utils.color(~r=255, ~g=220, ~b=200, ~a=255), env);
  } else if (state.currentWeaponIndex == 1) {
    Draw.background(Utils.color(~r=120, ~g=160, ~b=240, ~a=255), env);
  } else if (state.currentWeaponIndex == 2) {
    Draw.background(Utils.color(~r=240, ~g=160, ~b=120, ~a=255), env);
  } else {
    Draw.background(Constants.white, env);
  };

  Draw.pushMatrix(env);
  Draw.translate(
    ~x=playerXScreenf -. state.x,
    ~y=playerYScreenf -. state.y,
    env,
  );

  /*if (!Draw.isImageDrawnTo(state.cachedBackground)) {
    Draw.withImage(state.cachedBackground, env, env =>*/
  Array.iteri(
    (x, column) =>
      Array.iteri(
        (y, {kind}) => {
          let (x, y) = (
            float_of_int(x) *. tileSizef -. tileSizef /. 2.,
            float_of_int(y) *. tileSizef -. tileSizef /. 2.,
          );
          let paddingForSomeReason = 0.;
          let left = state.x -. halfWindowWf -. paddingForSomeReason;
          let right = state.x +. halfWindowWf +. paddingForSomeReason;
          let top = state.y -. halfWindowHf -. paddingForSomeReason;
          let bottom = state.y +. halfWindowHf +. paddingForSomeReason;
          if (x
              +. tileSizef > left
              && x < right
              && y
              +. tileSizef > top
              && y < bottom) {
            switch (kind) {
            | Floor =>
              ();
              Draw.fill(Constants.white, env);
              Draw.stroke(Constants.black, env);
              Draw.strokeWeight(1, env);
              Draw.rectf(
                ~pos=(x, y),
                ~width=tileSizef,
                ~height=tileSizef,
                env,
              );
            | Wall =>
              Draw.fill(Utils.color(124, 10, 2, 255), env);
              Draw.stroke(Constants.black, env);
              Draw.strokeWeight(1, env);
              Draw.rectf(
                ~pos=(x, y),
                ~width=tileSizef,
                ~height=tileSizef,
                env,
              );
            /*drawWithRotation(
                AssetMap.find("coconut", state.assetMap),
                ~pos=(x, y),
                ~width=54.,
                ~height=74.,
                ~rot=0.,
                env,
              )*/
            };
          };
        },
        column,
      ),
    state.grid,
  );
  /*);*/
  /*} else {
        /* Draw at that position for padding */
        Draw.image(
          state.cachedBackground,
          ~pos=(- backgroundImagePaddingX, - backgroundImagePaddingY),
          env,
        );
      };
    */

  if (state.deathTime > 0.) {
    Draw.text(
      ~body="REVIVED!",
      ~pos=(
        5 * tileSize - Draw.textWidth(~body="REVIVED!", env) / 2,
        (-200),
      ),
      env,
    );
    Draw.text(
      ~body="go fight",
      ~pos=(
        5 * tileSize - Draw.textWidth(~body="go fight", env) / 2 - 2,
        (-150),
      ),
      env,
    );
  };
  List.iter(
    ({x, y, width, height, rotation}) =>
      drawWithRotation(
        AssetMap.find("splash_red", state.assetMap),
        ~pos=(x, y),
        ~width,
        ~height,
        ~rot=rotation,
        env,
      ),
    state.splashes,
  );

  List.iter(
    ({pos: {x, y}, speed}) => {
      drawWithRotation(
        AssetMap.find("robot1", state.assetMap),
        ~pos=(x, y),
        ~width=224. /. 5.,
        ~height=344. /. 5.,
        ~rot=0.,
        env,
      );
      if (state.experiment == _DEBUG) {
        let (cellX, cellY) = (
          int_of_float(floor(x /. tileSizef)),
          int_of_float(floor(y /. tileSizef)),
        );
        if (cellX >= 0 && cellX < gridWidth && cellY >= 0 && cellY < gridHeight) {
          let cell = state.grid[cellX][cellY];
          if (!cell.collision) {
            let path =
              Pathfinder.pathfind(
                pathfinder,
                (playerCellX, playerCellY),
                (cellX, cellY),
              );
            switch (path) {
            | [] => ()
            | [current] => ()
            | [_, _] => ()
            | [_, next, nextnext, ...rest] =>
              let cellToWorld = ((x, y)) => (
                float_of_int(x) *. tileSizef,
                float_of_int(y) *. tileSizef,
              );
              let (nx1, ny1) = cellToWorld(next);
              let (dx1, dy1) = (nx1 -. x, ny1 -. y);
              let nextCellMag = sqrt(dx1 *. dx1 +. dy1 *. dy1);

              let (currentCellx, currentCelly) = cellToWorld((cellX, cellY));
              let (currentCelldx, currentCelldy) = (
                currentCellx -. x,
                currentCelly -. y,
              );
              let currentCellMag =
                sqrt(
                  currentCelldx
                  *. currentCelldx
                  +. currentCelldy
                  *. currentCelldy,
                );

              let ratio = currentCellMag /. (currentCellMag +. nextCellMag);

              let (nx2, ny2) = cellToWorld(nextnext);
              let (dx2, dy2) = (nx2 -. nx1, ny2 -. ny1);
              let (destX, destY) = (
                nx1 +. dx2 *. ratio,
                ny1 +. dy2 *. ratio,
              );
              let (destRelX, destRelY) = (destX -. x, destY -. y);
              let mag = sqrt(destRelX *. destRelX +. destRelY *. destRelY);
              Draw.stroke(Utils.color(0, 0, 255, 100), env);
              Draw.strokeWeight(3, env);
              Draw.linef(
                ~p1=(x, y),
                ~p2=(
                  x +. destRelX /. mag *. speed *. dt,
                  y +. destRelY /. mag *. speed *. dt,
                ),
                env,
              );
            };

            let (prevX, prevY) = (ref(x), ref(y));
            List.iter(
              ((x, y)) => {
                Draw.stroke(Utils.color(255, 0, 0, 100), env);
                Draw.strokeWeight(3, env);
                let (x, y) = (
                  float_of_int(x) *. tileSizef,
                  float_of_int(y) *. tileSizef,
                );
                Draw.linef(~p1=(prevX^, prevY^), ~p2=(x, y), env);
                prevX := x;
                prevY := y;
              },
              path,
            );
          };
        };
      };
    },
    state.enemies,
  );

  /*  let enemy1 = List.hd(state.enemies);

      let (cellX, cellY) = (
        int_of_float(floor((enemy1.pos.x -. tileSizef /. 2. -. halfPlayerSizef) /. tileSizef)),
        int_of_float(floor((enemy1.pos.y -. tileSizef /. 2. -. halfPlayerSizef) /. tileSizef)),
      );
      if (cellX >= 0 && cellX < gridWidth && cellY >= 0 && cellY < gridHeight) {
        let cell = state.grid[cellX][cellY];
        if (!cell.collision) {
          let l = Pathfinder.pathfind(pathfinder, (playerCellX, playerCellY), (cellX, cellY));

          let (prevX, prevY) = (ref(enemy1.pos.x), ref(enemy1.pos.y));
          List.iter(((x, y)) => {
            Draw.stroke(Utils.color(255, 0, 0, 100), env);
            Draw.strokeWeight(3, env);
            let (x, y) = (
                    backgroundImagePaddingXf +. float_of_int(x) *. tileSizef,
                    backgroundImagePaddingYf +. float_of_int(y) *. tileSizef,
                  );
            Draw.linef(~p1=(prevX^, prevY^), ~p2=(x, y), env);
            prevX := x;
            prevY := y;
          }, l);
        }
      };*/

  /*let enemy2 = List.hd(List.tl(state.enemies));

    let (cellX, cellY) = (
      int_of_float(floor((enemy2.pos.x -. tileSizef /. 2. -. halfPlayerSizef) /. tileSizef)),
      int_of_float(floor((enemy2.pos.y -. tileSizef /. 2. -. halfPlayerSizef) /. tileSizef)),
    );
    if (cellX >= 0 && cellX < gridWidth && cellY >= 0 && cellY < gridHeight) {
      let cell = state.grid[cellX][cellY];
      if (!cell.collision) {
        let l = Pathfinder.pathfind(pathfinder, (playerCellX, playerCellY), (cellX, cellY));

        let (prevX, prevY) = (ref(enemy2.pos.x), ref(enemy2.pos.y));
        List.iter(((x, y)) => {
          Draw.stroke(Utils.color(255, 0, 0, 100), env);
          Draw.strokeWeight(3, env);
          let (x, y) = (
                  backgroundImagePaddingXf +. float_of_int(x) *. tileSizef,
                  backgroundImagePaddingYf +. float_of_int(y) *. tileSizef,
                );
          Draw.linef(~p1=(prevX^, prevY^), ~p2=(x, y), env);
          prevX := x;
          prevY := y;
        }, l);
      }
    };*/

  if (state.experiment == _DEBUG) {
    /* Draw pineapple dots */
    Draw.pushStyle(env);
    List.iter(
      ({pos: {x, y}}) => {
        let dx = state.x -. x;
        let dy = state.y -. y;
        let mag = sqrt(dx *. dx +. dy *. dy);
        if (mag < enemyAttackDistance) {
          Draw.strokeWeight(2, env);
          Draw.stroke(Constants.green, env);
        } else {
          Draw.strokeWeight(1, env);
          Draw.stroke(Constants.red, env);
        };
        Draw.ellipsef(~center=(x, y), ~radx=4., ~rady=4., env);
      },
      getVisibleEnemies(state, env),
    );
    Draw.popStyle(env);
  };
  /* Draw the bullets.
      Uses some super hacky angle calculation do draw bullets as ellipses pointing in the right
      direction.
     */
  List.iter(
    ({x, y, vx, vy}: bulletT) => {
      Draw.pushMatrix(env);
      Draw.translate(~x=x -. 6., ~y=y -. 1., env);
      Draw.rotate(
        vy > 0. ?
          acos(vx /. sqrt(vx *. vx +. vy *. vy)) :
          Constants.pi /. 2. +. asin(vx /. sqrt(vx *. vx +. vy *. vy)),
        env,
      );
      Draw.stroke(Constants.black, env);
      Draw.ellipsef(~center=(0., 0.), ~radx=12., ~rady=2., env);
      Draw.popMatrix(env);
    },
    state.bullets,
  );

  Draw.popMatrix(env);

  /* Draw the aim line */
  switch (state.aim, Env.mousePressed(env)) {
  | (Aiming({x: sx, y: sy, points, startAimTime}), true) =>
    let currentWeapon = state.weapons[state.currentWeaponIndex];
    let (sx, sy) =
      if (state.experiment == _JOYSTICK) {
        (aimStartXf, aimStartYf);
      } else {
        (sx, sy);
      };
    let (dx, dy) =
      if (currentWeapon.kind == ShootsBehindYou) {
        (mx -. sx, my -. sy);
      } else {
        (sx -. mx, sy -. my);
      };
    let mag = sqrt(dx *. dx +. dy *. dy);

    let currentWeapon = state.weapons[state.currentWeaponIndex];

    if (mag > 20.) {
      let moveX = dx /. mag *. currentWeapon.playerTravelDistance;
      let moveY = dy /. mag *. currentWeapon.playerTravelDistance;
      Draw.pushStyle(env);
      Draw.strokeWeight(2, env);
      Draw.stroke(Constants.black, env);
      Draw.linef(
        ~p1=(playerXScreenf, playerYScreenf),
        ~p2=(playerXScreenf -. moveX, playerYScreenf -. moveY),
        env,
      );

      if (currentWeapon.kind == ShootsBehindYou) {
        let (dx, dy) = (sx -. mx, sy -. my);
        let mag = sqrt(dx *. dx +. dy *. dy);

        let moveX = dx /. mag *. currentWeapon.playerTravelDistance;
        let moveY = dy /. mag *. currentWeapon.playerTravelDistance;
        Draw.strokeWeight(4, env);
        Draw.stroke(Utils.color(100, 100, 200, 200), env);
        Draw.linef(
          ~p1=(playerXScreenf, playerYScreenf),
          ~p2=(playerXScreenf -. moveX, playerYScreenf -. moveY),
          env,
        );
      };

      Draw.popStyle(env);

      if (state.experiment == _DEBUG) {
        Draw.pushStyle(env);
        Draw.strokeWeight(1, env);
        Draw.stroke(Constants.red, env);
        Draw.linef(~p1=(sx, sy), ~p2=(mx, my), env);
        Draw.linef(
          ~p1=(playerXScreenf, playerYScreenf),
          ~p2=(
            playerXScreenf -. moveX *. 100.,
            playerYScreenf -. moveY *. 100.,
          ),
          env,
        );

        Draw.ellipsef(
          ~center=(windowWf -. 120., windowHf -. 120.),
          ~radx=60.,
          ~rady=60.,
          env,
        );

        let shouldAssistAim =
          state.time -. startAimTime < autoaimDisengageTime;
        if (shouldAssistAim) {
          let (dx, dy, dot) = aimAssist(state, dx, dy, mag, env);
          if (dot > 0.98) {
            let mag = sqrt(dx *. dx +. dy *. dy);
            let moveX = dx /. mag *. moveSpeed;
            let moveY = dy /. mag *. moveSpeed;
            Draw.stroke(Constants.blue, env);
            Draw.linef(
              ~p1=(playerXScreenf, playerYScreenf),
              ~p2=(
                playerXScreenf -. moveX *. 100.,
                playerYScreenf -. moveY *. 100.,
              ),
              env,
            );
          };
        };
        Draw.ellipsef(
          ~center=(playerXScreenf, playerYScreenf),
          ~radx=minFruitDistanceForAimAssist,
          ~rady=minFruitDistanceForAimAssist,
          env,
        );
        Draw.popStyle(env);
      };
    };
  | _ => ()
  };

  Draw.strokeWeight(1, env);
  Draw.fill(Utils.color(124, 10, 2, 255), env);
  Draw.rectf(~pos=(22., 42.), ~width=104., ~height=28., env);
  Draw.fill(Utils.color(30, 100, 30, 255), env);
  Draw.rectf(
    ~pos=(24., 44.),
    ~width=float_of_int(state.health),
    ~height=24.,
    env,
  );

  if (state.experiment == _JOYSTICK) {
    Draw.pushStyle(env);
    Draw.stroke(Constants.red, env);
    Draw.strokeWeight(1, env);
    Draw.ellipsef(
      ~center=(windowWf -. 120., windowHf -. 120.),
      ~radx=60.,
      ~rady=60.,
      env,
    );
    Draw.popStyle(env);
  };

  /* Draw the player */
  drawWithRotation(
    AssetMap.find("player", state.assetMap),
    ~pos=(playerXScreenf, playerYScreenf),
    ~width=220./.6.,
    ~height=311./.6.,
    ~rot=0.,
    env,
  );

  if (state.health <= 0) {
    let (grid, enemies) = parseMap(gridMap);

    {
      ...state,
      deathTime: 3.,
      x: 5. *. tileSizef,
      y: 2. *. tileSizef,
      time: 0.,
      aim: Nothing,
      health: 100,
      enemies,
      assetMap: loadAssetMap(env, possibleFruits),
      bullets: [],
      splashes: [],
      experiment: 0,
      prevMouseState: false,
      currentWeaponIndex: 0,
      grid,
    };
  } else {
    {...state, prevMouseState: Env.mousePressed(env)};
  };
};

let run = (size, assetDir) =>
  Reprocessing.run(~setup=setup(size, assetDir), ~draw, ());
