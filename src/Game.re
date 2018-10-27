let _DEBUG = 1;

let _NUMBER_OF_EXPERIMENTS = 2;

let _INVERTED_SHOOTING = 2;

open Reprocessing;
open Common;

let drawWithRotation =
    (img, ~pos as (x, y), ~height, ~width, ~rot, ~scale=1.0, env) => {
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.scale(globalScale, globalScale, env);
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
  let rec loop = enemiesRemaining =>
    switch (enemiesRemaining) {
    | [] => None
    | [{id, isDead, pos: {x: sx, y: sy}}, ...restOfPineapples] =>
      let enemyBoundingCircle = 30.;
      if (!isDead
          && Utils.distf(~p1=(sx, sy), ~p2=(x, y)) < enemyBoundingCircle) {
        Some(id);
      } else {
        loop(restOfPineapples);
      };
    };
  loop(enemies);
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
  let padding = 40.;
  let w = float_of_int(Env.width(env));
  let h = float_of_int(Env.height(env));
  List.filter(
    ({pos: {x, y}}: enemyT) =>
      abs_float(state.x -. x) < w
      /. 2.
      +. padding
      && abs_float(state.y -. y) < h
      /. 2.
      +. padding,
    state.enemies,
  );
};

let bulletIsOutOfRange = (state, bullet: bulletT, env) =>
  bullet.timeRemaining <= 0.;

let isThereAWallBetweenTheEnemyAndThePlayer =
    (state, x, y, mag, playerDirectionX, playerDirectionY) => {
  let step = tileSizef /. 2.;
  let numOfIterations = floor(mag /. step);
  let rec loop = i =>
    if (i >= numOfIterations) {
      false;
    } else {
      let ratio = i /. numOfIterations;
      let (cellX, cellY) = (
        int_of_float(floor((x +. playerDirectionX *. ratio) /. tileSizef)),
        int_of_float(floor((y +. playerDirectionY *. ratio) /. tileSizef)),
      );
      getCell(state.grid, (cellX, cellY)).collision || loop(i +. 1.0);
    };
  loop(0.);
};

let minFruitDistanceForAimAssist = 100.;
let aimAssist = (state, dx, dy, mag, env) => {
  let (newdx, newdy, cosangle) =
    List.fold_left(
      ((closestX, closestY, closestDot), {pos: {x, y}, isDead}) =>
        if (isDead) {
          (closestX, closestY, closestDot);
        } else {
          let (vecToEnemyX, vecToEnemyY) = (state.x -. x, state.y -. y);
          let distFromPlayerToEnemy =
            sqrt(vecToEnemyX *. vecToEnemyX +. vecToEnemyY *. vecToEnemyY);
          let isTheEnemyUnreachable =
            isThereAWallBetweenTheEnemyAndThePlayer(
              state,
              x,
              y,
              distFromPlayerToEnemy,
              vecToEnemyX,
              vecToEnemyY,
            );
          if (isTheEnemyUnreachable) {
            (closestX, closestY, closestDot);
          } else {
            let cosangle =
              vecToEnemyX
              /. distFromPlayerToEnemy
              *. dx
              /. mag
              +. vecToEnemyY
              /. distFromPlayerToEnemy
              *. dy
              /. mag;
            if (cosangle > closestDot) {
              (vecToEnemyX, vecToEnemyY, cosangle);
            } else {
              (closestX, closestY, closestDot);
            };
          };
        },
      (dx, dy, 0.98),
      getVisibleEnemies(state, env),
    );

  let distFromPlayerToEnemy = sqrt(newdx *. newdx +. newdy *. newdy);
  if (distFromPlayerToEnemy > minFruitDistanceForAimAssist) {
    (newdx, newdy, cosangle);
  } else {
    (dx, dy, 0.98);
  };
};

let resolveCollision = (~state, ~dt, ~allowSlide=true, x, y, vx, vy) => {
  let collides = (x, y) => {
    let cell =
      getCell(
        state.grid,
        (
          int_of_float(floor(x /. tileSizef)),
          int_of_float(floor(y /. tileSizef)),
        ),
      );
    cell.collision;
  };

  let (vx, collidesX) =
    if (!collides(x +. vx *. dt, y)) {
      (vx, false);
    } else if (!collides(x +. vx /. 2. *. dt, y)) {
      (vx /. 2., allowSlide ? false : true);
    } else {
      (0., true);
    };

  let (vy, collidesY) =
    if (!collides(x, y +. vy *. dt)) {
      (vy, false);
    } else if (!collides(x, y +. vy /. 2. *. dt)) {
      (vy /. 2., allowSlide ? false : true);
    } else {
      (0., true);
    };

  if (allowSlide
      && collidesX
      && collidesY
      || !allowSlide
      && (collidesX || collidesY)) {
    None;
  } else {
    Some((vx, vy));
  };
};

let easeInOutCubic = t =>
  if (t < 0.5) {
    4. *. t *. t *. t;
  } else {
    (t -. 1.) *. (2. *. t -. 2.) *. (2. *. t -. 2.) +. 1.;
  };

let (+/) = Filename.concat;

let cellToWorld = ((x, y)) => (
  float_of_int(x) *. tileSizef +. tileSizef /. 2.,
  float_of_int(y) *. tileSizef +. tileSizef /. 2.,
);

let slowDownTime = (~state, ~realdt, env) =>
  /*if (state.currentMoveTime < state.totalMoveTime) {*/
  if (state.currentMoveTime < 0.01) {
    if (Env.mousePressed(env)) {
      realdt /. slowMoveDivisor *. 2.;
    } else {
      realdt
      /. Utils.lerpf(
           ~value=
             Utils.norm(~value=state.currentMoveTime, ~low=0., ~high=0.01),
           ~low=slowMoveDivisor,
           ~high=1.,
         );
    };
  } else if (state.currentMoveTime >= 0.01
             && state.currentMoveTime < state.totalMoveTime
             -. 0.02) {
    realdt;
  } else {
    realdt
    /. Utils.lerpf(
         ~value=
           easeInOutCubic(
             Utils.norm(
               ~value=state.currentMoveTime,
               ~low=state.totalMoveTime -. 0.02,
               ~high=state.totalMoveTime,
             ),
           ),
         ~low=1.,
         ~high=slowMoveDivisor,
       );
  };
/*} else {
    realdt /. slowMoveDivisor;
  };*/

let getTouchPositions = env =>
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

let findClosestNonCollidableCell = (grid, x, y, cellX, cellY) => {
  let cell = getCell(grid, (cellX, cellY));
  if (cell.collision) {
    let (cellXf, cellYf) = (float_of_int(cellX), float_of_int(cellY));
    let neighbors = [
      (cellXf +. 1., cellYf),
      (cellXf -. 1., cellYf),
      (cellXf +. 1., cellYf +. 1.),
      (cellXf -. 1., cellYf +. 1.),
      (cellXf +. 1., cellYf -. 1.),
      (cellXf -. 1., cellYf -. 1.),
      (cellXf, cellYf +. 1.),
      (cellXf, cellYf -. 1.),
    ];

    let (closestCellX, closestCellY, _) =
      List.fold_left(
        ((closestCellX, closestCellY, closestDist), (neighborX, neighborY)) => {
          let mag = Utils.distf(~p1=(x, y), ~p2=(neighborX, neighborY));
          if (mag < closestDist
              && !
                   getCell(
                     grid,
                     (int_of_float(neighborX), int_of_float(neighborY)),
                   ).
                     collision) {
            (neighborX, neighborY, mag);
          } else {
            (closestCellX, closestCellY, closestDist);
          };
        },
        (cellXf, cellYf, 9999999.),
        neighbors,
      );
    (int_of_float(closestCellX), int_of_float(closestCellY));
  } else {
    (cellX, cellY);
  };
};

let moveBullets = (state, dt, ~removeIfTooFar=false, env) =>
  List.fold_left(
    (state, {x, y, vx, vy} as bullet: bulletT) =>
      switch (
        bulletCollidesWithEnemies(bullet, state.enemies),
        resolveCollision(~state, ~dt, ~allowSlide=false, x, y, vx, vy),
      ) {
      | (None, Some((vx, vy))) =>
        if (removeIfTooFar && bulletIsOutOfRange(state, bullet, env)) {
          state;
        } else {
          {
            ...state,
            bullets: [
              {
                ...bullet,
                x: x +. vx *. dt,
                y: y +. vy *. dt,
                timeRemaining: max(0., bullet.timeRemaining -. dt),
              },
              ...state.bullets,
            ],
          };
        }
      | (_, None) => state
      | (Some(id), _) =>
        Env.playSound(
          state.sounds.enemyDeathSound,
          ~volume=1.0,
          ~loop=false,
          env,
        );

        let pos = ref(zeroVec);
        let enemies =
          List.map(
            enemy =>
              if (enemy.id == id) {
                pos := enemy.pos;
                {...enemy, isDead: true};
              } else {
                enemy;
              },
            state.enemies,
          );
        {
          ...state,
          enemies,
          score: state.score + 1,
          splashes: [
            {
              x: pos^.x,
              y: pos^.y,
              width: Utils.randomf(~min=40., ~max=64.),
              height: Utils.randomf(~min=40., ~max=64.),
              rotation: Utils.randomf(~min=0., ~max=Constants.pi),
            },
            ...state.splashes,
          ],
        };
      },
    {...state, bullets: []},
    state.bullets,
  );

let pushEnemyIfNecessary =
    (~state, ~enemy, ~dt, ~enemiesInArea, resolvedVx, resolvedVy) =>
  if (enemy.forcefullyMovedTimer <= 0.) {
    List.fold_left(
      ((resolvedVx, resolvedVy, alreadyForcedToMove), otherEnemy) =>
        if (otherEnemy == enemy || otherEnemy.isDead) {
          (resolvedVx, resolvedVy, alreadyForcedToMove);
        } else {
          let (dx, dy) = (
            otherEnemy.pos.x -. (enemy.pos.x +. resolvedVx *. dt),
            otherEnemy.pos.y -. (enemy.pos.y +. resolvedVy *. dt),
          );
          /* @Hack this probably causes enemies to be stuck in walls */
          let mag = sqrt(dx *. dx +. dy *. dy);
          if (mag < 30.) {
            (
              -. dx /. mag *. enemy.speed /. 2.,
              -. dy /. mag *. enemy.speed /. 2.,
              true,
            );
          } else {
            (resolvedVx, resolvedVy, false);
          };
        },
      (resolvedVx, resolvedVy, false),
      enemiesInArea,
    );
  } else {
    let (resolvedVx, resolvedVy) =
      switch (
        resolveCollision(
          ~state,
          ~dt,
          enemy.pos.x,
          enemy.pos.y,
          enemy.direction.x,
          enemy.direction.y,
        )
      ) {
      | None => (0., 0.)
      | Some(direction) => direction
      };
    (resolvedVx, resolvedVy, false);
  };

let moveEnemiesAndAttack = (state, dt, pathfinderInstance, env) => {
  let (playerCellX, playerCellY) = (
    int_of_float(floor(state.x /. tileSizef)),
    int_of_float(floor(state.y /. tileSizef)),
  );
  let enemyGridScale = 3;
  let enemyGridWidth = gridWidth / enemyGridScale;
  let enemyGridHeight = gridHeight / enemyGridScale;
  let enemiesInRegion =
    List.fold_left(
      (acc, {pos: {x, y}, isDead} as enemy) =>
        if (isDead) {
          acc;
        } else {
          let (cellX, cellY) = (
            int_of_float(floor(x /. tileSizef)) / enemyGridScale,
            int_of_float(floor(y /. tileSizef)) / enemyGridScale,
          );

          /* @Hack this doesn't work for enemies too close to the right-most wall. It'll crash with
             index out of bounds. */
          acc[cellX][cellY] = [enemy, ...acc[cellX][cellY]];
          acc;
        },
      Array.make_matrix(enemyGridWidth + 1, enemyGridHeight + 1, []),
      state.enemies,
    );

  let movedEnemies =
    List.mapi(
      (
        i,
        {
          isDead,
          pos: {x, y},
          speed,
          error,
          timeUntilNextAttack,
          forcefullyMovedTimer,
          kind,
        } as enemy,
      ) =>
        if (isDead) {
          enemy;
        } else {
          let defaultEnemyUpdates = {
            ...enemy,
            forcefullyMovedTimer: max(0., forcefullyMovedTimer -. dt),
            timeUntilNextAttack: max(0., timeUntilNextAttack -. dt),
          };
          let (cellX, cellY) = (
            int_of_float(floor(x /. tileSizef)),
            int_of_float(floor(y /. tileSizef)),
          );
          let (cellX, cellY) =
            findClosestNonCollidableCell(state.grid, x, y, cellX, cellY);
          /* This is in "realDt" time, which means it lasts wayyyy longer than 200ms. */
          let forcefullyMovedTimerDefaultValue = 0.2;

          let (playerDirectionX, playerDirectionY) = (
            state.x -. x,
            state.y -. y,
          );
          let mag =
            sqrt(
              playerDirectionX
              *. playerDirectionX
              +. playerDirectionY
              *. playerDirectionY,
            );

          /* Helper for moving towards the player instead of awkwardly pathfinding */
          let moveInPlayerDirection =
              (enemy, mag, playerDirectionX, playerDirectionY) => {
            let vx = playerDirectionX /. mag *. speed;
            let vy = playerDirectionY /. mag *. speed;

            let (resolvedVx, resolvedVy) =
              switch (resolveCollision(~state, ~dt, x, y, vx, vy)) {
              | None => (0., 0.)
              | Some(direction) => direction
              };

            let enemiesInArea = enemiesInRegion[cellX / enemyGridScale][cellY
                                                                    / enemyGridScale];
            let (resolvedVx, resolvedVy, forcedToMove) =
              pushEnemyIfNecessary(
                ~state,
                ~enemy,
                ~dt,
                ~enemiesInArea,
                resolvedVx,
                resolvedVy,
              );

            {
              ...enemy,
              forcefullyMovedTimer:
                forcedToMove ?
                  forcefullyMovedTimerDefaultValue : enemy.forcefullyMovedTimer,
              pos: {
                x: x +. resolvedVx *. dt,
                y: y +. resolvedVy *. dt,
              },
              direction: {
                x: resolvedVx,
                y: resolvedVy,
              },
            };
          };
          switch (kind) {
          | Melee =>
            let path =
              Pathfinder.pathfind(
                pathfinderInstance,
                (playerCellX, playerCellY),
                (cellX, cellY),
              );
            let shouldFollowPath =
              List.for_all(
                ((x, y)) =>
                  state.grid[x][y].kind != Door || !state.grid[x][y].collision,
                path,
              );
            if (!shouldFollowPath) {
              {
                ...defaultEnemyUpdates,
                direction: {
                  x: 0.,
                  y: 0.,
                },
              };
            } else {
              /* @FixMe @Hack Somehow enemies keep swapping their direction and it doesn't make much sense.*/
              switch (path) {
              | [] => {
                  ...defaultEnemyUpdates,
                  direction: {
                    x: 0.,
                    y: 0.,
                  },
                }
              | [_]
              | [_, _] =>
                moveInPlayerDirection(
                  defaultEnemyUpdates,
                  mag,
                  playerDirectionX,
                  playerDirectionY,
                )
              | [_, next, nextnext, ...rest] =>
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
                let vx = destRelX /. mag *. speed;
                let vy = destRelY /. mag *. speed;
                let (resolvedVx, resolvedVy, forcedToMove) = (vx, vy, false);
                {
                  ...defaultEnemyUpdates,
                  forcefullyMovedTimer:
                    forcedToMove ?
                      forcefullyMovedTimerDefaultValue :
                      defaultEnemyUpdates.forcefullyMovedTimer,
                  pos: {
                    x: x +. resolvedVx *. dt,
                    y: y +. resolvedVy *. dt,
                  },
                  direction: {
                    x: resolvedVx,
                    y: resolvedVy,
                  },
                };
              };
            };
          | Shooter =>
            /* @Hack @FixMe had to disable pathfinding on the massive maps because it was too slow... */
            let anyWallsInBetweenEnemyAndPlayer =
              isThereAWallBetweenTheEnemyAndThePlayer(
                state,
                x,
                y,
                mag,
                playerDirectionX,
                playerDirectionY,
              );

            if (!anyWallsInBetweenEnemyAndPlayer && mag < enemy.weaponRange) {
              let enemy =
                if (timeUntilNextAttack <= 0.) {
                  {
                    ...defaultEnemyUpdates,
                    timeUntilNextAttack: 1.,
                    /*direction: {
                        x: 0.,
                        y: 0.,
                      },*/
                    bullets: [
                      {
                        x,
                        y,
                        vx: playerDirectionX /. mag *. enemy.bulletSpeed,
                        vy: playerDirectionY /. mag *. enemy.bulletSpeed,
                        timeRemaining: defaultEnemyUpdates.bulletLifeSpan,
                      },
                      ...enemy.bullets,
                    ],
                  };
                } else {
                  defaultEnemyUpdates;
                };

              let minDistanceBetweenPlayerAndShooter = 160.;
              if (mag < minDistanceBetweenPlayerAndShooter) {
                moveInPlayerDirection(
                  enemy,
                  mag,
                  -. playerDirectionX,
                  -. playerDirectionY,
                );
              } else {
                let direction = {
                  x:
                    enemy.direction.x
                    +. Utils.noise(x, y, state.time)
                    *. 2.
                    -. 1.,
                  y:
                    enemy.direction.y
                    +. Utils.noise(x, y, state.time)
                    *. 2.
                    -. 1.,
                };
                {
                  ...enemy,
                  direction,
                  pos: {
                    x: enemy.pos.x +. direction.x *. dt,
                    y: enemy.pos.y +. direction.y *. dt,
                  },
                };
              };
            } else {
              let path =
                Pathfinder.pathfind(
                  pathfinderInstance,
                  (playerCellX, playerCellY),
                  (cellX, cellY),
                );
              switch (path) {
              | [] => {
                  ...defaultEnemyUpdates,
                  direction: {
                    x: 0.,
                    y: 0.,
                  },
                }
              | [_]
              | [_, _] =>
                moveInPlayerDirection(
                  defaultEnemyUpdates,
                  mag,
                  playerDirectionX,
                  playerDirectionY,
                )
              | [_, next, nextnext, ...rest] =>
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
                let vx = destRelX /. mag *. speed;
                let vy = destRelY /. mag *. speed;

                let (resolvedVx, resolvedVy) =
                  switch (resolveCollision(~state, ~dt, x, y, vx, vy)) {
                  | None => (0., 0.)
                  | Some(direction) => direction
                  };

                let enemiesInArea = enemiesInRegion[cellX / enemyGridScale][cellY
                                                                    / enemyGridScale];
                let (resolvedVx, resolvedVy, forcedToMove) =
                  pushEnemyIfNecessary(
                    ~state,
                    ~enemy,
                    ~dt,
                    ~enemiesInArea,
                    resolvedVx,
                    resolvedVy,
                  );
                {
                  ...defaultEnemyUpdates,
                  forcefullyMovedTimer:
                    forcedToMove ?
                      forcefullyMovedTimerDefaultValue :
                      defaultEnemyUpdates.forcefullyMovedTimer,
                  pos: {
                    x: x +. resolvedVx *. dt,
                    y: y +. resolvedVy *. dt,
                  },
                  direction: {
                    x: resolvedVx,
                    y: resolvedVy,
                  },
                };
              };
            };
          };
        },
      state.enemies,
    );

  /* Manage attacking */
  let (health, enemies, currentPowerups) =
    List.fold_left(
      (
        (health, enemies, currentPowerups),
        {pos: {x, y}, isDead, timeUntilNextAttack, kind} as enemy,
      ) =>
        switch (kind) {
        | Melee =>
          if (isDead) {
            (health, [enemy, ...enemies], currentPowerups);
          } else if (timeUntilNextAttack <= 0.) {
            let dx = state.x -. x;
            let dy = state.y -. y;
            let mag = sqrt(dx *. dx +. dy *. dy);
            if (mag < enemyAttackDistance) {
              let (currentPowerups, usedAPowerup) =
                List.fold_left(
                  ((currentPowerups, usedAPowerup), p: powerupT) =>
                    switch (p.kind, usedAPowerup) {
                    | (Armor, false) => (currentPowerups, true)
                    | _ => ([p, ...currentPowerups], usedAPowerup)
                    },
                  ([], false),
                  currentPowerups,
                );
              (
                usedAPowerup ? health : health - 101,
                [{...enemy, timeUntilNextAttack: 1.}, ...enemies],
                currentPowerups,
              );
            } else {
              (health, [enemy, ...enemies], currentPowerups);
            };
          } else {
            (health, [enemy, ...enemies], currentPowerups);
          }
        | Shooter =>
          let (health, bullets, currentPowerups) =
            List.fold_left(
              (
                (health, bullets, currentPowerups),
                {x, y, vx, vy} as bullet: bulletT,
              ) => {
                let (playerDirectionX, playerDirectionY) = (
                  state.x -. x,
                  state.y -. y,
                );
                let mag =
                  sqrt(
                    playerDirectionX
                    *. playerDirectionX
                    +. playerDirectionY
                    *. playerDirectionY,
                  );
                let playerBoundingCircle = 20.;
                if (mag < playerBoundingCircle) {
                  Env.playSound(
                    state.sounds.playerShotSound,
                    ~volume=1.0,
                    ~loop=false,
                    env,
                  );

                  let (currentPowerups, usedAPowerup) =
                    List.fold_left(
                      ((currentPowerups, usedAPowerup), p: powerupT) =>
                        switch (p.kind, usedAPowerup) {
                        | (Armor, false) => (currentPowerups, true)
                        | _ => ([p, ...currentPowerups], usedAPowerup)
                        },
                      ([], false),
                      currentPowerups,
                    );
                  (
                    usedAPowerup ? health : health - enemy.bulletDamage,
                    bullets,
                    currentPowerups,
                  );
                } else {
                  switch (
                    resolveCollision(
                      ~state,
                      ~dt,
                      ~allowSlide=false,
                      x,
                      y,
                      vx,
                      vy,
                    )
                  ) {
                  | None => (health, bullets, currentPowerups)
                  | Some((vx, vy)) => (
                      health,
                      [
                        {
                          x: x +. vx *. dt,
                          y: y +. vy *. dt,
                          vx,
                          vy,
                          timeRemaining: bullet.timeRemaining -. dt,
                        },
                        ...bullets,
                      ],
                      currentPowerups,
                    )
                  };
                };
              },
              (health, [], currentPowerups),
              enemy.bullets,
            );
          (health, [{...enemy, bullets}, ...enemies], currentPowerups);
        },
      (state.health, [], state.currentPowerups),
      movedEnemies,
    );
  /* @Hack rev the enemies here because the fold_left reverses their order, making their order annoyingly unpredictable. */
  let enemies = List.rev(enemies);
  {...state, enemies, health, currentPowerups};
};

let spawnPowerups = (state, playerXScreenScaledf, playerYScreenScaledf, env) => {
  let numberOfPowerups =
    Array.fold_left(
      (count, column) =>
        Array.fold_left(
          (count, {kind}) =>
            switch (kind) {
            | Powerup(_) => count + 1
            | _ => count
            },
          count,
          column,
        ),
      0,
      state.grid,
    );

  let maxNumberOfPowerups = 2;
  if (numberOfPowerups < maxNumberOfPowerups) {
    let (cellX, cellY) = (
      int_of_float(floor(state.x /. tileSizef)),
      int_of_float(floor(state.y /. tileSizef)),
    );
    let (cellNumHalfWidth, cellNumHalfHeight) = (
      int_of_float(playerXScreenScaledf /. tileSizef),
      int_of_float(playerYScreenScaledf /. tileSizef),
    );

    let x = ref(-1);
    let y = ref(-1);
    let running = ref(true);
    while (running^) {
      let whichQuadrant = Utils.random(~min=0, ~max=4);

      let (minX, maxX, minY, maxY) =
        if (whichQuadrant == 0) {
          (
            /* TOP */
            0,
            gridWidth,
            0,
            max(0, cellY - cellNumHalfHeight - 1),
          );
        } else if (whichQuadrant == 1) {
          (
            /* RIGHT */
            min(gridWidth, cellX + cellNumHalfWidth + 1),
            gridWidth,
            0,
            gridHeight,
          );
        } else if (whichQuadrant == 2) {
          (
            /* BOTTOM */
            0,
            gridWidth,
            min(gridHeight, cellY + cellNumHalfHeight + 1),
            gridHeight,
          );
        } else {
          (
            /* LEFT */
            0,
            max(0, cellX - cellNumHalfWidth - 1),
            0,
            gridHeight,
          );
        };
      if (minX != maxX && minY != maxY) {
        x := Utils.random(~min=minX, ~max=maxX);
        y := Utils.random(~min=minY, ~max=maxY);
      };
      switch (getCell(state.grid, (x^, y^))) {
      | {collision: true}
      | {kind: Powerup(_)} => ()
      | _ => running := false
      };
    };

    /* @Mutation */
    state.grid[x^][y^] = {
      collision: false,
      kind: Powerup({time: 1., kind: Armor}),
    };
  };

  state;
};

/* We use the scaled screen coords because otherwise things are off! */
let spawnEnemies =
    (
      state,
      playerXScreenScaledf,
      playerYScreenScaledf,
      ~outsideOfFieldOfView=false,
      env,
    ) => {
  let (cellX, cellY) = (
    int_of_float(floor(state.x /. tileSizef)),
    int_of_float(floor(state.y /. tileSizef)),
  );

  let (cellNumHalfWidth, cellNumHalfHeight) = (
    int_of_float(playerXScreenScaledf /. tileSizef),
    int_of_float(playerYScreenScaledf /. tileSizef),
  );
  let x = ref(-1);
  let y = ref(-1);

  while (getCell(state.grid, (x^, y^)).collision) {
    if (outsideOfFieldOfView) {
      let whichQuadrant = Utils.random(~min=0, ~max=4);
      let (minX, maxX, minY, maxY) =
        if (whichQuadrant == 0) {
          /* TOP */
          let minX = max(0, cellX - cellNumHalfWidth - 1);
          let maxX = min(gridWidth, cellX + cellNumHalfWidth + 1);
          let minY = max(0, cellY - cellNumHalfHeight - 2);
          let maxY = minY + 1;
          (minX, maxX, minY, maxY);
        } else if (whichQuadrant == 1) {
          /* RIGHT */
          let minX = min(gridWidth, cellX + cellNumHalfWidth + 1);
          let maxX = minX + 1;
          let minY = max(0, cellY - cellNumHalfHeight - 1);
          let maxY = min(gridHeight, cellY + cellNumHalfHeight + 1);
          (minX, maxX, minY, maxY);
        } else if (whichQuadrant == 2) {
          /* BOTTOM */
          let minX = max(0, cellX - cellNumHalfWidth - 1);
          let maxX = min(gridWidth, cellX + cellNumHalfWidth + 1);
          let minY = min(gridHeight, cellY + cellNumHalfHeight + 2);
          let maxY = minY + 1;
          (minX, maxX, minY, maxY);
        } else {
          /* LEFT */
          let minX = max(0, cellX - cellNumHalfWidth - 2);
          let maxX = minX + 1;
          let minY = max(0, cellY - cellNumHalfHeight - 1);
          let maxY = min(gridHeight, cellY + cellNumHalfHeight + 1);
          (minX, maxX, minY, maxY);
        };
      if (minX != maxX && minY != maxY) {
        x := Utils.random(~min=minX, ~max=maxX);
        y := Utils.random(~min=minY, ~max=maxY);
      };
    } else {
      x := Utils.random(~min=0, ~max=gridWidth);
      y := Utils.random(~min=0, ~max=gridHeight);
    };
  };

  let isShooter = Utils.randomf(~min=0., ~max=1.) < 0.7;

  let enemies = [
    {
      id: makeEnemyID(),
      pos: {
        x: float_of_int(x^) *. tileSizef +. tileSizef /. 2.,
        y: float_of_int(y^) *. tileSizef +. tileSizef /. 2.,
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
      kind: isShooter ? Shooter : Melee,
      bullets: [],
      bulletSpeed: isShooter ? 300. : 0.,
      bulletDamage: isShooter ? 100 : 0,
      weaponRange: isShooter ? 200. : 0.,
      isDead: false,
      bulletLifeSpan: 2.,
    },
    ...state.enemies,
  ];
  {...state, timeSinceLastSpawned: state.time, enemies};
};

let checkSwapWeaponButton =
    (state, mx, my, playerXScreenf, playerYScreenf, env) => {
  let (swapWeaponsButtonWidth, swapWeaponsButtonHeight) = (64., 64.);
  let (swapWeaponsButtonX, swapWeaponsButtonY) = (
    playerXScreenf -. float_of_int(halfPlayerSize),
    playerYScreenf -. float_of_int(halfPlayerSize),
  );
  switch (state.aim, Env.mousePressed(env)) {
  | (Aiming({x: sx, y: sy, points, startAimTime}), false)
      when state.prevMouseState || state.didTap =>
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
    if ((state.prevMouseState || state.didTap)
        && mx > swapWeaponsButtonX
        && mx < swapWeaponsButtonX
        +. swapWeaponsButtonWidth
        && my > swapWeaponsButtonY
        && my < swapWeaponsButtonY
        +. swapWeaponsButtonHeight
        && state.realTime
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
};

let movePlayerAndAttack = (state, dt, didTapOnSwapButton, mx, my, env) => {
  let state =
    switch (state.aim, Env.mousePressed(env)) {
    | (Aiming({x: sx, y: sy, points} as aiming), true) => state
    /*let (dx, dy) = (sx -. mx, sy -. my);
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
          Aiming({...aiming, x: sx, y: sy, points: [(mx, my), ...points]}),
      };*/
    | (Nothing, false) => state
    | (Nothing, true) => {
        ...state,
        aim: Aiming({x: mx, y: my, points: [], startAimTime: state.realTime}),
      }
    | (Aiming({x: sx, y: sy, points, startAimTime}), false) =>
      if (!didTapOnSwapButton) {
        let currentWeapon = state.weapons[state.currentWeaponIndex];
        let (dx, dy) =
          if (currentWeapon.kind == ShootsBehindYou
              || state.experiment == _INVERTED_SHOOTING) {
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
              if (currentWeapon.kind == ShootsBehindYou
                  || state.experiment == _INVERTED_SHOOTING) {
                List.fold_left(
                  ((dx, dy), (x, y)) => (dx +. (x -. sx), dy +. (y -. sy)),
                  (dx, dy),
                  firstCoupleOfPoints,
                );
              } else {
                List.fold_left(
                  ((dx, dy), (x, y)) => (dx +. (sx -. x), dy +. (sy -. y)),
                  (dx, dy),
                  firstCoupleOfPoints,
                );
              };

            (
              dx /. float_of_int(numberOfPointsToAverage),
              dy /. float_of_int(numberOfPointsToAverage),
            );
          } else {
            (dx, dy);
          };

        let shouldAssistAim =
          state.realTime
          -. startAimTime < autoaimDisengageTime
          && currentWeapon.kind != Shotgun;
        let (aimAssistdx, aimAssistdy) =
          if (shouldAssistAim) {
            let (dx, dy, _) = aimAssist(state, dx, dy, mag, env);
            (dx, dy);
          } else {
            (dx, dy);
          };
        /* We check if the user moved their finger enough to make a movement vector. This allows the user to cancel their movement. */
        if (mag > 20.) {
          let mag = sqrt(dx *. dx +. dy *. dy);
          let aimAssistmag =
            sqrt(aimAssistdx *. aimAssistdx +. aimAssistdy *. aimAssistdy);
          let bulletSpeed = currentWeapon.bulletSpeed;

          /* The movement goes in the other direction when the player's holding the
             ShootsBehindYou gun. Everything other than the movement (the aim and bullet) goes in the normal direction. */
          let (dx, dy) =
            if (currentWeapon.kind == ShootsBehindYou) {
              (-. dx, -. dy);
            } else {
              (dx, dy);
            };

          /* Don't do collision detection here, do it while the player's moving */
          let (velocity, currentMoveTime, totalMoveTime) = (
            {
              x:
                -. dx
                /. mag
                *. currentWeapon.playerTravelDistance
                /. currentWeapon.moveTime,
              y:
                -. dy
                /. mag
                *. currentWeapon.playerTravelDistance
                /. currentWeapon.moveTime,
            },
            0.,
            currentWeapon.moveTime,
          );
          let (dirX, dirY) = (
            aimAssistdx /. aimAssistmag,
            aimAssistdy /. aimAssistmag,
          );
          let bullets =
            if (currentWeapon.kind == Shotgun) {
              [
                {
                  timeRemaining: currentWeapon.bulletLifeSpan,
                  x: state.x -. dirX *. halfPlayerSizef,
                  y: state.y -. dirY *. halfPlayerSizef,
                  vx: -. dirX *. bulletSpeed,
                  vy: -. dirY *. bulletSpeed,
                },
                {
                  timeRemaining: currentWeapon.bulletLifeSpan,
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
                  timeRemaining: currentWeapon.bulletLifeSpan,

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
                  timeRemaining: currentWeapon.bulletLifeSpan,

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
                  timeRemaining: currentWeapon.bulletLifeSpan,
                  x: state.x -. dirX *. halfPlayerSizef,
                  y: state.y -. dirY *. halfPlayerSizef,
                  vx: -. dirX *. bulletSpeed,
                  vy: -. dirY *. bulletSpeed,
                },
                ...state.bullets,
              ];
            };
          {
            ...state,
            aim: Nothing,
            currentMoveTime,
            totalMoveTime,
            velocity,
            bullets,
          };
        } else {
          {...state, aim: Nothing};
        };
      } else {
        state;
      }
    };

  if (state.currentMoveTime >= state.totalMoveTime) {
    {...state, currentMoveTime: 0., totalMoveTime: 0., velocity: zeroVec};
  } else {
    let cellPos = (
      int_of_float(floor((state.x +. state.velocity.x *. dt) /. tileSizef)),
      int_of_float(floor((state.y +. state.velocity.y *. dt) /. tileSizef)),
    );
    let cell = getCell(state.grid, cellPos);
    if (cell.kind == Door && cell.collision) {
      /* @Mutation */
      setCell(
        state.grid,
        cellPos,
        {...cell, collision: false},
      );
    };

    /* Only allow one powerup at a time right now

              Ben - October 26th 2018
       */
    let state =
      switch (cell.kind) {
      | Powerup(p) when List.length(state.currentPowerups) == 0 =>
        /* @Mutation */
        setCell(state.grid, cellPos, {...cell, kind: Floor});
        {...state, currentPowerups: [p, ...state.currentPowerups]};
      | _ => state
      };

    switch (
      resolveCollision(
        ~state,
        ~dt,
        state.x,
        state.y,
        state.velocity.x,
        state.velocity.y,
      )
    ) {
    | None => {
        ...state,
        velocity: zeroVec,
        currentMoveTime: 0.,
        totalMoveTime: 0.,
      }
    | Some((vx, vy)) => {
        ...state,
        x: state.x +. vx *. dt,
        y: state.y +. vy *. dt,
        velocity: {
          x: vx,
          y: vy,
        },
        currentMoveTime: state.currentMoveTime +. dt,
      }
    };
  };
};

let setBackgroundColor = (state, env) =>
  if (state.experiment == _DEBUG) {
    Draw.background(Utils.color(~r=255, ~g=220, ~b=200, ~a=255), env);
  } else {
    Draw.background(Constants.white, env);
  };

let drawPowerup = (x, y, kind, env) => {
  switch (kind) {
  | Armor => Draw.fill(Utils.color(77, 77, 77, 255), env)
  | _ => Draw.fill(Utils.color(0, 0, 0, 255), env)
  };
  let rad = 12.;
  Draw.ellipsef(
    ~center=(x +. tileSizef /. 2., y +. tileSizef /. 2.),
    ~radx=rad,
    ~rady=rad,
    env,
  );
};

let drawBackground = (state, playerXScreenf, playerYScreenf, env) =>
  Array.iteri(
    (cellX, column) =>
      Array.iteri(
        (cellY, {kind, collision}) => {
          let (x, y) = (
            float_of_int(cellX) *. tileSizef,
            float_of_int(cellY) *. tileSizef,
          );
          let paddingForSomeGodDamReason = 10.;
          let left = state.x -. playerXScreenf;
          let right = state.x +. playerXScreenf;
          let top = state.y -. playerYScreenf -. paddingForSomeGodDamReason;
          let bottom = state.y +. playerYScreenf +. paddingForSomeGodDamReason;
          if (x
              +. tileSizef > left
              && x < right
              && y
              +. tileSizef > top
              && y < bottom) {
            switch (kind) {
            | Door =>
              if (collision) {
                Draw.fill(Utils.color(~r=205, ~g=133, ~b=63, ~a=255), env);
              } else {
                Draw.fill(Utils.color(~r=255, ~g=183, ~b=113, ~a=255), env);
              };
              Draw.noStroke(env);
              let leftCell = state.grid[cellX - 1][cellY];
              let topCell = state.grid[cellX][cellY - 1];
              if (leftCell.kind == Wall || leftCell.kind == Door) {
                Draw.rectf(
                  ~pos=(x, y +. tileSizef /. 2. -. 5.),
                  ~width=tileSizef,
                  ~height=10.,
                  env,
                );
              } else if (topCell.kind == Wall || topCell.kind == Door) {
                Draw.rectf(
                  ~pos=(x +. tileSizef /. 2. -. 5., y),
                  ~width=10.,
                  ~height=tileSizef,
                  env,
                );
              } else {
                Draw.rectf(
                  ~pos=(x, y),
                  ~width=tileSizef,
                  ~height=tileSizef,
                  env,
                );
              };
            | Floor =>
              Draw.fill(Constants.white, env);
              Draw.noStroke(env);

              /*Draw.stroke(Constants.black, env);
                Draw.strokeWeight(1, env);*/
              Draw.rectf(
                ~pos=(x, y),
                ~width=tileSizef,
                ~height=tileSizef,
                env,
              );
            | Wall =>
              Draw.fill(Utils.color(~r=124, ~g=124, ~b=124, ~a=255), env);
              Draw.noStroke(env);
              /*Draw.stroke(Constants.black, env);
                Draw.strokeWeight(1, env);*/
              Draw.rectf(
                ~pos=(x, y),
                ~width=tileSizef,
                ~height=tileSizef,
                env,
              );
            | Powerup({kind}) => drawPowerup(x, y, kind, env)
            };
          };
        },
        column,
      ),
    state.grid,
  );

let drawDeathMessage = (state, env) => {
  let ((x, y), _) = Levels.levels[state.currentLevel];
  let (x, y) = cellToWorld((x, y));
  let body = "revived";
  let textWidth = Draw.textWidth(~body, env);
  Draw.pushStyle(env);
  Draw.noStroke(env);
  Draw.strokeCap(Round, env);
  Draw.strokeWeight(8, env);
  Draw.stroke(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.rect(
    ~pos=(int_of_float(x) - textWidth / 2 - 8, (-172)),
    ~width=textWidth + 16,
    ~height=80,
    env,
  );
  Draw.tint(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.text(~body, ~pos=(int_of_float(x) - textWidth / 2, (-164)), env);
  let body = "go fight";
  let textWidth = Draw.textWidth(~body, env);
  Draw.text(~body, ~pos=(int_of_float(x) - textWidth / 2, (-134)), env);
  Draw.popStyle(env);
};

let drawSplashes = (state, env) => {
  Draw.pushStyle(env);
  Draw.tint(Utils.color(255, 255, 255, 150), env);
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
  Draw.popStyle(env);
};

let gunColor = Utils.color(~r=100, ~g=140, ~b=240, ~a=255);
let shotgunColor = Utils.color(~r=80, ~g=200, ~b=100, ~a=255);

let drawBullets =
    (
      bullets,
      dt,
      ~color=Utils.color(~r=255, ~g=255, ~b=255, ~a=255),
      ~strokeWeight=1,
      ~bulletWidth as radx=12.,
      env,
    ) => {
  /* Draw the bullets.
     Uses some super hacky angle calculation do draw bullets as ellipses pointing in the right
     direction. */
  Draw.fill(color, env);
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
      Draw.strokeWeight(strokeWeight, env);
      Draw.ellipsef(~center=(0., 0.), ~radx, ~rady=2., env);
      Draw.popMatrix(env);
    },
    bullets,
  );
};

let drawScore = (state, env) => {
  let body = sp("%d", state.score);
  let textWidth = Draw.textWidth(~body, env);
  let (x, y) = (Env.width(env) - textWidth - 24, 42);
  Draw.pushStyle(env);
  Draw.strokeCap(Round, env);
  Draw.strokeWeight(8, env);
  Draw.stroke(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.rect(~pos=(x - 8, y - 8), ~width=textWidth + 16, ~height=48, env);
  Draw.tint(Utils.color(~r=0, ~g=0, ~b=0, ~a=150), env);
  Draw.text(~pos=(x, y), ~body, env);
  Draw.popStyle(env);
};

let drawBestScore = (state, env) => {
  let body = sp("Best score: %d", state.maxScore);
  let ((x, y), _) = Levels.levels[state.currentLevel];
  let (x, y) = cellToWorld((x, y));
  Draw.pushStyle(env);
  Draw.tint(Utils.color(~r=0, ~g=0, ~b=0, ~a=150), env);
  Draw.text(
    ~pos=(int_of_float(x) - Draw.textWidth(~body, env) / 2, (-48)),
    ~body,
    env,
  );
  Draw.popStyle(env);
};

let drawLevelName = (state, env) => {
  let ((x, y), _) = Levels.levels[state.currentLevel];
  let (x, y) = cellToWorld((x, y));
  let body = sp("Level %d", state.currentLevel + 1);
  Draw.pushStyle(env);
  Draw.tint(Utils.color(~r=0, ~g=0, ~b=0, ~a=150), env);
  Draw.text(
    ~body,
    ~pos=(int_of_float(x) - Draw.textWidth(~body, env) / 2, (-88)),
    env,
  );
  Draw.popStyle(env);
};

let drawGuy =
    (
      (px, py),
      ~bodyColor=Utils.color(~r=41, ~g=166, ~b=244, ~a=255),
      time,
      env,
    ) => {
  let animationSpeed = 30.;
  let guyW = 30. /. 2.;
  let guyH = 30. /. 2.;
  let footW = 10. /. 2.;
  let footH = 5. /. 2.;
  let headSize = 13. /. 2.;
  Draw.noStroke(env);
  Draw.fill(bodyColor, env);
  Draw.ellipsef(~center=(px, py), ~radx=guyW, ~rady=guyH, env);
  Draw.fill(Constants.black, env);

  /* Legs */
  Draw.ellipsef(
    ~center=(
      px -. guyW /. 2. +. sin(time *. 10. +. 2.) *. footW /. 2.,
      py
      +. guyH
      -. 2.
      +. min(cos(time *. animationSpeed +. 2.) *. footH /. 2., 0.5),
    ),
    ~radx=footW,
    ~rady=footH,
    env,
  );

  Draw.ellipsef(
    ~center=(
      px +. guyW /. 2. +. sin(time *. animationSpeed) *. footW /. 2.,
      py
      +. guyH
      -. 2.
      +. min(cos(time *. animationSpeed) *. footH /. 3., 0.5),
    ),
    ~radx=footW,
    ~rady=footH,
    env,
  );

  /* Head */
  Draw.ellipsef(
    ~center=(
      px,
      py -. guyH +. headSize /. 2. +. sin(time *. animationSpeed),
    ),
    ~radx=headSize,
    ~rady=headSize,
    env,
  );
};

let drawEnemies = (state, dt, realdt, pathfinderInstance, env) => {
  let (playerCellX, playerCellY) = (
    int_of_float(floor(state.x /. tileSizef)),
    int_of_float(floor(state.y /. tileSizef)),
  );

  /*let visibleEnemies = getVisibleEnemies(state, env);*/
  List.iter(
    ({pos: {x, y}, isDead, direction, speed, bullets}) => {
      let color = Utils.color(~r=255, ~g=20, ~b=50, ~a=255);
      if (!isDead) {
        /*Draw.tint(Utils.color(~r=255, ~g=255, ~b=255, ~a=50), env);
          drawWithRotation(
            AssetMap.find("robot1", state.assetMap),
            ~pos=(
              x -. direction.x *. (realdt -. dt) *. 12.,
              y -. direction.y *. (realdt -. dt) *. 12.,
            ),
            ~width=224. /. 7.,
            ~height=344. /. 7.,
            ~rot=0.,
            env,
          );
          Draw.tint(Utils.color(~r=255, ~g=255, ~b=255, ~a=100), env);
          drawWithRotation(
            AssetMap.find("robot1", state.assetMap),
            ~pos=(
              x -. direction.x *. (realdt -. dt) *. 8.,
              y -. direction.y *. (realdt -. dt) *. 8.,
            ),
            ~width=224. /. 7.,
            ~height=344. /. 7.,
            ~rot=0.,
            env,
          );*/
        Draw.noTint(env);
        drawGuy((x, y), ~bodyColor=color, state.time, env);
        /*drawWithRotation(
            AssetMap.find("robot1", state.assetMap),
            ~pos=(x, y),
            ~width=224. /. 7.,
            ~height=344. /. 7.,
            ~rot=0.,
            env,
          );*/
      };

      drawBullets(bullets, dt, ~color, ~bulletWidth=8., ~strokeWeight=2, env);

      if (state.experiment == _DEBUG) {
        let (cellX, cellY) = (
          int_of_float(floor(x /. tileSizef)),
          int_of_float(floor(y /. tileSizef)),
        );
        if (cellX >= 0 && cellX < gridWidth && cellY >= 0 && cellY < gridHeight) {
          let cell = state.grid[cellX][cellY];
          if (!cell.collision && !isDead) {
            let path =
              Pathfinder.pathfind(
                pathfinderInstance,
                (playerCellX, playerCellY),
                (cellX, cellY),
              );
            switch (path) {
            | [] => ()
            | [_]
            | [_, _] =>
              let (dx, dy) = (state.x -. x, state.y -. y);
              let mag = sqrt(dx *. dx +. dy *. dy);
              Draw.stroke(Utils.color(~r=0, ~g=0, ~b=255, ~a=100), env);
              Draw.strokeWeight(3, env);
              Draw.linef(
                ~p1=(x, y),
                ~p2=(x +. dx /. mag *. speed, y +. dy /. mag *. speed),
                env,
              );
            | [_, next, nextnext, ...rest] =>
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
              Draw.stroke(Utils.color(~r=0, ~g=0, ~b=255, ~a=100), env);
              Draw.strokeWeight(3, env);
              Draw.linef(
                ~p1=(x, y),
                ~p2=(
                  x +. destRelX /. mag *. speed,
                  y +. destRelY /. mag *. speed,
                ),
                env,
              );
            };

            let (prevX, prevY) = (ref(x), ref(y));
            List.iter(
              ((x, y)) => {
                Draw.stroke(Utils.color(~r=255, ~g=0, ~b=0, ~a=100), env);
                Draw.strokeWeight(3, env);
                let (x, y) = (
                  float_of_int(x) *. tileSizef +. tileSizef /. 2.,
                  float_of_int(y) *. tileSizef +. tileSizef /. 2.,
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

  /* Draw pineapple dots */
  if (state.experiment == _DEBUG) {
    Draw.pushStyle(env);
    List.iter(
      ({pos: {x, y}}: enemyT) => {
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
      state.enemies,
    );
    Draw.popStyle(env);
  };
};

let drawAimLine = (state, mx, my, playerXScreenf, playerYScreenf, env) =>
  switch (state.aim, Env.mousePressed(env)) {
  | (Aiming({x: sx, y: sy, points, startAimTime}), true) =>
    let currentWeapon = state.weapons[state.currentWeaponIndex];

    let (dx, dy) =
      if (currentWeapon.kind == ShootsBehindYou
          || state.experiment == _INVERTED_SHOOTING) {
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

      if (currentWeapon.kind == ShootsBehindYou
          || state.experiment == _INVERTED_SHOOTING) {
        let (dx, dy) = (sx -. mx, sy -. my);
        let mag = sqrt(dx *. dx +. dy *. dy);

        let moveX = dx /. mag *. currentWeapon.playerTravelDistance;
        let moveY = dy /. mag *. currentWeapon.playerTravelDistance;
        Draw.strokeWeight(4, env);
        Draw.stroke(Utils.color(~r=100, ~g=100, ~b=200, ~a=200), env);
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

        let shouldAssistAim =
          state.realTime -. startAimTime < autoaimDisengageTime;
        if (shouldAssistAim) {
          let (dx, dy, dot) = aimAssist(state, dx, dy, mag, env);
          let aimAssistMinAngle = 0.97;
          if (dot > aimAssistMinAngle) {
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
        Draw.noFill(env);
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

let drawHealthBar = (state, env) => {
  Draw.noStroke(env);

  Draw.fill(Utils.color(~r=124, ~g=10, ~b=2, ~a=200), env);
  let padding = 4.;
  let height = 24.;
  let width = 100. +. padding *. 2.;
  let x = 24.;
  let y = 42.;
  Draw.rectf(
    ~pos=(x, y),
    ~width=width *. globalScale,
    ~height=height *. globalScale,
    env,
  );

  Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.rectf(
    ~pos=(x +. padding *. globalScale, y +. padding *. globalScale),
    ~width=(width -. 2. *. padding) *. globalScale,
    ~height=(height -. 2. *. padding) *. globalScale,
    env,
  );
  Draw.fill(Utils.color(~r=30, ~g=100, ~b=30, ~a=200), env);
  Draw.rectf(
    ~pos=(x +. padding *. globalScale, y +. padding *. globalScale),
    ~width=(width -. 2. *. padding) *. globalScale,
    ~height=(height -. 2. *. padding) *. globalScale,
    env,
  );
};

let getLevel = currentLevel => {
  let ((sx, sy), map) = Levels.levels[currentLevel];
  let (grid, enemies) = Levels.parseMap(map);
  if (currentLevel == 8) {
    let x = ref(-1);
    let y = ref(-1);

    while (getCell(grid, (x^, y^)).collision) {
      x := Utils.random(~min=0, ~max=gridWidth);
      y := Utils.random(~min=0, ~max=gridHeight);
    };

    (x^, y^, grid, enemies);
  } else {
    (sx, sy, grid, enemies);
  };
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

  let currentLevel = 8;

  let (startPosX, startPosY, grid, enemies) = getLevel(currentLevel);
  {
    x: float_of_int(startPosX) *. tileSizef +. tileSizef /. 2.,
    y: float_of_int(startPosY) *. tileSizef +. tileSizef /. 2.,
    realTime: 0.,
    time: 0.,
    score: 0,
    maxScore: 0,
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
      playerShotSound: Env.loadSound(assetDir +/ "playerShotSound.wav", env),
    },
    currentWeaponIndex: 0,
    deathTime: 0.,
    weapons: [|
      {
        length: 5,
        moveTime: 0.2,
        playerTravelDistance: 40.,
        bulletSpeed: 1200.,
        kind: Pistol,
        bulletLifeSpan: 0.5,
      },
      {
        length: 10,
        moveTime: 0.3,
        playerTravelDistance: 80.,
        bulletSpeed: 1200.,
        kind: Shotgun,
        bulletLifeSpan: 0.2,
      },
    |],
    currentPowerups: [],
    grid,
    velocity: {
      x: 0.,
      y: 0.,
    },
    currentMoveTime: 0.,
    totalMoveTime: 0.,
    currentLevel,
    touches: [],
    didTap: false,
    timeSinceLastSpawned: 0.,
  };
};

let draw = (state, env) => {
  let realdt = Env.deltaTime(env);
  let dt = slowDownTime(~state, ~realdt, env);

  let (windowW, windowH) = (Env.width(env), Env.height(env));
  let (windowWf, windowHf) = (
    float_of_int(windowW),
    float_of_int(windowH),
  );

  let paddingForScale = globalScale < 1. ? 20. : 0.;
  let (playerXScreenf, playerYScreenf) = (windowWf /. 2., windowHf /. 2.);
  let (playerXScreenScaledf, playerYScreenScaledf) = (
    playerXScreenf *. (2. -. globalScale) +. paddingForScale,
    playerYScreenf *. (2. -. globalScale) +. paddingForScale,
  );

  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   *                                     EVENTS + UPDATES                                        *
   * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

  /* Advance time before doing any calculations. No real reason why. */
  let state = {
    ...state,
    realTime: state.realTime +. realdt,
    time: state.time +. dt,
    deathTime: max(0., state.deathTime -. realdt),
  };

  let (mx, my, _mx2, _my2, numOfTouches) = getTouchPositions(env);

  /* Bullet collision detection and response */
  let state = moveBullets(state, dt, ~removeIfTooFar=true, env);

  let state =
    if ((
          state.currentLevel == 6
          || state.currentLevel == 7
          || state.currentLevel == 8
        )
        && Utils.randomf(~min=0., ~max=100.) < 50.
        *. dt) {
      spawnPowerups(state, playerXScreenScaledf, playerYScreenScaledf, env);
    } else {
      state;
    };

  let cosUpperBound = 1.6;
  let cosLowerBound = 0.8;
  let cosUpperBoundHard = 1.2;
  let cosLowerBoundHard = 0.2;

  /* In seconds */
  let maxGameTimeUntilVeryHard = 800.;

  let cosUpperBound =
    -. ((state.time /. maxGameTimeUntilVeryHard) ** 2.) +. cosUpperBound;
  let cosUpperBound = max(cosUpperBound, cosUpperBoundHard);

  let cosLowerBound =
    -. ((state.time /. (maxGameTimeUntilVeryHard +. 20.)) ** 2.)
    +. cosLowerBound;
  let cosLowerBound = max(cosLowerBound, cosLowerBoundHard);

  let cosPhaseShift = 2.;

  /* Maybe scale this based on your level */
  let timeScalingFactor = 5.;
  let cosSize = cosUpperBound -. cosLowerBound;
  let timeUntilNextSpawn =
    state.timeSinceLastSpawned
    +. cos(state.time /. timeScalingFactor +. cosPhaseShift)
    /. 2.
    *. cosSize
    +. cosSize
    /. 2.
    +. cosLowerBound;
  let shouldSpawnEnemy = state.time > timeUntilNextSpawn;
  let body =
    sp(
      "%f - %f",
      state.time,
      timeUntilNextSpawn -. state.timeSinceLastSpawned,
    );
  let state =
    if ((
          state.currentLevel == 6
          || state.currentLevel == 7
          || state.currentLevel == 8
        )
        && shouldSpawnEnemy) {
      let state =
        spawnEnemies(
          state,
          playerXScreenScaledf,
          playerYScreenScaledf,
          ~outsideOfFieldOfView=true,
          env,
        );
      spawnEnemies(
        state,
        playerXScreenScaledf,
        playerYScreenScaledf,
        ~outsideOfFieldOfView=true,
        env,
      );
    } else {
      state;
    };

  let pathfinderInstance = Pathfinder.make(state.grid);
  let state = moveEnemiesAndAttack(state, dt, pathfinderInstance, env);

  let (state, didTapOnSwapButton) =
    checkSwapWeaponButton(state, mx, my, playerXScreenf, playerYScreenf, env);

  let (devButtonX, devButtonY) = (60., 60.);

  let state =
    if (state.didTap && mx < devButtonX && my < devButtonY) {
      {
        ...state,
        experiment: (state.experiment + 1) mod (_NUMBER_OF_EXPERIMENTS + 1),
      };
    } else if (state.didTap
               && mx > float_of_int(Env.width(env))
               -. devButtonX
               && my < devButtonY) {
      {
        ...state,
        health: (-1), /* Massive @Hack to move forward in levels */
        currentLevel: (state.currentLevel + 1) mod Levels.numberOfLevels,
      };
    } else {
      movePlayerAndAttack(state, dt, didTapOnSwapButton, mx, my, env);
    };

  let state = {...state, maxScore: max(state.maxScore, state.score)};

  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
   *                                           DRAWING                                           *
   * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

  /* Tint everything red based on health */
  Draw.tint(
    Utils.color(~r=255, ~g=state.health + 155, ~b=state.health + 155, ~a=255),
    env,
  );

  setBackgroundColor(state, env);

  Draw.pushMatrix(env);

  Draw.translate(~x=playerXScreenf, ~y=playerYScreenf, env);
  Draw.scale(~x=globalScale, ~y=globalScale, env);
  Draw.translate(~x=-. state.x, ~y=-. state.y, env);

  drawBackground(state, playerXScreenScaledf, playerYScreenScaledf, env);

  if (state.experiment == _DEBUG) {
    Pathfinder.TupleSet.iter(
      ((x, y)) => {
        let (x, y) = (
          float_of_int(x) *. tileSizef,
          float_of_int(y) *. tileSizef,
        );
        Draw.fill(Utils.color(~r=205, ~g=0, ~b=63, ~a=150), env);
        Draw.noStroke(env);
        Draw.rectf(
          ~pos=(x, y +. tileSizef /. 2. -. 5.),
          ~width=tileSizef,
          ~height=tileSizef,
          env,
        );
      },
      pathfinderInstance.openSet,
    );

    Pathfinder.TupleSet.iter(
      ((x, y)) => {
        let (x, y) = (
          float_of_int(x) *. tileSizef,
          float_of_int(y) *. tileSizef,
        );
        Draw.fill(Utils.color(~r=0, ~g=0, ~b=255, ~a=150), env);
        Draw.noStroke(env);
        Draw.rectf(
          ~pos=(x, y +. tileSizef /. 2. -. 5.),
          ~width=tileSizef,
          ~height=tileSizef,
          env,
        );
      },
      pathfinderInstance.closedSet,
    );
  };

  drawSplashes(state, env);

  drawEnemies(state, dt, realdt, pathfinderInstance, env);

  let bodyColor =
    if (state.currentWeaponIndex == 0) {
      gunColor;
    } else {
      shotgunColor;
    };

  drawBullets(state.bullets, dt, ~color=bodyColor, env);

  drawLevelName(state, env);

  drawBestScore(state, env);

  if (state.deathTime > 0.) {
    drawDeathMessage(state, env);
  };

  Draw.popMatrix(env);

  /* Draw the aim line */
  drawAimLine(state, mx, my, playerXScreenf, playerYScreenf, env);

  /* Draw the player */
  drawGuy((playerXScreenf, playerYScreenf), ~bodyColor, state.time, env);

  let hasArmor = List.length(state.currentPowerups) > 0;
  if (hasArmor) {
    let armorColor =
      switch (List.hd(state.currentPowerups)) {
      | {kind: Armor} => Utils.color(55, 55, 55, 255)
      };

    Draw.fill(armorColor, env);
    for (i in 0 to 6) {
      let r = 28.;
      let angle =
        10. *. state.time +. float_of_int(i) *. 2. *. Constants.pi /. 6.;
      let x = r *. cos(angle) +. playerXScreenf;
      let y = r *. sin(angle) +. playerYScreenf;
      Draw.ellipsef(~center=(x, y), ~radx=4., ~rady=6., env);
    };
  };

  drawScore(state, env);

  /*Draw.text(~body, ~pos=(50, 100), env);

  Draw.text(
    ~body=sp("%f - %f", cosUpperBound, cosLowerBound),
    ~pos=(50, 150),
    env,
  );*/

  if (state.health <= 0) {
    let (startPosX, startPosY, grid, enemies) = getLevel(state.currentLevel);

    {
      ...state,
      time: 0.,
      timeSinceLastSpawned: 0.,
      score: 0,
      deathTime: 3.,
      x: float_of_int(startPosX) *. tileSizef +. tileSizef /. 2.,
      y: float_of_int(startPosY) *. tileSizef +. tileSizef /. 2.,
      realTime: 0.,
      aim: Nothing,
      health: 100,
      enemies,
      bullets: [],
      splashes: [],
      prevMouseState: false,
      currentWeaponIndex: 0,
      grid,
      touches: [],
      didTap: false,
      currentPowerups: [],
    };
  } else {
    {...state, prevMouseState: Env.mousePressed(env), didTap: false};
  };
};

let touchesMoved = (state, env) =>
  switch (state.aim) {
  | Aiming({x: sx, y: sy, points} as aiming) =>
    let (mx, my, _mx2, _my2, numOfTouches) = getTouchPositions(env);
    let (dx, dy) = (sx -. mx, sy -. my);
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
      aim: Aiming({...aiming, x: sx, y: sy, points: [(mx, my), ...points]}),
    };
  | _ => state
  };

let touchesBegan = (state, env) => {
  let touches =
    List.map(touch => (touch, state.realTime), Env.changedTouches(env));
  let touches =
    switch (touches) {
    | [] =>
      let (x, y) = Env.mouse(env);
      [
        (
          {Events.hash: 1., x: float_of_int(x), y: float_of_int(y)},
          state.realTime,
        ),
      ];
    | touches => touches
    };
  {...state, touches: state.touches @ touches};
};

let touchesEnded = (state, env) => {
  let touches =
    switch (Env.changedTouches(env)) {
    | [] =>
      let (x, y) = Env.mouse(env);
      [{Events.hash: 1., x: float_of_int(x), y: float_of_int(y)}];
    | touches => touches
    };
  let (didTap, touches) =
    List.fold_left(
      ((didTap, newTouches), (touch, prevTime)) =>
        if (didTap) {
          (didTap, [(touch, prevTime), ...newTouches]);
        } else if (List.mem(touch, touches)) {
          (state.realTime -. prevTime < 0.2, newTouches);
        } else {
          (didTap, [(touch, prevTime), ...newTouches]);
        },
      (false, []),
      state.touches,
    );
  {...state, didTap, touches};
};

/* @Hack UUUGH we have to do this runtime check because otherwise the mouse handler and the touch handles will BOTH fire on iOS. */
let run =
  if (Reprocessing.target == "native-ios") {
    (size, assetDir) =>
      Reprocessing.run(
        ~setup=setup(size, assetDir),
        ~draw,
        ~touchesBegan,
        ~touchesMoved,
        ~touchesEnded,
        (),
      );
  } else {
    (size, assetDir) =>
      Reprocessing.run(
        ~setup=setup(size, assetDir),
        ~draw,
        ~mouseDown=touchesBegan,
        ~mouseUp=touchesEnded,
        ~touchesBegan,
        ~touchesMoved,
        ~touchesEnded,
        (),
      );
  };
