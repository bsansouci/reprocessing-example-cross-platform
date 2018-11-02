let _DEBUG = 1;

let _NUMBER_OF_EXPERIMENTS = 1;

open Reprocessing;
open Common;

let drawWithRotation =
    (img, ~pos as (x, y), ~height, ~width, ~rot, ~scale=1.0, env) => {
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.scale(~x=globalScale, ~y=globalScale, env);
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

/* @Incomplete @Optimization We could crank this up to be the size of a screen, and use it to order the enemies so that we first pathfind the ones that are close to you, then the other ones.

         Ben – October 28th 2018
 */
let enemyGridScale = 3;
let enemyGridWidth = gridWidth / enemyGridScale;
let enemyGridHeight = gridHeight / enemyGridScale;

let makeEnemyGrid = state =>
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

let getEnemiesInRegion = (enemyGrid, x, y) => {
  let getCell = (grid, x, y) =>
    if (x >= Array.length(grid)
        || x < 0
        || y >= Array.length(grid[0])
        || y < 0) {
      [];
    } else {
      grid[x][y];
    };

  let (x, y) = (x / enemyGridScale, y / enemyGridScale);
  let neighbors = [
    (x - 1, y),
    (x + 1, y),
    (x, y - 1),
    (x, y + 1),
    (x, y),
  ];
  let enemies = List.map(((x, y)) => getCell(enemyGrid, x, y), neighbors);
  List.concat(enemies);
};

let isTileHorizontal = (state, cellX, cellY) => {
  let {kind: kindLeft} = getCell(state.grid, (cellX - 1, cellY));
  let {kind: kindRight} = getCell(state.grid, (cellX + 1, cellY));
  let {kind: kindTop} = getCell(state.grid, (cellX, cellY - 1));
  let {kind: kindBottom} = getCell(state.grid, (cellX, cellY + 1));
  
  (kindLeft == Wall || kindRight == Wall) && kindTop != Wall && kindBottom != Wall;
};

let isTileVertical = (state, cellX, cellY) => {
  let {kind: kindLeft} = getCell(state.grid, (cellX - 1, cellY));
  let {kind: kindRight} = getCell(state.grid, (cellX + 1, cellY));
  let {kind: kindTop} = getCell(state.grid, (cellX, cellY - 1));
  let {kind: kindBottom} = getCell(state.grid, (cellX, cellY + 1));
  
  (kindTop == Wall || kindBottom == Wall) && kindLeft != Wall && kindRight != Wall
};

let bulletCollidesWithEnemies = ({x, y}: bulletT, enemies) => {
  let rec loop = enemiesRemaining =>
    switch (enemiesRemaining) {
    | [] => None
    | [{id, isDead, pos: {x: sx, y: sy}} as enemy, ...restOfPineapples] =>
      let enemyBoundingCircle = 30.;
      if (!isDead
          && Utils.distf(~p1=(sx, sy), ~p2=(x, y)) < enemyBoundingCircle) {
        Some(enemy);
      } else {
        loop(restOfPineapples);
      };
    };
  loop(enemies);
};

/* Better collision detection for bullets which take into account whether a wall is a vertical or 
   horizontal line or a cross. */
let bulletCollidesWithWall = (state, {x, y}: bulletT, (cellX, cellY)) => {
  let {collision} = getCell(state.grid, (cellX, cellY));
  if (collision) {
    let (tileCenterX, tileCenterY) = (float_of_int(cellX) *. tileSizef +. tileSizef /. 2., float_of_int(cellY) *. tileSizef +. tileSizef /. 2.);
    let (halfWidth, halfHeight) = if (isTileVertical(state, cellX, cellY)) {
      (tileSizef /. 4., tileSizef /. 2.)
    } else if (isTileHorizontal(state, cellX, cellY)) {
      (tileSizef /. 2., tileSizef /. 4.)
    } else {
      (tileSizef /. 2., tileSizef /. 2.)
    };
    
    x >= tileCenterX -. halfWidth && x <= tileCenterX +. halfWidth && y >= tileCenterY -. halfHeight && y <= tileCenterY +. halfHeight;
  } else {
    false
  }
};

let loadAssetMap = (env, possibleFruits) => {
  let files = [
    ("./assets/splash_red_small.png", "splash_red"),
    ("./assets/all_assets.png", "all_assets"),
    ("./assets/cross.png", "wall_cross"),
    ("./assets/vertical_line.png", "wall_vertical"),
    ("./assets/horizontal_line.png", "wall_horizontal"),
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
      (dx, dy, 0.97),
      getVisibleEnemies(state, env),
    );

  let distFromPlayerToEnemy = sqrt(newdx *. newdx +. newdy *. newdy);
  if (distFromPlayerToEnemy > minFruitDistanceForAimAssist) {
    (newdx, newdy, cosangle);
  } else {
    (dx, dy, 0.97);
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
    let (cellX, cellY) = (float_of_int(cellX), float_of_int(cellY));
    let neighbors = [|
      (cellX +. 1., cellY),
      (cellX -. 1., cellY),
      (cellX +. 1., cellY +. 1.),
      (cellX -. 1., cellY +. 1.),
      (cellX +. 1., cellY -. 1.),
      (cellX -. 1., cellY -. 1.),
      (cellX, cellY +. 1.),
      (cellX, cellY -. 1.),
    |];

    let (closestCellX, closestCellY, _) =
      Array.fold_left(
        ((closestCellX, closestCellY, closestDist), (neighborCellX, neighborCellY)) => {
          let mag =
            Utils.distf(
              ~p1=(x, y),
              ~p2=(neighborCellX *. tileSizef +. tileSizef /. 2., neighborCellY *. tileSizef +. tileSizef /. 2.),
            );
          if (mag < closestDist
              && !getCell(grid, (int_of_float(neighborCellX), int_of_float(neighborCellY))).collision) {
            (neighborCellX, neighborCellY, mag);
          } else {
            (closestCellX, closestCellY, closestDist);
          };
        },
        (cellX, cellY, 9999999.),
        neighbors,
      );
    (int_of_float(closestCellX), int_of_float(closestCellY));
  } else {
    (cellX, cellY);
  };
};

let moveBullets = (state, dt, ~enemyGrid, ~removeIfTooFar=false, env) =>
  List.fold_left(
    (state, {x, y, vx, vy} as bullet: bulletT) =>
      switch (
        bulletCollidesWithEnemies(
          bullet,
          getEnemiesInRegion(
            enemyGrid,
            int_of_float(floor(x /. tileSizef)),
            int_of_float(floor(y /. tileSizef)),
          ),
        ),
        bulletCollidesWithWall(state, bullet, (int_of_float(floor(x /. tileSizef)),
            int_of_float(floor(y /. tileSizef)))),
      ) {
      | (None, false) =>
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
      | (_, true) => state
      | (Some(enemy), _) =>
        Env.playSound(
          state.sounds.enemyDeathSound,
          ~volume=1.0,
          ~loop=false,
          env,
        );

        /* @Mutation */
        enemy.isDead = true;
        {
          ...state,
          score: state.score + 1,
          splashes: [
            {
              x: enemy.pos.x,
              y: enemy.pos.y,
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

let moveEnemiesAndAttack = (state, dt, enemyGrid, env) => {
  let (playerCellX, playerCellY) = (
    int_of_float(floor(state.x /. tileSizef)),
    int_of_float(floor(state.y /. tileSizef)),
  );

  /* @Optimize with mutation. Well... This is a bit more annoying than I thought because we'd need to be able to quickly slice the list of enemies for enemies that are unreachable. 
  So I'm leaving this as is for now. */
  let ranOutOfTime = ref(false);
  let enemies =
    List.fold_left(
      (enemies, {pos: {x, y}, pathLastUpdatedTime, isDead, path} as enemy) =>
        if (ranOutOfTime^ || isDead) {
          [enemy, ...enemies];
        } else {
          let (cellX, cellY) = (
            int_of_float(floor(x /. tileSizef)),
            int_of_float(floor(y /. tileSizef)),
          );
          let (cellX, cellY) =
            findClosestNonCollidableCell(state.grid, x, y, cellX, cellY);
          if (pathLastUpdatedTime < state.pathfinderInstanceTime) {
            switch (
              Pathfinder.pathfind(
                state.pathfinderInstance,
                (playerCellX, playerCellY),
                (cellX, cellY),
              )
            ) {
            | None => 
              ranOutOfTime := true;
              [enemy, ...enemies]
            | Some([]) => enemies
            | Some(path) => 
            let now = Unix.gettimeofday();
            [
                {...enemy, path, pathLastUpdatedTime: now},
                ...enemies,
              ]
            };
          } else {
            [enemy, ...enemies];
          };
        },
      [],
      state.enemies,
    );
  
  let newBullets = ref(state.enemyBullets);
  let newBombs = ref(state.enemyBombs);
  let enemies =
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
          path,
          pathLastUpdatedTime,
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
          let moveInDirection =
              (enemy, mag, playerDirectionX, playerDirectionY) => {
            let vx = playerDirectionX /. mag *. speed;
            let vy = playerDirectionY /. mag *. speed;

            let enemiesInArea = getEnemiesInRegion(enemyGrid, cellX, cellY);
            let (resolvedVx, resolvedVy, forcedToMove) =
              pushEnemyIfNecessary(
                ~state,
                ~enemy,
                ~dt,
                ~enemiesInArea,
                vx,
                vy,
              );

            let (resolvedVx, resolvedVy) =
              switch (
                resolveCollision(~state, ~dt, x, y, resolvedVx, resolvedVy)
              ) {
              | None => (0., 0.)
              | Some(direction) => direction
              };

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
          
          let followPath = (path, x, y, cellX, cellY) => {
            switch (path) {
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
              let vx = destRelX /. mag *. speed;
              let vy = destRelY /. mag *. speed;

              let enemiesInArea = getEnemiesInRegion(enemyGrid, cellX, cellY);
              let (resolvedVx, resolvedVy, forcedToMove) =
                pushEnemyIfNecessary(
                  ~state,
                  ~enemy,
                  ~dt,
                  ~enemiesInArea,
                  vx,
                  vy,
                );
              (resolvedVx, resolvedVy, forcedToMove)
            | _ => (0., 0., false)
            }
          };
          
          let moveAwayFromPlayer = (enemy, mag, playerDirectionX, playerDirectionY, cellX, cellY, minDistanceBetweenPlayerAndShooter) => {
              if (mag < minDistanceBetweenPlayerAndShooter) {
                moveInDirection(
                  enemy,
                  mag,
                  -. playerDirectionX,
                  -. playerDirectionY,
                );
              } else {
                let vx = enemy.direction.x
                    +. Utils.noise(enemy.pos.x, enemy.pos.y, state.time)
                    *. 2.
                    -. 1.;
                let vy = enemy.direction.y
                    +. Utils.noise(enemy.pos.x, enemy.pos.y, state.time)
                    *. 2.
                    -. 1.;
                let enemiesInArea = getEnemiesInRegion(enemyGrid, cellX, cellY);
                let (resolvedVx, resolvedVy, forcedToMove) =
                  pushEnemyIfNecessary(
                    ~state,
                    ~enemy,
                    ~dt,
                    ~enemiesInArea,
                    vx,
                    vy,
                  );
                {
                  ...enemy,
                  direction: {x: resolvedVx, y: resolvedVy},
                  pos: {
                    x: enemy.pos.x +. resolvedVx *. dt,
                    y: enemy.pos.y +. resolvedVy *. dt,
                  },
                };
              };
          };
          
          let moveAlongPath = () => {
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
              moveInDirection(
                defaultEnemyUpdates,
                mag,
                playerDirectionX,
                playerDirectionY,
              )
            | [_, next, nextnext, ...rest] =>
              let (resolvedVx, resolvedVy, forcedToMove) = followPath(path, x, y, cellX, cellY);
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
            }
          };
          
          switch (kind) {
          | Melee => moveAlongPath()
          | Bomber => 
          if (mag < enemy.weaponRange) {
              let enemy =
                if (timeUntilNextAttack <= 0.) {
                  /* Gets the last cell from a path towards the player, and shoot there instead of where the enemy currently is. It's a nice way to disincentivize the player from moving towards the bomber guys. */
                  /*let rec getTail = (li) => {
                    switch (li) {
                    | [] => (state.x, state.y)
                    | [_] => (state.x, state.y)
                    | [(x, y), _] => (float_of_int(x) *. tileSizef +. tileSizef /. 2., float_of_int(y) *. tileSizef +. tileSizef /. 2.)
                    | [_, ...rest] => getTail(rest)
                    }
                  };
                  let (x, y) = getTail(path);*/
                  newBombs := [{
                        x: state.x -. playerDirectionX /. mag *. enemy.bulletSpeed *. 1.5,
                        y: state.y -. playerDirectionY /. mag *. enemy.bulletSpeed *. 1.5,
                        vx: playerDirectionX /. mag *. enemy.bulletSpeed,
                        vy: playerDirectionY /. mag *. enemy.bulletSpeed,
                        timeRemaining: defaultEnemyUpdates.bulletLifeSpan,
                      }, ...newBombs^];
                  {
                    ...defaultEnemyUpdates,
                    timeUntilNextAttack: bombTime +. 1.,
                    direction: {x: 0., y: 0.}
                  };
                } else {
                  defaultEnemyUpdates;
                };
              
              moveAwayFromPlayer(enemy, mag, playerDirectionX, playerDirectionY, cellX, cellY, 200.);
          } else {
            moveAlongPath()
          }
          
          | Shooter =>
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
                  newBullets := [{
                        x,
                        y,
                        vx: playerDirectionX /. mag *. enemy.bulletSpeed,
                        vy: playerDirectionY /. mag *. enemy.bulletSpeed,
                        timeRemaining: defaultEnemyUpdates.bulletLifeSpan,
                      }, ...newBullets^];
                  {
                    ...defaultEnemyUpdates,
                    timeUntilNextAttack: 1.,
                  };
                } else {
                  defaultEnemyUpdates;
                };

                moveAwayFromPlayer(enemy, mag, playerDirectionX, playerDirectionY, cellX, cellY, 160.);
            } else {
              moveAlongPath()
            };
          };
        },
      enemies,
    );

  /* Manage melee attacking */
  let (health, currentPowerups) =
    List.fold_left(
      (
        (health, currentPowerups),
        {pos: {x, y}, isDead, timeUntilNextAttack, kind} as enemy,
      ) =>
        switch (kind) {
        | Melee =>
          if (isDead) {
            (health, currentPowerups);
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
                currentPowerups,
              );
            } else {
              (health, currentPowerups);
            };
          } else {
            (health, currentPowerups);
          }
        | Shooter =>
          (health, currentPowerups);
        | Bomber => (health, currentPowerups)
        },
      (state.health, state.currentPowerups),
      enemies,
    );
  
  /* Manage enemyBullets */
  let (health, enemyBullets, currentPowerups) =
            List.fold_left(
              (
                (health, enemyBullets, currentPowerups),
                {x, y, vx, vy, timeRemaining} as bullet: bulletT,
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
                    usedAPowerup ? health : health - 101,
                    enemyBullets,
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
                  | None => (health, enemyBullets, currentPowerups)
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
                        ...enemyBullets,
                      ],
                      currentPowerups,
                    )
                  };
                };
              },
              (health, [], currentPowerups),
              newBullets^,
            );
            
  let (health, enemyBombs, currentPowerups) = List.fold_left(((health, bombs, currentPowerups), {timeRemaining, x, y, vx, vy} as bomb) => {
    if (timeRemaining <= 0.) {
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
                if (mag < bombRadius) {
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
                  (usedAPowerup ? health : 0, bombs, currentPowerups)
                } else {
                  (health, bombs, currentPowerups)
                }
    } else {
      
      (
                      health,
                      [
                        {
                          x: x +. vx *. dt,
                          y: y +. vy *. dt,
                          vx,
                          vy,
                          timeRemaining: bomb.timeRemaining -. dt,
                        },
                        ...bombs,
                      ],
                      currentPowerups,
                    )
      
      /*(health, [{...bomb, timeRemaining: bomb.timeRemaining -. dt}, ...bombs], currentPowerups)*/
    }
  }, (health, [], currentPowerups), newBombs^);
  let enemies = List.rev(enemies);
  let enemyBombs = List.rev(enemyBombs);
  {...state, enemies, enemyBullets, enemyBombs, health, currentPowerups};
};

let spawnPowerups = (state, playerXScreenScaledf, playerYScreenScaledf, env) => {
  if (state.numberOfPowerups < maxNumberOfPowerups) {
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
    setCell(
      state.grid,
      (x^, y^),
      {collision: false, kind: Powerup({time: 1., kind: Armor})},
    );
  };

  {...state, numberOfPowerups: state.numberOfPowerups + 1};
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

  let (kind, bulletSpeed, bulletDamage, weaponRange, bulletLifeSpan) = {
    /*let r = Utils.randomf(~min=0., ~max=1.);*/
    let r = 0.25;
    if (r < 0.2) {
      (Melee, 0., 0, 0., 0.2)
    } else if (r < 0.3) {
      (Bomber, 100., 0, 320., 1.)
    } else {
      (Shooter, 300., 100, 240., 0.2)
    }
  };

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
      kind,
      bulletSpeed,
      bulletDamage,
      weaponRange,
      isDead: false,
      bulletLifeSpan,
      pathLastUpdatedTime: 0.,
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
    | (Aiming({x: sx, y: sy, points} as aiming), true) => 
    switch (points) {
    | [] => state
    | [(x, _), ...rest] => {...state, lastAimDirectionX: x -. sx == 0. ? state.lastAimDirectionX : x -. sx}
    };
    | (Nothing, false) => state
    | (Nothing, true) => {
            ...state,
            aim: Aiming({x: mx, y: my, points: [], startAimTime: state.realTime}),
          }
    | (Aiming({x: sx, y: sy, points, startAimTime}), false) =>
      if (!didTapOnSwapButton) {
        let currentWeapon = state.weapons[state.currentWeaponIndex];
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
              if (currentWeapon.kind == ShootsBehindYou) {
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
                      lastAimDirectionX: -. dirX,
                    };
        } else {
                  switch (points) {
    | [] => {...state, aim: Nothing}
    | [(x, _), ...rest] => {...state, aim: Nothing, lastAimDirectionX: x -. sx == 0. ? state.lastAimDirectionX : x -. sx}
    };
        };
      } else {
        state
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
        {
          ...state,
          currentPowerups: [p, ...state.currentPowerups],
          numberOfPowerups: state.numberOfPowerups - 1,
        };
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

let drawPowerup = (state, x, y, kind, env) => {
  let img = AssetMap.find("all_assets", state.assetMap);
  let size = tileSizef;
  Draw.subImagef(img, ~pos=(x, y), ~width=size, ~height=size, ~texPos=(0, 60), ~texWidth=64, ~texHeight=64, env);
};

let drawBackground = (state, playerXScreenf, playerYScreenf, env) => {
  let paddingForSomeGodDamReason = 10.;
  let left = int_of_float((state.x -. playerXScreenf) /. tileSizef) - 1;
  let right = int_of_float((state.x +. playerXScreenf) /. tileSizef) + 1;
  let top = int_of_float((state.y -. playerYScreenf) /. tileSizef) - 2;
  let bottom = int_of_float((state.y +. playerYScreenf) /. tileSizef) + 2;
  for (cellX in left to right) {
    for (cellY in top to bottom) {
      let {kind, collision} = getCell(state.grid, (cellX, cellY));
      let (x, y) = (
        float_of_int(cellX) *. tileSizef,
        float_of_int(cellY) *. tileSizef,
      );

      switch (kind) {
      | Door =>
        if (collision) {
          Draw.fill(Utils.color(~r=205, ~g=133, ~b=63, ~a=255), env);
        } else {
          Draw.fill(Utils.color(~r=255, ~g=183, ~b=113, ~a=255), env);
        };
        Draw.noStroke(env);
        let leftCell = getCell(state.grid, (cellX - 1, cellY));
        let topCell = getCell(state.grid, (cellX, cellY - 1));
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
          Draw.rectf(~pos=(x, y), ~width=tileSizef, ~height=tileSizef, env);
        };
      | Floor =>
        Draw.fill(Constants.white, env);
        Draw.noStroke(env);

        /*Draw.stroke(Constants.black, env);
          Draw.strokeWeight(1, env);*/
        Draw.rectf(~pos=(x, y), ~width=tileSizef, ~height=tileSizef, env);
      | Wall =>
        Draw.pushMatrix(env);
        Draw.translate(~x, ~y, env);
        if (isTileHorizontal(state, cellX, cellY)) {
          Draw.translate(~x=0., ~y=tileSizef /. (4.), env);
          Draw.imagef(AssetMap.find("wall_horizontal", state.assetMap), ~pos=(0., 0.), ~height=tileSizef /. 2., ~width=tileSizef, env);
        } else if (isTileVertical(state, cellX, cellY)) {
          Draw.translate(~x=tileSizef /. (4.), ~y=0., env);
          Draw.imagef(AssetMap.find("wall_vertical", state.assetMap), ~pos=(0., 0.), ~height=tileSizef, ~width=tileSizef /. 2., env);
        } else {
          Draw.imagef(AssetMap.find("wall_cross", state.assetMap), ~pos=(0., 0.), ~height=tileSizef, ~width=tileSizef, env);
        };
        
        Draw.popMatrix(env);
        
        /*Draw.fill(Utils.color(~r=124, ~g=124, ~b=124, ~a=255), env);
        Draw.noStroke(env);
        Draw.rectf(~pos=(x, y), ~width=tileSizef, ~height=tileSizef, env);*/
      | Powerup({kind}) => drawPowerup(state, x, y, kind, env)
      };
    };
  };
};

let drawDeathMessage = (state, env) => {
  Draw.pushStyle(env);
  let body = "go fight";
  let textWidth = Draw.textWidth(~body, ~font=state.font, env);
  let alpha =
    if (state.deathTime > deathMessageMaxTime /. 2.) {
      int_of_float(
        Utils.remapf(
          ~value=state.deathTime,
          ~low1=deathMessageMaxTime,
          ~high1=deathMessageMaxTime /. 2.,
          ~low2=0.,
          ~high2=255.,
        ),
      );
    } else {
      int_of_float(
        Utils.remapf(
          ~value=state.deathTime,
          ~low1=deathMessageMaxTime /. 2.,
          ~high1=0.,
          ~low2=255.,
          ~high2=0.,
        ),
      );
    };
  Draw.tint(Utils.color(~r=255, ~g=20, ~b=50, ~a=alpha), env);
  Draw.text(
    ~body, ~font=state.font,
    ~pos=(- textWidth / 2,  -100),
    env,
  );
  Draw.popStyle(env);
};

let drawSplashes = (state, env) => {
  Draw.pushStyle(env);
  Draw.tint(Utils.color(~r=255, ~g=255, ~b=255, ~a=150), env);
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

let gunColor1 = Utils.color(~r=115, ~g=155, ~b=255, ~a=255);
let gunColor2 = Utils.color(~r=80, ~g=120, ~b=220, ~a=255);
let shotgunColor1 = Utils.color(~r=100, ~g=220, ~b=120, ~a=255);
let shotgunColor2 = Utils.color(~r=40, ~g=160, ~b=60, ~a=255);

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

let drawBombs = (bombs, dt, env) => {
  Draw.stroke(Constants.black, env);
  Draw.strokeWeight(2, env);
  List.iter(({x, y, vx, vy, timeRemaining}: bulletT) => {
    Draw.fill(Utils.color(~r=255, ~g=60, ~b=80, ~a=100), env);
    Draw.ellipsef(~center=(x, y), ~radx=bombRadius, ~rady=bombRadius, env);
    
    Draw.fill(Utils.color(~r=255, ~g=60, ~b=80, ~a=255), env);
    let percentage = (bombTime -. timeRemaining) /. bombTime;
    Draw.ellipsef(~center=(x, y), ~radx=percentage *. bombRadius, ~rady=percentage *. bombRadius, env);
  }, bombs);
};

let drawScore = (state, env) => {
  let body = sp("%d", state.score);
  let textWidth = Draw.textWidth(~body, ~font=state.font, env);
  let (x, y) = (Env.width(env) - textWidth - 24, 42);
  Draw.pushStyle(env);
  Draw.strokeCap(Round, env);
  Draw.strokeWeight(8, env);
  Draw.stroke(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.fill(Utils.color(~r=255, ~g=255, ~b=255, ~a=255), env);
  Draw.rect(~pos=(x - 8, y - 28), ~width=textWidth + 16, ~height=34, env);
  Draw.tint(Utils.color(~r=0, ~g=0, ~b=0, ~a=150), env);
  Draw.text(~pos=(x, y), ~body, ~font=state.font, env);
  Draw.popStyle(env);
};

let drawBestScore = (state, env) => {
  let body = sp("Best score: %d", state.maxScore);
  Draw.pushStyle(env);
  Draw.tint(Utils.color(~r=0, ~g=0, ~b=0, ~a=150), env);
  Draw.text(
    ~pos=(
       - Draw.textWidth(~body, ~font=state.font, env) / 2,
      -64,
    ), ~font=state.font,
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
    ~pos=(int_of_float(x) - Draw.textWidth(~body, ~font=state.font, env) / 2, (-88)),
    ~body, ~font=state.font,
    env,
  );
  Draw.popStyle(env);
};

let drawGuy =
    (
      state,
      (px, py),
      dirX,
      ~kind,
      ~bodyColor,
      ~feetColor,
      time,
      env,
    ) => {
  let animationSpeed = 30.;
  let guyW = 30.;
  let guyH = 30.;
  let footW = 30. /. 2.;
  let footH = 18. /. 2.;
  let headW = 18.;
  let headH = 18.;
  
  let img = AssetMap.find("all_assets", state.assetMap);
  
  Draw.noStroke(env);
  Draw.tint(bodyColor, env);
  
  /* Body */
  let pos = (px -. guyW /. 2., py -. guyH /. 2.);
  Draw.subImagef(img, ~pos, ~width=guyW, ~height=guyH, ~texPos=(122, 0), ~texWidth=64, ~texHeight=64, env);
  
  /* Gun */
  let gunW = guyW +. 6.;
  let gunH = 16.;
    let (gunW, x) = if (dirX > 0.) {
      (-. gunW, px +. gunW /. 2.)
    } else {
      (gunW, px -. gunW /. 2.)
    };
    let y = 
        py -. gunH /. 2. +. sin(time *. animationSpeed +. 3.);
  switch (kind) {
  | Shooter => 
  let x = if (dirX > 0.) { x +. 2.} else { x };
    Draw.subImagef(img, ~pos=(x, y), ~width=gunW, ~height=gunH, ~texPos=(36, 3), ~texWidth=84, ~texHeight=31, env);
  | Melee => 
    let gunW = if (dirX > 0.) { gunW +. 4. } else { gunW -. 4. };
    let x = if (dirX > 0.) { x } else { x +. 1.};
    let y = y +. 2.;
    Draw.subImagef(img, ~pos=(x, y), ~width=gunW, ~height=gunH -. 4., ~texPos=(72, 66), ~texWidth=85, ~texHeight=31, env);
  | Bomber =>
    Draw.subImagef(img, ~pos=(x, y), ~width=gunW, ~height=gunH, ~texPos=(73, 108), ~texWidth=88, ~texHeight=36, env);
  }
  
  Draw.tint(feetColor, env);
  /* Legs */
  let pos = (
      px -. guyW /. 4. +. sin(time *. animationSpeed +. 2.) *. footW /. 8. -. footW /. 2.,
      py
      +. guyH /. 2.
      -. 4. -. footH /. 2.
      +. min(cos(time *. animationSpeed +. 2.) *. footH /. 6., 0.5),
    );
  
  Draw.subImagef(img, ~pos, ~width=footW, ~height=footH, ~texPos=(4, 36), ~texWidth=28, ~texHeight=18, env);
  
  let pos = 
  (
      px +. guyW /. 4. +. sin(time *. animationSpeed) *. footW /. 8. -. footW /. 2.,
      py
      +. guyH /. 2.
      -. 4. -. footH /. 2.
      +. min(cos(time *. animationSpeed) *. footH /. 6., 0.5),
    );
  Draw.subImagef(img, ~pos, ~width=footW, ~height=footH, ~texPos=(4, 36), ~texWidth=28, ~texHeight=18, env);
  
  /* Head */
  let (headW) = if (dirX > 0.) {
    (-. headW)
  } else {
    (headW)
  };
  Draw.noTint(env);
  let pos = (
      px -. headW /. 2.,
      py -. guyH /. 2. -. headH /. 2. +. sin(time *. animationSpeed) /. 4.,
    );
  Draw.subImagef(img, ~pos, ~width=headW, ~height=headH, ~texPos=(0, 0), ~texWidth=34, ~texHeight=34, env);
};

let drawEnemies = (state, dt, realdt, env) => {
  let (playerCellX, playerCellY) = (
    int_of_float(floor(state.x /. tileSizef)),
    int_of_float(floor(state.y /. tileSizef)),
  );

  let color = Utils.color(~r=255, ~g=60, ~b=80, ~a=255);
  List.iter(
    ({pos: {x, y}, kind, isDead, direction, speed, path}) => {
      if (!isDead) {
          drawGuy(state, (x, y), state.x -. x, ~kind, ~bodyColor=color, ~feetColor=Utils.color(~r=200, ~g=10, ~b=40, ~a=255), state.time, env);
      };

      if (state.experiment == _DEBUG) {
        let (cellX, cellY) = (
          int_of_float(floor(x /. tileSizef)),
          int_of_float(floor(y /. tileSizef)),
        );
        if (cellX >= 0 && cellX < gridWidth && cellY >= 0 && cellY < gridHeight) {
          let cell = getCell(state.grid, (cellX, cellY));
          if (!cell.collision && !isDead) {
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
  
  drawBullets(state.enemyBullets, dt, ~color, ~bulletWidth=8., ~strokeWeight=2, env);
  
  drawBombs(state.enemyBombs, dt, env);

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

let drawArmor = (state, playerXScreenf, playerYScreenf, env) => {
  let hasArmor = List.length(state.currentPowerups) > 0;
  if (hasArmor) {
    let armorColor =
      switch (List.hd(state.currentPowerups)) {
      | {kind: Armor} => Utils.color(~r=80, ~g=55, ~b=100, ~a=255)
      };
    
    Draw.noFill(env);
    Draw.stroke(armorColor, env);
    Draw.strokeWeight(2, env);
    for (i in 0 to 5) {
      let r = 26.;
      let angle =
        10. *. state.time +. float_of_int(i) *. 2. *. Constants.pi /. 6.;
      Draw.arcf(~center=(playerXScreenf, playerYScreenf -. 2.), ~radx=r, ~rady=r, ~start=angle -. 0.4, ~stop=angle +. 0.4, ~isOpen=true, ~isPie=false, env);
    };
  };
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
    enemyBullets: [],
    enemyBombs: [],
    splashes: [],
    experiment: 0,
    prevMouseState: false,
    sounds: {
      enemyDeathSound: Env.loadSound(assetDir +/ "enemyDeathSound.wav", env),
      playerShotSound: Env.loadSound(assetDir +/ "playerShotSound.wav", env),
    },
    font: Draw.loadFont(~filename="assets" +/ "monaco_3x.fnt", env),
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
        bulletLifeSpan: 0.18,
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

    pathfinderInstance: Pathfinder.make(grid),
    pathfinderInstanceTime: 1.,
    numberOfPowerups: 0,
    startLocation: (startPosX, startPosY),
    lastAimDirectionX: 0.,
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

  let state =
    if (List.for_all(
          ({pathLastUpdatedTime, isDead}) =>
            pathLastUpdatedTime > state.pathfinderInstanceTime || isDead,
          state.enemies,
        )) {
      {
        ...state,
        pathfinderInstance: Pathfinder.make(state.grid),
        pathfinderInstanceTime: Unix.gettimeofday(),
      };
    } else {
      state;
    };

  let (mx, my, _mx2, _my2, numOfTouches) = getTouchPositions(env);

  let enemyGrid = makeEnemyGrid(state);

  /* Bullet collision detection and response */
  let state = moveBullets(state, dt, ~enemyGrid, ~removeIfTooFar=true, env);

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

  let state = moveEnemiesAndAttack(state, dt, enemyGrid, env);

  let (state, didTapOnSwapButton) =
    checkSwapWeaponButton(state, mx, my, playerXScreenf, playerYScreenf, env);

  let (devButtonX, devButtonY) = (60., 60.);

  let state =
    if (state.didTap && mx < devButtonX && my < devButtonY) {
      {
              ...state,
              experiment: (state.experiment + 1) mod (_NUMBER_OF_EXPERIMENTS + 1),
            }
              /*} else if (state.didTap
                         && mx > float_of_int(Env.width(env))
                         -. devButtonX
                         && my < devButtonY) {
                {
                  ...state,
                  health: (-1), /* Massive @Hack to move forward in levels */
                  currentLevel: (state.currentLevel + 1) mod Levels.numberOfLevels,
                };*/
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
    TupleSet.iter(
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
      state.pathfinderInstance.openSet,
    );

    TupleSet.iter(
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
      state.pathfinderInstance.closedSet,
    );
  };

  drawSplashes(state, env);
  
  drawEnemies(state, dt, realdt, env);
  
  let (bodyColor, feetColor) =
    if (state.currentWeaponIndex == 0) {
      (gunColor1, gunColor2);
    } else {
      (shotgunColor1, shotgunColor2);
    };

  drawBullets(state.bullets, dt, ~color=bodyColor, env);

  Draw.popMatrix(env);
  
  {
    Draw.pushMatrix(env);
    let (sx, sy) = state.startLocation;
    let (x, y) = cellToWorld((sx, sy));
    
    Draw.translate(~x=playerXScreenf, ~y=playerYScreenf, env);
    Draw.scale(~x=globalScale, ~y=globalScale, env);
    Draw.translate(~x=x -. state.x, ~y=y -. state.y, env);
    Draw.scale(~x=2. -. globalScale, ~y=2. -. globalScale, env);
    
    drawBestScore(state, env);

    if (state.deathTime > 0.) {
      drawDeathMessage(state, env);
    };
    
    Draw.popMatrix(env);
  };

  /* Draw the aim line */
  drawAimLine(state, mx, my, playerXScreenf, playerYScreenf, env);

  drawArmor(state, playerXScreenf, playerYScreenf, env);

  /* Draw the player */
  drawGuy(state, (playerXScreenf, playerYScreenf), state.lastAimDirectionX, ~kind=Shooter, ~bodyColor, ~feetColor, state.time, env);
  
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
      deathTime: deathMessageMaxTime,
      x: float_of_int(startPosX) *. tileSizef +. tileSizef /. 2.,
      y: float_of_int(startPosY) *. tileSizef +. tileSizef /. 2.,
      realTime: 0.,
      aim: Nothing,
      health: 100,
      enemies,
      bullets: [],
      enemyBullets: [],
      enemyBombs: [],
      splashes: [],
      prevMouseState: false,
      currentWeaponIndex: 0,
      grid,
      touches: [],
      didTap: false,
      currentPowerups: [],
      pathfinderInstance: Pathfinder.make(state.grid),
      pathfinderInstanceTime: 1.,
      startLocation: (startPosX, startPosY),
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
        ~mouseDragged=touchesMoved,
        ~touchesBegan,
        ~touchesMoved,
        ~touchesEnded,
        (),
      );
  };
