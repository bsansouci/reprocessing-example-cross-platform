let _DEBUG = true;

let _NUMBER_OF_EXPERIMENTS = 2;

/* The idea with this is that currently the rotation amount of the aiming line depends on how far
   the touch has moved from its start position.
   Say you start your touch at 300, 300, and you move straight up by 100pt.
               20pt
              +-----+
             |
             |  100pt
             |
             |
             + angle formed 0.19

   Now if you move right 20pt, the angle formed is atan(20/100) = 0.19
   If instead you had only moved up by 40pt, the angle formed would be atan(20/40) = 0.46
   The angle is smaller the longer your finger travels away from the start position. That's confusing
   because you can't easily see your start position nor can you properly estimate the relationship between the distance travel and the angle formed.
   Solution: cap the distance of the vector made! So it's predictable.
   */
let _EXPERIMENT_CAPPED_AIMING = 1;

/* Something I noticed while playing with the game was that it was kinda shitty to swipe quickly.
   The main reason is that we're using the last touch position as the end of the destination
   vector, but the last touch position is almost always off because of how I lift my finger off
   the screen. I lift my finger very roughly and it moves the destination target a lot.

   The simplest way to fix this is to make a quick gesture into a list of points, which we average
   based on recency (the newest points have more influence on the direction than the older ones,
   but doesn't fully influence the direction). We sum and multiply each points by i /
   numberOfPoints.

   This makes quick shots feel much better.
   */
let _EXPERIMENT_PATH_AIMING = 2;

open Reprocessing;

module AssetMap = Map.Make(String);

type aimT =
  | Nothing
  | Aiming(float, float, list((float, float)))
  | Moving((float, float), (float, float), float);

type bulletT = {
  x: float,
  y: float,
  vx: float,
  vy: float,
};

type splashT = {
  x: int,
  y: int,
  rotation: float,
  width: int,
  height: int,
};

type state = {
  x: float,
  y: float,
  aim: aimT,
  stuff: list((int, int)),
  splashes: list(splashT),
  bg: imageT,
  assetMap: AssetMap.t(imageT),
  bullets: list(bulletT),
  experiment: int,
  prevMouseState: bool,
};

let drawWithRotation = (img, ~pos as (x, y), ~height, ~width, ~rot, env) => {
  Draw.pushMatrix(env);
  Draw.translate(~x=float_of_int(x), ~y=float_of_int(y), env);
  Draw.rotate(rot, env);
  Draw.translate(
    ~x=float_of_int(width) /. (-2.),
    ~y=float_of_int(height) /. (-2.),
    env,
  );
  Draw.image(img, ~pos=(0, 0), ~height, ~width, env);
  Draw.popMatrix(env);
};

let dropLast = li => {
  let rec loop = (newList, tailOldList) =>
    switch (tailOldList) {
    | [] => assert(false)
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

let bulletCollidesWithPineapple = ({x, y}: bulletT, pineapples) => {
  let rec loop = (pinesapplesRemaining, index) =>
    switch (pinesapplesRemaining) {
    | [] => None
    | [(sx, sy), ...restOfPineapples] =>
      if (Utils.intersectRectCircle(
            ~rectPos=(float_of_int(sx), float_of_int(sy)),
            ~rectW=64.,
            ~rectH=64.,
            ~circlePos=(x, y),
            ~circleRad=4.,
          )) {
        Some(index);
      } else {
        loop(restOfPineapples, index + 1);
      }
    };
  loop(pineapples, 0);
};

let moveSpeed = 60.;
let moveTime = 0.3;
let possibleFruits = ["banana", "pineapple", "apple", "coconut", "orange"];

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
    ("./assets/splash_yellow_small.png", "splash_yellow"),
    ...files,
  ];
  List.fold_left(
    (assetMap, (filename, name)) =>
      AssetMap.add(name, Draw.loadImage(~filename, env), assetMap),
    AssetMap.empty,
    files,
  );
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
  | `Normal => Env.size(~width=500, ~height=500, env)
  };
  {
    x: 0.,
    y: 0.,
    aim: Nothing,
    stuff:
      initList(100, _ =>
        (
          Utils.random(~min=-1000, ~max=100),
          Utils.random(~min=-1000, ~max=1000),
        )
      ),
    bg: Draw.loadImage(~filename="./assets/background.png", env),
    assetMap: loadAssetMap(env, possibleFruits),
    bullets: [],
    splashes: [],
    experiment: 0,
    prevMouseState: false,
  };
};

let draw = (state, env) => {
  let dt = Env.deltaTime(env);
  let halfWindowW = Env.width(env) / 2;
  let halfWindowWf = float_of_int(halfWindowW);
  let halfWindowH = Env.height(env) / 2;
  let halfWindowHf = float_of_int(halfWindowH);
  let playerSize = 32;
  let playerSizef = 32.;
  /* Draw.image(state.bg, ~pos=(0, 0), env); */
  if (state.experiment == _EXPERIMENT_CAPPED_AIMING) {
    Draw.background(Utils.color(220, 120, 120, 255), env);
  } else if (state.experiment == _EXPERIMENT_PATH_AIMING) {
    Draw.background(Utils.color(140, 220, 140, 255), env);
  } else {
    Draw.background(Constants.white, env);
  };
  Draw.pushMatrix(env);
  Draw.translate(
    ~x=halfWindowWf -. playerSizef -. state.x,
    ~y=halfWindowHf -. playerSizef -. state.y,
    env,
  );

  List.iter(
    ({x, y, width, height, rotation}) =>
      drawWithRotation(
        AssetMap.find("splash_yellow", state.assetMap),
        ~pos=(x, y),
        ~width,
        ~height,
        ~rot=rotation,
        env,
      ),
    state.splashes,
  );

  List.iter(
    pos =>
      drawWithRotation(
        AssetMap.find("pineapple", state.assetMap),
        ~pos,
        ~width=54,
        ~height=74,
        ~rot=0.,
        env,
      ),
    state.stuff,
  );

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
  Draw.image(
    AssetMap.find("apple", state.assetMap),
    ~pos=(halfWindowW - playerSize, halfWindowH - playerSize),
    env,
  );

  let (mx, my) = Env.mouse(env);
  let (mx, my) = (float_of_int(mx), float_of_int(my));
  switch (state.aim, Env.mousePressed(env)) {
  | (Aiming(sx, sy, points), true) =>
    let dx = sx -. mx;
    let dy = sy -. my;

    let (dx, dy) =
      if (state.experiment == _EXPERIMENT_PATH_AIMING) {
        let len = List.length(points);
        let maxNumberOfPointsInSwipe = 10;
        if (len > maxNumberOfPointsInSwipe) {
          (dx, dy);
        } else {
          let ratio = 0.5;
          let (dx, dy) =
            List.fold_left(
              ((dx, dy), (x, y)) => (
                dx *. (1. -. ratio) +. (sx -. x) *. ratio,
                dy *. (1. -. ratio) +. (sy -. y) *. ratio,
              ),
              (0., 0.),
              points,
            );
          (dx, dy);
        };
      } else {
        (dx, dy);
      };

    let mag = sqrt(dx *. dx +. dy *. dy);
    if (mag > 20. || state.experiment == _EXPERIMENT_PATH_AIMING && mag > 4.) {
      let moveX = dx /. mag *. moveSpeed;
      let moveY = dy /. mag *. moveSpeed;
      Draw.pushStyle(env);
      Draw.strokeWeight(1, env);
      Draw.stroke(Constants.red, env);
      if (_DEBUG) {
        Draw.linef(~p1=(sx, sy), ~p2=(mx, my), env);
      };
      Draw.linef(
        ~p1=(halfWindowWf, halfWindowHf),
        ~p2=(halfWindowWf -. moveX *. 100., halfWindowHf -. moveY *. 100.),
        env,
      );
      Draw.popStyle(env);
      Draw.linef(
        ~p1=(halfWindowWf, halfWindowHf),
        ~p2=(halfWindowWf -. moveX, halfWindowHf -. moveY),
        env,
      );
    };
  | _ => ()
  };

  /* Bullet collision detection and response */
  let state =
    List.fold_left(
      (state, {x, y, vx, vy} as bullet: bulletT) =>
        switch (bulletCollidesWithPineapple(bullet, state.stuff)) {
        | None => {
            ...state,
            bullets: [{...bullet, x: x +. vx, y: y +. vy}, ...state.bullets],
          }
        | Some(index) =>
          switch (splitListAt(state.stuff, index)) {
          | (headList, [(px, py), ...restOfTail]) => {
              ...state,
              stuff: headList @ restOfTail,
              splashes: [
                {
                  x: px,
                  y: py,
                  width: Utils.random(~min=40, ~max=64),
                  height: Utils.random(~min=40, ~max=64),
                  rotation: Utils.randomf(~min=0., ~max=Constants.pi),
                },
                ...state.splashes,
              ],
            }
          | _ => assert(false)
          }
        },
      {...state, bullets: []},
      state.bullets,
    );

  let state =
    if (state.prevMouseState
        && !Env.mousePressed(env)
        && mx < 100.
        && my < 100.) {
      {
        ...state,
        experiment: (state.experiment + 1) mod (_NUMBER_OF_EXPERIMENTS + 1),
      };
    } else {
      switch (state.aim, Env.mousePressed(env)) {
      | (Aiming(sx, sy, points), true) =>
        let dx = sx -. mx;
        let dy = sy -. my;
        let mag = sqrt(dx *. dx +. dy *. dy);
        let (sx, sy) =
          if (state.experiment == _EXPERIMENT_CAPPED_AIMING && mag > 50.) {
            (mx +. dx /. mag *. 50., my +. dy /. mag *. 50.);
          } else {
            (sx, sy);
          };
        {...state, aim: Aiming(sx, sy, [(mx, my), ...points])};
      | (Nothing, false) => state
      | (Nothing, true) => {...state, aim: Aiming(mx, my, [])}
      | (Aiming(sx, sy, points), false) =>
        let dx = sx -. mx;
        let dy = sy -. my;

        let (dx, dy) =
          if (state.experiment == _EXPERIMENT_PATH_AIMING) {
            let len = List.length(points);
            let maxNumberOfPointsInSwipe = 10;
            if (len > maxNumberOfPointsInSwipe) {
              (dx, dy);
            } else {
              let ratio = 0.5;
              let (dx, dy) =
                List.fold_left(
                  ((dx, dy), (x, y)) => (
                    dx *. (1. -. ratio) +. (sx -. x) *. ratio,
                    dy *. (1. -. ratio) +. (sy -. y) *. ratio,
                  ),
                  (0., 0.),
                  points,
                );
              (dx, dy);
            };
          } else {
            (dx, dy);
          };

        let mag = sqrt(dx *. dx +. dy *. dy);
        if (mag > 20.
            || state.experiment == _EXPERIMENT_PATH_AIMING
            && mag > 4.) {
          let destX = state.x -. dx /. mag *. moveSpeed;
          let destY = state.y -. dy /. mag *. moveSpeed;
          {
            ...state,
            aim: Moving((state.x, state.y), (destX, destY), moveTime),
            bullets: [
              {
                x: state.x -. dx /. mag *. playerSizef +. playerSizef,
                y: state.y -. dy /. mag *. playerSizef +. playerSizef,
                vx: -. dx /. mag *. 10.,
                vy: -. dy /. mag *. 10.,
              },
              ...state.bullets,
            ],
          };
        } else {
          {...state, aim: Nothing};
        };
      | (Moving(_prev, (destX, destY), time), _) when time <= 0. => {
          ...state,
          aim: Nothing,
          x: destX,
          y: destY,
        }
      | (
          Moving((startX, startY) as start, (destX, destY) as dest, time),
          _,
        ) =>
        let lerpAmt =
          Utils.constrain(~amt=time /. moveTime, ~low=0., ~high=1.);
        {
          ...state,
          aim: Moving(start, dest, time -. dt),
          x: Utils.lerpf(~low=destX, ~high=startX, ~value=lerpAmt),
          y: Utils.lerpf(~low=destY, ~high=startY, ~value=lerpAmt),
        };
      };
    };

  let state = {...state, prevMouseState: Env.mousePressed(env)};

  let state =
    if (Env.key(Down, env)) {
      {...state, y: state.y +. 10.};
    } else if (Env.key(Up, env)) {
      {...state, y: state.y -. 10.};
    } else {
      state;
    };
  state;
};

let run = (size, assetDir) =>
  Reprocessing.run(
    ~setup=setup(size, assetDir),
    ~draw,
    ~screen="FightyFruity",
    (),
  );
