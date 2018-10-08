open Reprocessing;

module AssetMap = Map.Make(String);

type aimT =
  | Nothing
  | Aiming(int, int)
  | Moving((float, float), (float, float), float);

type bulletT = {
  x: float,
  y: float,
  vx: float,
  vy: float,
};

type state = {
  x: float,
  y: float,
  aim: aimT,
  stuff: array((int, int)),
  bg: imageT,
  assetMap: AssetMap.t(imageT),
  bullets: list(bulletT),
};

let drawWithRotation = (img, ~pos as (x, y), ~height, ~width, ~rot, env) => {
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.rotate(rot, env);
  Draw.translate(
    ~x=float_of_int(width) /. (-2.),
    ~y=float_of_int(height) /. (-2.),
    env,
  );
  Draw.image(img, ~pos=(0, 0), ~height, ~width, env);
  Draw.popMatrix(env);
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
      Array.init(100, _ =>
        (
          Utils.random(~min=-1000, ~max=100),
          Utils.random(~min=-1000, ~max=1000),
        )
      ),
    bg: Draw.loadImage(~filename="./assets/background.png", env),
    assetMap: loadAssetMap(env, possibleFruits),
    bullets: [],
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
  Draw.background(Constants.white, env);
  Draw.pushMatrix(env);
  Draw.translate(
    ~x=halfWindowWf -. playerSizef -. state.x,
    ~y=halfWindowHf -. playerSizef -. state.y,
    env,
  );

  Array.iter(
    ((x, y)) =>
      Draw.image(
        AssetMap.find("pineapple", state.assetMap),
        ~pos=(x, y),
        env,
      ),
    state.stuff,
  );

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
  switch (state.aim, Env.mousePressed(env)) {
  | (Aiming(px, py), true) =>
    let dx = float_of_int(px - mx);
    let dy = float_of_int(py - my);
    let mag = sqrt(dx *. dx +. dy *. dy);
    if (mag > 20.) {
      let moveX = dx /. mag *. moveSpeed;
      let moveY = dy /. mag *. moveSpeed;
      Draw.pushStyle(env);
      Draw.strokeWeight(1, env);
      Draw.stroke(Constants.red, env);
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

  let state = {
    ...state,
    bullets:
      List.fold_left(
        (acc, {x, y, vx, vy} as bullet: bulletT) => [
          {...bullet, x: x +. vx, y: y +. vy},
          ...acc,
        ],
        [],
        state.bullets,
      ),
  };

  let state =
    switch (state.aim, Env.mousePressed(env)) {
    | (Aiming(_), true) /* Draw the arrow */
    | (Nothing, false) => state
    | (Nothing, true) => {...state, aim: Aiming(mx, my)}
    | (Aiming(px, py), false) =>
      let dx = float_of_int(px - mx);
      let dy = float_of_int(py - my);
      let mag = sqrt(dx *. dx +. dy *. dy);
      if (mag > 20.) {
        let moveX = dx /. mag *. moveSpeed;
        let moveY = dy /. mag *. moveSpeed;
        {
          ...state,
          aim:
            Moving(
              (state.x, state.y),
              (state.x -. moveX, state.y -. moveY),
              moveTime,
            ),
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
    | (Moving((startX, startY) as start, (destX, destY) as dest, time), _) =>
      let lerpAmt = Utils.constrain(~amt=time /. moveTime, ~low=0., ~high=1.);
      {
        ...state,
        aim: Moving(start, dest, time -. dt),
        x: Utils.lerpf(~low=destX, ~high=startX, ~value=lerpAmt),
        y: Utils.lerpf(~low=destY, ~high=startY, ~value=lerpAmt),
      };
    };

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
