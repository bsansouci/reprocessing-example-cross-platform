/*open Reprocessing;

module AssetMap = Map.Make(String);

type aimT =
  | Nothing
  | Aiming(int, int)
  | Moving((float, float), (float, float), float);

type state = {
  x: float,
  y: float,
  aim: aimT,
  stuff: array((int, int)),
  bg: imageT,
  assetMap: AssetMap.t(imageT),
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
  switch size {
  | `InitialSize => ()
  | `FullScreen => Env.size(~width=Env.maxWidth(env), ~height=Env.maxHeight(env), env)
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


let run = (size, assetDir) => Reprocessing.run(~setup=setup(size, assetDir), ~draw, ~title="FightyFruity", ());
*/

open Reprocessing;

let g = 400.;
let fruitSize = 64;
let fruitSizef = 64.;

type objectT = {
  x: float,
  y: float,
  vx: float,
  vy: float,
  r: float,
  vr: float,
  kind: string,
};

module AssetMap = Map.Make(String);

type state = {
  fruits: list(objectT),
  halves: list(objectT),
  bg: imageT,
  assetMap: AssetMap.t(imageT),
};

let possibleFruits = ["banana", "pineapple", "apple", "coconut", "orange"];

let spawnFruit = (state, env) => {
  ...state,
  fruits: [
    {
      x: Utils.randomf(~min=100., ~max=float_of_int(Env.width(env)) -. 100.),
      y: float_of_int(Env.height(env)) +. 64.,
      r: 0.,
      vr: Utils.randomf(~min=-5., ~max=5.),
      vx: Utils.randomf(~min=-100., ~max=100.),
      vy: (-600.) +. Utils.randomf(~min=-100., ~max=100.),
      kind: List.nth(possibleFruits, Utils.random(~min=0, ~max=5)),
    },
    ...state.fruits,
  ],
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

let applyGravity = (dt, g, {x, y, r, vr, vx, vy, kind}) => {
  x: x +. dt *. vx,
  y: y +. dt *. vy,
  r: r +. dt *. vr,
  vr,
  vy: vy +. dt *. g,
  vx,
  kind,
};

let drawObjectList = (l, assetMap, env) =>
  List.iter(
    ({x, y, r, kind}) =>
      drawWithRotation(
        AssetMap.find(kind, assetMap),
        ~pos=(x, y),
        ~width=fruitSize,
        ~height=fruitSize + 20,
        ~rot=r,
        env,
      ),
    l,
  );

let loadAssetMap = (env, assetDir) =>
  List.fold_left(
    (assetMap, fruitName) => {
      let assetMap =
        AssetMap.add(
          fruitName,
          Draw.loadImage(
            ~filename=assetDir ++ "/" ++ fruitName ++ "_small.png",
            env,
          ),
          assetMap,
        );
      let assetMap =
        AssetMap.add(
          fruitName ++ "_half_1",
          Draw.loadImage(
            ~filename=assetDir ++ "/" ++ fruitName ++ "_half_1_small.png",
            env,
          ),
          assetMap,
        );
      let assetMap =
        AssetMap.add(
          fruitName ++ "_half_2",
          Draw.loadImage(
            ~filename=assetDir ++ "/" ++ fruitName ++ "_half_2_small.png",
            env,
          ),
          assetMap,
        );
      assetMap;
    },
    AssetMap.empty,
    possibleFruits,
  );

let setup = (size, assetDir, env) => {
  switch size {
  | `InitialSize => ()
  | `FullScreen => Env.size(~width=Env.displayWidth(env), ~height=Env.displayHeight(env), env)
  | `Normal => Env.size(~width=500, ~height=500, env)
  };
  print_endline("displayHeight " ++ string_of_int(Env.displayHeight(env)));
  spawnFruit({
    fruits: [],
    halves: [],
    bg: Draw.loadImage(~filename=assetDir ++ "/background.png", env),
    assetMap: loadAssetMap(env, assetDir),
  }, env);
};

let draw = (state, env) => {
  let dt = Env.deltaTime(env);
  let bottom = float_of_int(Env.height(env));
  Draw.background(Utils.color(~r=100, ~g=80, ~b=29, ~a=255), env);
  Draw.image(state.bg, ~pos=(0, 0), env);
  drawObjectList(state.halves, state.assetMap, env);
  drawObjectList(state.fruits, state.assetMap, env);

  let state = Utils.random(~min=0, ~max=50) == 1 ? spawnFruit(state, env) : state;

  let state = {
    ...state,
    fruits: List.map(applyGravity(dt, g), state.fruits),
    halves: List.map(applyGravity(dt, g *. 4.), state.halves),
  };
    let halfFruitf = fruitSizef /. 2.;
    let halfFruitf = 0.;
  let state= if (Env.mousePressed(env)) {
    let (mx, my) = Env.mouse(env);
    /*print_endline("mx " ++ string_of_int(mx) ++ " my " ++ string_of_int(my));*/
    let mousef = (float_of_int(mx), float_of_int(my));
    let (sliced, unsliced) =
      List.partition(
        ({x, y}) =>
          Utils.distf(~p1=(x +. halfFruitf, y +. halfFruitf), ~p2=mousef)
          < fruitSizef
          -. 20.,
        state.fruits,
      );
    let calcVY = r => {
      let vy = Utils.randomf(~min=100., ~max=300.);
      sin(r +. Constants.pi /. 2.) > 0. ? -. vy : vy;
    };
    let newHalves =
      List.flatten(
        List.map(
          ({x, y, r, kind}) => [
            {
              x,
              y: y +. 5.,
              vx: Utils.randomf(~min=-30., ~max=30.),
              vy: -. calcVY(r),
              r,
              vr: 0.,
              kind: kind ++ "_half_1",
            },
            {
              x,
              y: y -. 5.,
              vx: Utils.randomf(~min=-30., ~max=50.),
              vy: calcVY(r),
              r,
              vr: 0.,
              kind: kind ++ "_half_2",
            },
          ],
          sliced,
        ),
      );

    {...state, fruits: unsliced, halves: state.halves @ newHalves};
    
  } else {
    state
  };

  /* Remove old stuff */
  let state = {
    ...state,
    fruits: List.filter(({y}) => y < bottom +. 100., state.fruits),
    halves: List.filter(({y}) => y < bottom +. 100., state.halves),
  };

  state;
};


let run = (size, assetDir) => Reprocessing.run(~setup=setup(size, assetDir), ~draw, ~screen="FightyFruity", ());
