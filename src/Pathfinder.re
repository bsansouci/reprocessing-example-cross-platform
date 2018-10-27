open Common;

module TupleCompare = {
  type t = (int, int);
  let compare = ((x1, y1), (x2, y2)) =>
    if (x1 == x2) {
      if (y1 == y2) {
        0;
      } else if (y1 < y2) {
        (-1);
      } else {
        1;
      };
    } else if (x1 < x2) {
      (-1);
    } else {
      1;
    };
};

module TupleMap = Map.Make(TupleCompare);

module TupleSet = Set.Make(TupleCompare);

type nodeT = {
  mutable cameFrom: option((int, int)),
  mutable gScore: float,
};

type t = {
  mutable map: TupleMap.t(nodeT),
  mutable openSet: TupleSet.t,
  mutable closedSet: TupleSet.t,
  grid: array(array(tileT)),
};

let distanceHeuristic = ((x1, y1), (x2, y2)) => {
  let dx = x2 - x1;
  let dy = y2 - y1;
  sqrt(float_of_int(dx * dx + dy * dy));
};

let rec reconstructPath = (pathfinderState, current) =>
  switch (TupleMap.find(current, pathfinderState.map)) {
  | exception _ => [current]
  | node =>
    switch (node.cameFrom) {
    | None => [current]
    | Some(parent) =>
      let path = reconstructPath(pathfinderState, parent);
      [current, ...path];
    }
  };

let almostMaxFloat = 99999999.;

let collides = (grid, (x, y)) => grid[x][y].collision;

let rec pathfindHelper = (pathfinderState, goal) => {
  let anyNode = TupleSet.choose(pathfinderState.openSet);

  let (current, smallestFScore) =
    TupleSet.fold(
      (cur, (smallesNodeSoFar, smallestScoreSoFar)) => {
        let currentScore =
          switch (TupleMap.find(cur, pathfinderState.map)) {
          | exception _ =>
            failwith("Couldn't get " ++ cellToString(cur) ++ " fScore")
          | {gScore} => gScore +. distanceHeuristic(cur, goal)
          };
        if (currentScore < smallestScoreSoFar) {
          (cur, currentScore);
        } else {
          (smallesNodeSoFar, smallestScoreSoFar);
        };
      },
      pathfinderState.openSet,
      (anyNode, almostMaxFloat),
    );

  if (current == goal) {
    reconstructPath(pathfinderState, current);
  } else {
    pathfinderState.openSet =
      TupleSet.remove(current, pathfinderState.openSet);
    pathfinderState.closedSet =
      TupleSet.add(current, pathfinderState.closedSet);

    let (x, y) = current;
    let neighbors = [|(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)|];
    let rec loop = i =>
      if (i >= Array.length(neighbors)) {
        ();
      } else {
        let neighbor = neighbors[i];
        if (TupleSet.mem(neighbor, pathfinderState.closedSet)
            || collides(pathfinderState.grid, neighbor)) {
          loop(i + 1);
        } else {
          let currentNode =
            switch (TupleMap.find(current, pathfinderState.map)) {
            | exception _ =>
              let node = {gScore: almostMaxFloat, cameFrom: None};
              pathfinderState.map =
                TupleMap.add(current, node, pathfinderState.map);
              node;
            | node => node
            };
          let distToNeighbor = 1.;
          let tentativeGScore = currentNode.gScore +. distToNeighbor;

          if (!TupleSet.mem(neighbor, pathfinderState.openSet)) {
            pathfinderState.openSet =
              TupleSet.add(neighbor, pathfinderState.openSet);
          };

          let neighborNode =
            switch (TupleMap.find(neighbor, pathfinderState.map)) {
            | exception _ =>
              let node = {gScore: almostMaxFloat, cameFrom: None};
              pathfinderState.map =
                TupleMap.add(neighbor, node, pathfinderState.map);
              node;
            | node => node
            };
          if (tentativeGScore >= neighborNode.gScore) {
            loop(i + 1);
          } else {
            neighborNode.cameFrom = Some(current);
            neighborNode.gScore = tentativeGScore;
            loop(i + 1);
          };
        };
      };
    loop(0);

    if (TupleSet.cardinal(pathfinderState.openSet) == 0) {
      [];
        /* This probably means it can't find a path */
    } else {
      pathfindHelper(pathfinderState, goal);
    };
  };
};

let pathfind = (pathfinderState, start, goal) =>
  if (TupleSet.mem(goal, pathfinderState.closedSet)) {
    reconstructPath(pathfinderState, goal);
  } else {
    if (TupleSet.cardinal(pathfinderState.openSet) == 0) {
      pathfinderState.openSet = TupleSet.add(start, pathfinderState.openSet);
      let startNode = {cameFrom: None, gScore: 0.};
      pathfinderState.map =
        TupleMap.add(start, startNode, pathfinderState.map);
    };
    pathfindHelper(pathfinderState, goal);
  };

let make = grid => {
  let openSet = TupleSet.empty;
  let closedSet = TupleSet.empty;
  let pathfinderMap = ref(TupleMap.empty);

  {map: pathfinderMap^, grid, openSet, closedSet};
};
