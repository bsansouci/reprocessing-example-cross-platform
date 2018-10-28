open Common;

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

let stepsCount = ref(0);

let rec pathfindHelper = (pathfinderState, goal) => {
  let anyNode = TupleSet.choose(pathfinderState.openSet);

  let (current, smallestFScore) =
    TupleSet.fold(
      (cur, (smallestNodeSoFar, smallestScoreSoFar)) => {
        let currentScore =
          switch (TupleMap.find(cur, pathfinderState.map)) {
          | exception _ =>
            failwith("Couldn't get " ++ cellToString(cur) ++ " fScore")
          | {gScore} => gScore +. distanceHeuristic(cur, goal)
          };
        if (currentScore < smallestScoreSoFar) {
          (cur, currentScore);
        } else {
          (smallestNodeSoFar, smallestScoreSoFar);
        };
      },
      pathfinderState.openSet,
      (anyNode, almostMaxFloat),
    );

  if (current == goal) {
    Some(reconstructPath(pathfinderState, current));
  } else {
    /* @Mutation */
    pathfinderState.openSet =
      TupleSet.remove(current, pathfinderState.openSet);
    pathfinderState.closedSet =
      TupleSet.add(current, pathfinderState.closedSet);

    let (x, y) = current;
    let neighbors = [|
      (x - 1, y),
      (x + 1, y),
      (x, y - 1),
      (x, y + 1),
      /*(x - 1, y - 1),
      (x - 1, y + 1),
      (x + 1, y - 1),
      (x + 1, y + 1),*/
    |];
    for (i in 0 to Array.length(neighbors) - 1) {
      let neighbor = neighbors[i];
      if (!TupleSet.mem(neighbor, pathfinderState.closedSet)
          && !collides(pathfinderState.grid, neighbor)) {
        let currentNode =
          switch (TupleMap.find(current, pathfinderState.map)) {
          | exception _ =>
            let node = {gScore: almostMaxFloat, cameFrom: None};

            /* @Mutation */
            pathfinderState.map =
              TupleMap.add(current, node, pathfinderState.map);
            node;
          | node => node
          };

        let distToNeighbor = i > 3 ? 1.2 : 1.;
        let tentativeGScore = currentNode.gScore +. distToNeighbor;

        if (!TupleSet.mem(neighbor, pathfinderState.openSet)) {
          /* @Mutation */
          pathfinderState.openSet =
            TupleSet.add(neighbor, pathfinderState.openSet);
        };

        let neighborNode =
          switch (TupleMap.find(neighbor, pathfinderState.map)) {
          | exception _ =>
            let node = {gScore: almostMaxFloat, cameFrom: None};

            /* @Mutation */
            pathfinderState.map =
              TupleMap.add(neighbor, node, pathfinderState.map);
            node;
          | node => node
          };
        if (tentativeGScore < neighborNode.gScore) {
          /* @Mutation */
          neighborNode.cameFrom = Some(current);
          neighborNode.gScore = tentativeGScore;
        };
      };
    };

    if (TupleSet.cardinal(pathfinderState.openSet) == 0) {
      Some
        ([]);
        /* This probably means it can't find a path */
    } else {
      pathfinderState.stepsCount = pathfinderState.stepsCount + 1;
      if (pathfinderState.stepsCount > maxPathfindingStepsPerEnemyPerTick) {
        pathfinderState.stepsCount = 0;
        None;
      } else {
        pathfindHelper(pathfinderState, goal);
      };
    };
  };
};

let pathfind = (pathfinderState, start, goal) =>
  if (TupleSet.mem(goal, pathfinderState.closedSet)) {
    Some(reconstructPath(pathfinderState, goal));
  } else {
    if (TupleSet.cardinal(pathfinderState.openSet) == 0) {
      /* @Mutation */
      pathfinderState.openSet = TupleSet.add(start, pathfinderState.openSet);
      let startNode = {cameFrom: None, gScore: 0.};

      /* @Mutation */
      pathfinderState.map =
        TupleMap.add(start, startNode, pathfinderState.map);
    };
    pathfindHelper(pathfinderState, goal);
  };

let make = grid => {
  let openSet = TupleSet.empty;
  let closedSet = TupleSet.empty;
  let pathfinderMap = ref(TupleMap.empty);

  {map: pathfinderMap^, grid, openSet, closedSet, stepsCount: 0};
};
