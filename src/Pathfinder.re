open Belt;
open Common;

module BeltTupleCompare = Id.MakeComparable({
  type t = (int, int);
  let cmp = ((x1, y1), (x2, y2)) => {
    let c = compare(x1, x2);
    if (c == 0) {
      compare(y1, y2)
    } else {
      c
    }
  };
});

type nodeT = {
  mutable cameFrom: option((int, int)),
  mutable gScore: int,
  mutable fScore: int,  
};

type t = {
  mutable map: Belt.Map.t((int, int), nodeT, BeltTupleCompare.identity),
  mutable openSet: Belt.Set.t((int, int), BeltTupleCompare.identity),
  mutable closedSet: Belt.Set.t((int, int), BeltTupleCompare.identity),
  grid: array(array(tileT)),
}

let manhattan = ((x1, y1), (x2, y2)) => {
  (x2 - x1) + (y2 - y1);
};

let rec reconstructPath = (pathfinderState, current) => {
  switch (pathfinderState.map->Map.getExn(current)) {
  | exception _ => [current]
  | {cameFrom} => 
    switch (cameFrom) {
    | None => [current]
    | Some(parent) => 
      let path = reconstructPath(pathfinderState, parent);
      [current, ...path]
    }
  }
};

let almostMaxInt = 99999999;

let collides = (grid, (x, y)) => grid->Array.getExn(x)->Array.getExn(y).collision;

let equals = ((x1, y1), (x2, y2)) => x1 == x2 && y1 == y2;

let rec pathfindHelper = (pathfinderState, goal) => {
  let anyNode = switch(pathfinderState.openSet->Set.minimum) {
    | None => failwith("Nothing in openSet")
    | Some(node) => node
  };
   
  let (current, smallestFScore) = pathfinderState.openSet->Set.reduce((anyNode, almostMaxInt), ((smallesNodeSoFar, smallestScoreSoFar), cur) => {
    let currentScore = switch(pathfinderState.map->Map.getExn(cur)) {
      | exception _ => failwith("Couldn't get " ++ cellToString(cur) ++ " fScore")
      | {fScore} => fScore
    };
    if (currentScore < smallestScoreSoFar) {
      (cur, currentScore)
    } else {
      (smallesNodeSoFar, smallestScoreSoFar)
    }
  });
  
  if (equals(current, goal)) {
    reconstructPath(pathfinderState, current);
  } else {
    
    pathfinderState.openSet = pathfinderState.openSet->Set.remove(current);
    pathfinderState.closedSet = pathfinderState.closedSet->Set.add(current);
    
    let (x, y) = current;
    let neighbors = [|(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1) |];
    let rec loop = (i) => {
      if (i >= neighbors->Array.length) {
        ()
      } else {
        let neighbor = neighbors->Array.getExn(i);
        if (pathfinderState.closedSet->Set.has(neighbor) || collides(pathfinderState.grid, neighbor)) {
          loop(i + 1);
        } else {
          let currentNode = switch (pathfinderState.map->Map.getExn(current)) {
          | exception _ => failwith("Couldn't get " ++ cellToString(current) ++ " gScore")
          | node => node
          };
          let distToNeighbor = 1;
          let tentativeGScore = currentNode.gScore + distToNeighbor;
          
          if (!pathfinderState.openSet->Set.has(neighbor)) {
            pathfinderState.openSet = pathfinderState.openSet->Set.add(neighbor);
          };
          
          let neighborNode = switch (pathfinderState.map->Map.getExn(neighbor)) {
          | exception _ => failwith("Couldn't get " ++ cellToString(neighbor) ++ " gScore")
          | node => node
          };
          if (tentativeGScore >= neighborNode.gScore) {
            loop(i + 1);
          } else {
            neighborNode.cameFrom = Some(current);
            neighborNode.gScore = tentativeGScore;
            neighborNode.fScore = tentativeGScore + manhattan(neighbor, goal);
            loop(i + 1);
          }
        }
      }
    };
    loop(0);

    if (pathfinderState.openSet->Set.isEmpty) {
      /* This probably means it can't find a path */
      []
    } else {
      pathfindHelper(pathfinderState, goal);
    }
  }
};

let pathfind = (pathfinderState, start, goal) => {
  if(pathfinderState.closedSet->Set.has(goal)) {
    reconstructPath(pathfinderState, goal);
  } else {
    if (pathfinderState.openSet->Set.isEmpty) {
      pathfinderState.openSet = Set.add(pathfinderState.openSet, start);
      let startNode = {
        cameFrom: None,
        gScore: 0,
        fScore:manhattan(start, goal)
      };
      pathfinderState.map = Map.set(pathfinderState.map, start, startNode);    
    };
    pathfindHelper(pathfinderState, goal);
  }
  /*pathfinderState.openSet = Set.add(pathfinderState.openSet, start);
  
  let startNode = {
    cameFrom: None,
    gScore: 0,
    fScore:manhattan(start, goal)
  };
  pathfinderState.map = Map.set(pathfinderState.map, start, startNode);*/

  /*pathfind(pathfinderState, goal);*/
};

let make = (grid) => {
  let openSet = Set.make(~id=(module BeltTupleCompare));
  let closedSet = Set.make(~id=(module BeltTupleCompare));
  let pathfinderMap = ref(Map.make(~id=(module BeltTupleCompare)));
  
  for (i in 0 to Array.length(grid) - 1) {
    for (j in 0 to Array.length(Array.getExn(grid, i)) - 1) {
      let node = {
        cameFrom: None,
        gScore: almostMaxInt,
        fScore: almostMaxInt
      }
      pathfinderMap := Map.set(pathfinderMap^, (i, j), node);
    }
  };
  {map: pathfinderMap^, grid, openSet, closedSet}
};
