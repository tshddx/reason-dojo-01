open Reprocessing;

let size = 600;
let sizef = float(size);

type bodyT = {
  pos: (float, float),
  vel: (float, float),
  angle: float,
};

type stateT = {
  player: bodyT,
  asteroids: list(bodyT),
  bullets: list(bodyT),
  lastBulletCreated: int,
};

let makeInitialState = () => {
  let asteroids =
    Array.init(10, _ =>
      {
        pos: (Random.float(sizef), Random.float(sizef)),
        vel: (Random.float(2.0) -. 1.0, Random.float(2.0) -. 1.0),
        angle: 0.,
      }
    );
  {
    player: {
      pos: (sizef /. 2.0, sizef /. 2.0),
      vel: (0.0, 0.0),
      angle: -. Constants.half_pi,
    },
    asteroids: Array.to_list(asteroids),
    bullets: [],
    lastBulletCreated: 0,
  };
};

let setup = env => {
  Env.size(~width=size, ~height=size, env);
  makeInitialState();
};

let drawPlayer = (player, env) => {
  let (x, y) = player.pos;
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.rotate(player.angle, env);
  Draw.trianglef(
    ~p1=(0.0, 0.0),
    ~p2=((-20.0), 5.0),
    ~p3=((-20.0), (-5.0)),
    env,
  );
  Draw.popMatrix(env);
};

let asteroidRadius = 20.0;

let drawAsteroid = (asteroid, env) => {
  let (x, y) = asteroid.pos;
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.ellipsef(
    ~center=(0.0, 0.0),
    ~radx=asteroidRadius,
    ~rady=asteroidRadius,
    env,
  );

  Draw.popMatrix(env);
};

let drawBullet = (bullet, env) => {
  let (x, y) = bullet.pos;
  Draw.pushMatrix(env);
  Draw.translate(~x, ~y, env);
  Draw.ellipsef(~center=(0.0, 0.0), ~radx=1., ~rady=1., env);
  Draw.popMatrix(env);
};

let collidesWith = (a, b) => {
  let distance = Utils.distf(~p1=a.pos, ~p2=b.pos);
  distance < asteroidRadius;
};

let directionVector = angle => {
  let scale = 0.1;
  (scale *. cos(angle), scale *. sin(angle));
};

let onRight = (player, env) =>
  if (Env.key(Right, env)) {
    {...player, angle: player.angle +. 0.1};
  } else {
    player;
  };

let onLeft = (player, env) =>
  if (Env.key(Left, env)) {
    {...player, angle: player.angle -. 0.1};
  } else {
    player;
  };

let onUp = (player, env) =>
  if (Env.key(Up, env)) {
    let (dx, dy) = directionVector(player.angle);
    let (x, y) = player.vel;
    {...player, vel: (x +. dx, y +. dy)};
  } else {
    player;
  };

let scaleVec = ((x, y), ~by) => (by *. x, by *. y);

let onSpace = (state, env) => {
  let player = state.player;
  if (Env.key(Space, env)) {
    let frameCount = Env.frameCount(env);
    print_endline(string_of_int(frameCount));
    print_endline(string_of_int(state.lastBulletCreated));
    if (frameCount - state.lastBulletCreated > 20) {
      let newBullet = {
        pos: player.pos,
        vel: scaleVec(directionVector(player.angle), ~by=20.),
        angle: 0.,
      };
      {
        ...state,
        lastBulletCreated: frameCount,
        bullets: [newBullet, ...state.bullets],
      };
    } else {
      state;
    };
  } else {
    state;
  };
};

let wrap = x => x > sizef ? 0. : x < 0. ? sizef : x;
let wrap = ((x, y)) => (wrap(x), wrap(y));

let updatePos = body => {
  let (x, y) = body.pos;
  let (dx, dy) = body.vel;
  {...body, pos: (x +. dx, y +. dy) |> wrap};
};

let updateBullets = (state, env) => {
  ...state,
  bullets: state.bullets |> List.map(updatePos),
};

let draw = (state, env) => {
  Draw.background(Constants.black, env);
  Draw.noFill(env);
  Draw.stroke(Constants.white, env);
  Draw.strokeWeight(1, env);
  let player = state.player;
  drawPlayer(player, env);
  state.asteroids |> List.iter(asteroid => drawAsteroid(asteroid, env));
  state.bullets |> List.iter(b => drawBullet(b, env));
  let asteroids = state.asteroids |> List.map(updatePos);
  let player = onRight(player, env);
  let player = onLeft(player, env);
  let player = onUp(player, env);
  let player = updatePos(player);
  let state = onSpace(state, env);
  let state = updateBullets(state, env);
  let asteroids =
    asteroids
    |> List.filter(a => !(state.bullets |> List.exists(collidesWith(a))));
  if (asteroids |> List.exists(collidesWith(player))) {
    makeInitialState();
  } else {
    {...state, player, asteroids};
  };
};

run(~setup, ~draw, ~mouseDown=(_, env) => makeInitialState(), ());