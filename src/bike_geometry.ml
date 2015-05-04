module Html = Dom_html

let pi = 4. *. atan 1.

type point = {
  x: float;
  y: float;
}

type geometry = {
  bb_rise           : float;
  chain_stay_length : float;
  head_angle        : float;
  seat_angle        : float;
  wheel_radius      : float;
  wheel_base        : float;
}

let scale_factor = 4.0

let scale geometry = {geometry with
  bb_rise           = geometry.bb_rise           /. scale_factor;
  chain_stay_length = geometry.chain_stay_length /. scale_factor;
  wheel_radius      = geometry.wheel_radius      /. scale_factor;
  wheel_base        = geometry.wheel_base        /. scale_factor;
}

let test_geometry = scale {
  bb_rise           = -13.5;
  chain_stay_length = 411.0;
  head_angle        = 65.5;
  seat_angle        = 72.0;
  wheel_radius      = 340.0;
  wheel_base        = 1103.0;
}

let width = 600
let height = width

let create_canvas width height =
  let canvas = Html.createCanvas Html.document in
  canvas##width <- width;
  canvas##height <- height;
  canvas

let circle ctx centre radius =
  ctx##moveTo (centre.x +. radius, centre.y);
  ctx##arc (centre.x, centre.y, radius, 0., 2. *. pi, Js._true);
  ctx##stroke ()

let line ctx start finish =
  ctx##moveTo (start.x, start.y);
  ctx##lineTo (finish.x, finish.y);
  ctx##stroke ()

let render ctx geometry =
  (* Front wheel. *)
  let front_wheel_centre = {
    x = geometry.wheel_radius;
    y = (float height) -. geometry.wheel_radius;
  } in
  circle ctx front_wheel_centre geometry.wheel_radius;
  (* Rear wheel. *)
  let rear_wheel_centre = {
    x = geometry.wheel_radius +. geometry.wheel_base;
    y = front_wheel_centre.y;
  } in
  circle ctx rear_wheel_centre geometry.wheel_radius;
  (* Chain stays. *)
  let bb_centre = {
    x = rear_wheel_centre.x -.
      sqrt (geometry.chain_stay_length ** 2.0 -. geometry.bb_rise ** 2.0);
    y = rear_wheel_centre.y -. geometry.bb_rise;
  } in
  line ctx rear_wheel_centre bb_centre

let start _ =
  let canvas = create_canvas width height in
  Dom.appendChild Html.document##body canvas;
  let ctx = canvas##getContext (Html._2d_) in
  ctx##strokeStyle <- (Js.string "#000000");
  ctx##clearRect (0., 0., float width, float height);
  render ctx test_geometry;
  ctx##stroke ();
  Js._false

let _ =
  Html.window##onload <- Html.handler start
