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
  reach             : float;
  seat_angle        : float;
  seat_tube_length  : float;
  stack             : float;
  wheel_radius      : float;
  wheel_base        : float;
}

let scale_factor = 4.0

let scale geometry = {geometry with
  bb_rise           = geometry.bb_rise           /. scale_factor;
  chain_stay_length = geometry.chain_stay_length /. scale_factor;
  reach             = geometry.reach             /. scale_factor;
  seat_tube_length  = geometry.seat_tube_length  /. scale_factor;
  stack             = geometry.stack             /. scale_factor;
  wheel_radius      = geometry.wheel_radius      /. scale_factor;
  wheel_base        = geometry.wheel_base        /. scale_factor;
}

let ns_surge_medium = scale {
  bb_rise           = -13.5;
  chain_stay_length = 411.0;
  head_angle        = 65.5 *. pi /. 180.0;
  reach             = 387.0;
  seat_angle        = 72.0 *. pi /. 180.0;
  seat_tube_length  = 393.0;
  stack             = 613.0;
  wheel_radius      = 340.0;
  wheel_base        = 1103.0;
}

let ns_surge_large = scale {
  bb_rise           = -13.5;
  chain_stay_length = 411.0;
  head_angle        = 65.5 *. pi /. 180.0;
  reach             = 409.0;
  seat_angle        = 72.0 *. pi /. 180.0;
  seat_tube_length  = 432.0;
  stack             = 613.0;
  wheel_radius      = 340.0;
  wheel_base        = 1124.0;
}

let cube_hanzz_pro_medium = scale {
  bb_rise           = -20.0;
  chain_stay_length = 430.0;
  head_angle        = 65.5 *. pi /. 180.0;
  reach             = 404.0;
  seat_angle        = 68.0 *. pi /. 180.0;
  seat_tube_length  = 425.0;
  stack             = 596.0;
  wheel_radius      = 340.0;
  wheel_base        = 1159.0;
}

let vitus_vee_1_small = scale {
  bb_rise           = -45.0;
  chain_stay_length = 429.0;
  head_angle        = 71.0 *. pi /. 180.0;
  reach             = 398.0;
  seat_angle        = 73.0 *. pi /. 180.0;
  seat_tube_length  = 410.0;
  stack             = 543.0;
  wheel_radius      = 340.0;
  wheel_base        = 1040.0;
}

let orange_airo_14 = scale {
  bb_rise           = -12.0;
  chain_stay_length = 430.0;
  head_angle        = 67.0 *. pi /. 180.0;
  reach             = 370.0;
  seat_angle        = 72.5 *. pi /. 180.0;
  seat_tube_length  = 360.0;
  stack             = 540.0;
  wheel_radius      = 340.0;
  wheel_base        = 1080.0;
}

let width = 600
let height = width

let create_canvas width height =
  let canvas = Html.createCanvas Html.document in
  canvas##.width := width;
  canvas##.height := height;
  canvas

let circle ctx centre radius =
  ctx##(moveTo (centre.x +. radius) (centre.y));
  ctx##(arc (centre.x) (centre.y) radius (0.) (2. *. pi) (Js._true));
  ctx##stroke

let line ctx start finish =
  ctx##(moveTo (start.x) (start.y));
  ctx##(lineTo (finish.x) (finish.y))

let render ctx geometry =
  ctx##beginPath;
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
  line ctx rear_wheel_centre bb_centre;
  (* Seat tube. *)
  let seat_tube_top = {
    x = bb_centre.x +. geometry.seat_tube_length *. (cos geometry.seat_angle);
    y = bb_centre.y -. geometry.seat_tube_length *. (sin geometry.seat_angle);
  } in
  line ctx bb_centre seat_tube_top;
  (* Seat stays. *)
  line ctx rear_wheel_centre seat_tube_top;
  (* Top tube. *)
  let head_tube_top = {
    x = bb_centre.x -. geometry.reach;
    y = bb_centre.y -. geometry.stack;
  } in
  line ctx seat_tube_top head_tube_top;
  (* Steerer tube. *)
  line ctx front_wheel_centre head_tube_top;
  ctx##stroke

let start _ =
  let canvas = create_canvas width height in
  Dom.appendChild Html.document##.body canvas;
  let ctx = canvas##(getContext (Html._2d_)) in
  ctx##(clearRect (0.) (0.) (float width) (float height));
  ctx##.strokeStyle := (Js.string "#000000");
  render ctx ns_surge_medium;
  ctx##.strokeStyle := (Js.string "#FF0000");
  render ctx ns_surge_large;
  ctx##.strokeStyle := (Js.string "#00FF00");
  render ctx cube_hanzz_pro_medium;
  ctx##.strokeStyle := (Js.string "#0000FF");
  render ctx vitus_vee_1_small;
  ctx##.strokeStyle := (Js.string "#FF00FF");
  render ctx orange_airo_14;
  ctx##stroke;
  Js._false

let _ =
  Html.window##.onload := Html.handler start
