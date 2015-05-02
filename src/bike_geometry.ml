module Html = Dom_html

let pi = 4. *. atan 1.

type geometry = {
  head_angle   : float;
  seat_angle   : float;
  wheel_radius : float;
  wheel_base   : float;
}

let scale_factor = 4.0

let scale geometry = {geometry with
  wheel_radius = geometry.wheel_radius /. scale_factor;
  wheel_base   = geometry.wheel_base   /. scale_factor;
}

let test_geometry = scale {
  head_angle   = 65.5;
  seat_angle   = 72.0;
  wheel_radius = 340.0;
  wheel_base   = 1103.0;
}

let width = 600
let height = width

let create_canvas width height =
  let canvas = Html.createCanvas Html.document in
  canvas##width <- width;
  canvas##height <- height;
  canvas

let render ctx geometry =
  (* Front wheel. *)
  ctx##moveTo (
    2. *. geometry.wheel_radius,
    (float height) -. geometry.wheel_radius
  );
  ctx##arc (
    geometry.wheel_radius,
    (float height) -. geometry.wheel_radius,
    geometry.wheel_radius,
    0.,
    2. *. pi,
    Js._true
  );
  ctx##stroke ();
  (* Rear wheel. *)
  ctx##moveTo (
    2. *. geometry.wheel_radius +. geometry.wheel_base,
    (float height) -. geometry.wheel_radius
  );
  ctx##arc (
    geometry.wheel_radius +. geometry.wheel_base,
    (float height) -. geometry.wheel_radius,
    geometry.wheel_radius,
    0.,
    2. *. pi,
    Js._true
  );
  ctx##stroke ()

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
