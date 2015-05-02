module Html = Dom_html

type geometry = {
  head_angle : float;
  seat_angle : float;
  wheel_size : float;
  wheelbase : float;
}

let width = 600
let height = width

let create_canvas width height =
  let canvas = Html.createCanvas Html.document in
  canvas##width <- width;
  canvas##height <- height;
  canvas


let start _ =
  let canvas = create_canvas width height in
  Dom.appendChild Html.document##body canvas;
  Js._false

let _ =
  Html.window##onload <- Html.handler start
