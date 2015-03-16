open Tsdl
open Gfx
open Printf

let () =
  let w = init () in
  Sdl.delay 3000l;
  finish w;
  exit 0

