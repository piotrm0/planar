open Printf
open Tsdl

let init () =
  match Sdl.init Sdl.Init.video with
    | `Error e -> Sdl.log "init error: %s" e; exit 1
    | `Ok () ->
      match Sdl.create_window ~w:640 ~h:480 "SDL OpenGL" Sdl.Window.opengl with
        | `Error e -> Sdl.log "Create window error: %s" e; exit 1
        | `Ok w -> w
          
let finish w =
  Sdl.destroy_window w;
  Sdl.quit ()
