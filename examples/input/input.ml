open Core
open Ev3
let addr = "00:16:53:46:E7:F0"

let rec scan conn = function
  | 32 -> ()
  | port ->
    begin
      try
        let (tpe, mode) = Commands.Input.get_typemode conn ~layer:0 ~port in
        Printf.printf "port %d: %d, %d\n" port tpe mode;
      with
        _ -> Printf.printf "port %d: Error\n" port
    end;
    scan conn (succ port)

let rec forever f =
  f ();
  forever f

let () =

  let conn = Comm.connect addr in
  print_endline "Connected";
  (* Read form color sensor *)
  let read () =
    let f = Commands.Input.read_si ~input_type:29 ~mode:4 conn ~port:3 in
    Printf.printf "Val:%f\n%!" f;
    Thread.delay 1.0
  in

  forever read
