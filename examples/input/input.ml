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

let () =
  let conn = Comm.connect addr in
  print_endline "Connected";

  (*   scan conn 1 *)
  let rec read () =
    let str = Commands.Input.get_device_list conn in
    Printf.printf "Data\n%s\n" str;
    Thread.delay 1.0;
    read ()
  in
  read ()
