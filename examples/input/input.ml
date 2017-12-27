open Ev3
let addr = "00:16:53:46:E7:F0"

let () =
  let conn = Comm.connect addr in
  print_endline "Connected";

  let str = Commands.Input.get_device_list conn in
  Printf.printf "Data\n%s\n" str
