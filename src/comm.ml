(** Use ctypes if at all needed *)
type t = out_channel

external connect : string -> Unix.file_descr = "ocaml_mindstorm_connect"

let connect addr =
  connect addr |> Unix.out_channel_of_descr

let system_reply = '\x01'
let system_no_reply = '\x81'
let direct_reply = '\x00'
let direct_no_reply = '\x80'

let print_buffer buf =
  for i = 0 to Buffer.length buf - 1 do
    Printf.printf "%.02x " (Buffer.nth buf i |> Char.code)
  done;
  print_newline ()

let send t msg =
  print_buffer msg;
  Buffer.output_buffer t msg;
  flush t
