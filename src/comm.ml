(** Use ctypes if at all needed *)
type t = { ic: in_channel; oc: out_channel }

external connect : string -> Unix.file_descr = "ocaml_mindstorm_connect"

let connect addr =
  let fd = connect addr in
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  { ic; oc }

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
  Buffer.output_buffer t.oc msg;
  flush t.oc

let recv t =
  let buf = Buffer.create 2 in
  Buffer.add_channel buf t.ic 2;
  print_buffer buf;

  let len =
    Char.code (Buffer.nth buf 0) + (0xFF * Char.code (Buffer.nth buf 1))
  in
  let buf = Buffer.create len in
  Buffer.add_channel buf t.ic len;
  print_buffer buf

  (* Decode the reply *)
