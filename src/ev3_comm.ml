(** Use ctypes if at all needed *)
type t = { ic: in_channel; oc: out_channel }

type error =
  | Unknown_handle
  | Handle_not_ready
  | Corrupt_file
  | No_handles_available
  | No_permission
  | Illegal_path
  | File_exists
  | End_of_file
  | Size_error
  | Unknown_error
  | Illegal_filename
  | Illegal_connection

exception Error of error

let error = function
  | 0x01 -> Error Unknown_handle
  | 0x02 -> Error Handle_not_ready
  | 0x03 -> Error Corrupt_file
  | 0x04 -> Error No_handles_available
  | 0x05 -> Error No_permission
  | 0x06 -> Error Illegal_path
  | 0x07 -> Error File_exists
  | 0x08 -> Error End_of_file
  | 0x09 -> Error Size_error
  | 0x0A -> Error Unknown_error
  | 0x0B -> Error Illegal_filename
  | 0x0C -> Error Illegal_connection
  | _ -> failwith "Unknown error"


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

(* This function should set the header *)
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
  (* Check the version code *)
  let buf = Buffer.create len in
  Buffer.add_channel buf t.ic len;
  print_buffer buf

  (* Decode the reply *)
