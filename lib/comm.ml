open Core
type t = { ic: In_channel.t; oc: Out_channel.t }

let connect addr =
  let fd = Bluetooth.connect addr in
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  { ic; oc }

(* Use hex module *)
let print_buffer prefix str =
  Hex.of_string str
  |> Hex.show
  |> Printf.eprintf "%s: %s\n%!" prefix

(* Receive data *)
let recv t =
  let buffer = Bytes.create 2 in
  let (_ : unit option) = In_channel.really_input t.ic ~buf:buffer ~pos:0 ~len:2 in
  let len = EndianBytes.LittleEndian.get_int16 buffer 0 in
  let buf = Bytes.create len in
  let (_ : unit option) = In_channel.really_input t.ic ~buf ~pos:0 ~len in
  print_buffer "recv" (buf |> Bytes.to_string);
  buf

(* Send data *)
let send t (msg : String.t) =
  print_buffer "send" msg;
  Out_channel.output_string t.oc msg;
  Out_channel.flush t.oc
