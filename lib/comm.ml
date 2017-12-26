open Core
type t = { ic: In_channel.t; oc: Out_channel.t }
exception CommandError
exception IllegalResponse

let connect addr =
  let fd = Bluetooth.connect addr in
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  { ic; oc }

(* Use hex module *)
let print_buffer prefix str =
  Hex.of_string str
  |> Hex.show
  |> Printf.printf "%s: %s\n%!" prefix

(** Use endian *)
let read_short buffer offset =
  Char.to_int (Buffer.nth buffer offset) + (0xFF * Char.to_int (Buffer.nth buffer (offset + 1)))

(* Receive data *)

let recv t =
  let buffer = Bytes.create 2 in
  let (_ : unit option) =
    In_channel.really_input t.ic ~buf:(Caml.Bytes.unsafe_to_string buffer) ~pos:0 ~len:2
  in
  print_buffer "input" (Caml.Bytes.to_string buffer);
  (* Read bytes at a time. *)
  let len = EndianBytes.LittleEndian.get_int16 buffer 0 in
  Printf.printf "Read len: %d\n%!" len;
  let buf = Buffer.create len in
  Buffer.add_channel buf t.ic len;
  (* Byte 2 3 is the message counter, which should match the sent message
    if (message_id = read_short buf 0) then raise IllegalResponse;
  *)
  match Buffer.nth buf 2 |> Char.to_int with
  | 0x02 -> (* No error *) ()
  | 0x04 -> (* Error *) raise CommandError
  | _ -> raise IllegalResponse

(* This function should set the header *)
let send t (msg : String.t) =
  print_buffer "output" msg;
  Out_channel.output_string t.oc msg;
  Out_channel.flush t.oc;
  recv t
