open Core
type t = { ic: In_channel.t; oc: Out_channel.t }
exception CommandError
exception IllegalResponse

let connect addr =
  let fd = Ev3_bluetooth.connect addr in
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  { ic; oc }

(* Use hex module *)
let print_buffer str =
  Hex.of_string str
  |> Hex.show
  |> Printf.printf "%s\n"

(** Use endian *)
let read_short buffer offset =
  Char.to_int (Buffer.nth buffer offset) + (0xFF * Char.to_int (Buffer.nth buffer (offset + 1)))

(* Receive data *)

let recv t =
  (* Read bytes at a time. *)
  let buf = Buffer.create 2 in
  Buffer.add_channel buf t.ic 2;
  let len = read_short buf 0 in
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
  print_buffer msg;
  Out_channel.output_string t.oc msg;
  Out_channel.flush t.oc;
  recv t
