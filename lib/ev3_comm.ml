(** Use ctypes if at all needed *)
type t = { ic: in_channel; oc: out_channel }

exception CommandError
exception IllegalResponse

let connect addr =
  let fd = Ev3_bluetooth.connect addr in
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  { ic; oc }

let print_buffer buf =
  for i = 0 to Buffer.length buf - 1 do
    Printf.printf "%.02x " (Buffer.nth buf i |> Char.code)
  done;
  print_newline ()

let read_short buffer offset =
  Char.code (Buffer.nth buffer offset) + (0xFF * Char.code (Buffer.nth buffer (offset + 1)))

let recv t =
  let buf = Buffer.create 2 in
  Buffer.add_channel buf t.ic 2;
  let len = read_short buf 0 in
  let buf = Buffer.create len in
  Buffer.add_channel buf t.ic len;
  (* Byte 2 3 is the message counter, which should match the sent message
    if (message_id = read_short buf 0) then raise IllegalResponse;
  *)
  match Buffer.nth buf 2 |> Char.code with
  | 0x02 -> (* No error *) ()
  | 0x04 -> (* Error *) raise CommandError
  | _ -> raise IllegalResponse

(* This function should set the header *)
let send t ~sync msg =
  print_buffer msg;
  Buffer.output_buffer t.oc msg;
  flush t.oc;
  match sync with
  | false -> ()
  | true -> recv t (* Need the message id *)
