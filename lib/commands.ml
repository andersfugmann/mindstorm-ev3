open Core
open Protocol

exception CommandError
exception IllegalResponse

(*

Protcol:

For commands that return data, we must
set byte 5 to the length of the data, and for all returned values
specify offset in the result:
0x60 + offset (global values. A global value is buffer space in the result)

So we need to convert the reply spec into values.
So we need to get the offsets. So strings must have a finite size.

*)


let next_message_id =
  let id = ref 0 in
  fun () ->
    let r = !id in
    incr id;
    r

let do_command conn ~opcode ~request_spec ~reply_spec ~reply_func =
  let message_id = next_message_id () in

  let reply_func msg_id status =
    assert (msg_id = message_id);
    match status with
    | 0x02 -> reply_func
    | 0x04 -> raise CommandError
    | _ -> raise IllegalResponse
  in

  let make_frame message =
    (* Message should be the payload only. *)
    let return_arg_offsets = return_arg_offsets reply_spec |> Array.of_list in
    let reply_spec = Raw16 :: Raw8 :: reply_spec in
    let reply_length = Protocol.length reply_spec - 3 in
    let spec = Raw16 :: Raw16 :: Raw8 :: Raw8 :: Raw8 :: Raw8 ::
               Raw (Bytes.length message) :: Array (Raw8, Array.length return_arg_offsets) :: Nil in
    let message_length = (Protocol.length spec) - 2 in

    let send_recv message =
      Comm.send conn (Bytes.to_string message);
      let reply = Comm.recv conn |> Bytes.to_string in
      decode reply_spec reply reply_func
    in
    encode send_recv spec message_length message_id 0x00 reply_length 0x0 opcode message return_arg_offsets
  in
  encode make_frame request_spec

module Sound = struct
  let tone conn ~vol ~freq ~ms : unit =
    let opcode = 0x94 in
    let request_spec = Data8 :: Data8 :: Data16 :: Data16 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      0x01 vol freq ms
end

module Output = struct
  type output_ports = PortA | PortB | PortC | PortD | All
  let int_of_port = function
    | PortA -> 1
    | PortB -> 2
    | PortC -> 4
    | PortD -> 8
    | All -> 15

  type motor_type = Medium | Large
  let int_of_motor_type = function
    | Medium -> 0x07
    | Large -> 0x08

  let port_bitmask ports =
    List.dedup_and_sort ~compare:compare ports
    |> List.map ~f:int_of_port
    |> List.fold_left ~f:(lor) ~init:0

  let set_type conn ?(layer=0) ~ports ~motor_type =
    let opcode = 0xA1 in
    let request_spec = Data8 :: Data8 :: Data8 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      layer (port_bitmask ports) (int_of_motor_type motor_type)

  let start conn ?(layer=0) ~ports =
    let opcode = 0xA6 in
    let request_spec = Data8 :: Data8 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      layer (port_bitmask ports)

  let stop conn ?(layer=0) ~ports ~break =
    let opcode = 0xA3 in
    let request_spec = Data8 :: Data8 :: Data8 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      layer (port_bitmask ports) (if break then 0x01 else 0x0)

  let set_speed conn ?(layer=0) ~ports ~speed =
    let opcode = 0xA5 in
    let request_spec = Data8 :: Data8 :: Data8 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
        layer (port_bitmask ports) speed

  let set_power conn ?(layer=0) ~ports ~power =
    let opcode = 0xA4 in
    let request_spec = Data8 :: Data8 :: Data8 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      layer (port_bitmask ports) power

  let time_power conn ?(layer=0) ~ports ~power ~rampup_ms ~run_ms ~rampdown_ms ~break =
    let opcode = 0xAD in
    let request_spec = Data8 :: Data8 :: Data8 :: Data32 :: Data32 :: Data32 :: Data8 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      layer (port_bitmask ports) power rampup_ms run_ms rampdown_ms (if break then 0x01 else 0x0)

  let time_speed conn ?(layer=0) ~ports ~speed ~rampup_ms ~run_ms ~rampdown_ms ~break =
    let opcode = 0xAF in
    let request_spec = Data8 :: Data8 :: Data8 :: Data32 :: Data32 :: Data32 :: Data8 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
        layer (port_bitmask ports) speed rampup_ms run_ms rampdown_ms (if break then 0x01 else 0x0)

  type polarity = Forward | Backward | Opposite
  let polarity conn ?(layer=0) ~ports ~polarity =
    let polarity = match polarity with
      | Forward -> 0x01
      | Backward -> 0xFF
      | Opposite -> 0x0
    in
    let opcode = 0xA7 in
    let request_spec = Data8 :: Data8 :: Data8 :: Nil in
    let reply_spec = Nil in
    let reply_func = () in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      layer (port_bitmask ports) polarity

  let time_sync conn ?(layer=0) ~speed ~turn ~time ~break =
    ignore conn;
    ignore layer;
    ignore speed;
    ignore turn;
    ignore time;
    ignore break;
    ()


end

module Input = struct
  module Opcodes = struct
    let sample = 0x97
    let device_list = 0x98
    let device = 0x99
    let read = 0x9A
    let test = 0x9B
    let ready = 0x9C
    let readsi = 0x9D
    let readext = 0x9E
    let write = 0x9F
  end
  module Device = struct
    type t =
      | Unknown
      | Daisychain
      | Nxt_color
      | Nxt_dumb
      | Nxt_iic
      | Input_dumb
      | Input_uart
      | Output_dumb
      | Output_intelligent
      | Output_tacho
      | None
      | Error

    let to_string = function
      | Unknown -> "Unknown"
      | Daisychain -> "Daisychain"
      | Nxt_color -> "Nxt_color"
      | Nxt_dumb -> "Nxt_dumb"
      | Nxt_iic -> "Nxt_iic"
      | Input_dumb -> "Input_dumb"
      | Input_uart -> "Input_uart"
      | Output_dumb -> "Output_dumb"
      | Output_intelligent -> "Output_intelligent"
      | Output_tacho -> "Output_tacho"
      | None -> "None"
      | Error -> "Error"

    let of_int = function
      | 0x6f -> Unknown
      | 0x75 -> Daisychain
      | 0x76 -> Nxt_color
      | 0x77 -> Nxt_dumb
      | 0x78 -> Nxt_iic
      | 0x79 -> Input_dumb
      | 0x7A -> Input_uart
      | 0x7B -> Output_dumb
      | 0x7C -> Output_intelligent
      | 0x7D -> Output_tacho
      | 0x7E -> None
      | _ -> Error
  end

  let read_si ?(layer=0) ?(input_type=0) ?(mode=0) conn ~port =
    let opcode = 0x9D in
    let request_spec = Data8 :: Data8 :: Data8 :: Data8 :: Nil in
    let reply_spec = Float :: Nil in
    let reply_func f = f in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      layer port input_type mode


  let get_device_list conn =
    let opcode = Opcodes.device_list in
    let request_spec = Raw8 :: Nil in
    let reply_spec = Array (Raw8, 4) :: Raw8 :: Nil in
    let reply_func arr changed =
      Array.to_list arr
      |> List.mapi ~f:(Printf.sprintf "%d: 0x%X")
      |> String.concat ~sep:"\n"
      |> Printf.sprintf "Changed: 0x%X\n%s\n" changed
    in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      0x04

  (* Device mode: 0x05 *)
  (* Connection: 0x0c *)
  let get_name ?(layer=0) conn ~port =
    let opcode = Opcodes.device in
    let request_spec = Raw8 :: Raw8 :: Raw8 :: Raw8 :: Nil in
    let reply_spec = String 15 :: Nil in
    let reply_func s = s in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      layer port 0x15 16

  let get_connection?(layer=0) conn ~port =
    let opcode = Opcodes.device in
    let request_spec = Data8 :: Data8 :: Data8 :: Nil in
    let reply_spec = Data8 :: Nil in
    let reply_func device = Device.of_int device in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      0x0c layer port

  let get_typemode conn ?(layer=0) ~port =
    let opcode = Opcodes.device in
    let request_spec = Data8 :: Data8 :: Data8 :: Nil in
    let reply_spec = Data8 :: Data8 :: Nil in
    let reply_func device mode = (device, mode) in
    do_command conn ~opcode ~request_spec ~reply_spec ~reply_func
      0x05 layer port


end
