module Comm = Ev3_comm
open Ev3_protocol

let message_id = ref 0

(* Helper to construct a send function *)
let send_command conn opcode data =
  let spec = Raw16 :: Raw16 :: Raw8 :: Raw8 :: Raw8 :: Raw8 :: Nil in
  let length = Ev3_protocol.length spec + (Bytes.length data) - 2 in
  let id = !message_id in
  incr message_id;
  let header = encode spec length id 0x00 0x0 0x0 opcode in
  Comm.send conn (Bytes.cat header data |> Bytes.to_string)

module Sound = struct
  let tone conn ~vol ~freq ~ms =
    let spec = Data8 :: Data8 :: Data16 :: Data16 :: Nil in
    let data = encode spec 0x01 vol freq ms in
    send_command conn 0x94 data
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
    List.sort_uniq compare ports
    |> List.map int_of_port
    |> List.fold_left (lor) 0

  let set_type conn ?(layer=0) ~ports ~motor_type =
    let data =
      encode
        (Data8 :: Data8 :: Data8 :: Nil)
        layer (port_bitmask ports) (int_of_motor_type motor_type)
    in
    send_command conn 0xA1 data

  let start conn ?(layer=0) ~ports =
    let data =
      encode
        (Data8 :: Data8 :: Nil)
        layer (port_bitmask ports)
    in
    send_command conn 0xA6 data

  let stop conn ?(layer=0) ~ports ~break =
    let data =
      encode
        (Data8 :: Data8 :: Data8 :: Nil)
        layer (port_bitmask ports) (if break then 0x01 else 0x0)
    in
    send_command conn 0xA3 data

  let set_speed conn ?(layer=0) ~ports ~speed =
    let spec = Data8 :: Data8 :: Data8 :: Nil in
    let data = encode spec layer (port_bitmask ports) speed in
    send_command conn 0xA4 data

  let set_power conn ?(layer=0) ~ports ~power =
    let spec = Data8 :: Data8 :: Data8 :: Nil in
    let data = encode spec layer (port_bitmask ports) power in
    send_command conn  0xA4 data

  let time_power conn ?(layer=0) ~ports ~power ~rampup_ms ~run_ms ~rampdown_ms ~break =
    let spec = Data8 :: Data8 :: Data8 :: Data32 :: Data32 :: Data32 :: Data8 :: Nil in
    let data = encode spec layer (port_bitmask ports) power rampup_ms run_ms rampdown_ms (if break then 0x01 else 0x0)
    in
    send_command conn 0xAD data

  let time_speed conn ?(layer=0) ~ports ~speed ~rampup_ms ~run_ms ~rampdown_ms ~break =
    let spec = Data8 :: Data8 :: Data8 :: Data32 :: Data32 :: Data32 :: Data8 :: Nil in
    let data = encode spec
        layer (port_bitmask ports) speed rampup_ms run_ms rampdown_ms (if break then 0x01 else 0x0)
    in
    send_command conn 0xAF data

  type polarity = Forward | Backward | Opposite
  let polarity conn ?(layer=0) ~ports ~polarity =
    let polarity = match polarity with
      | Forward -> 0x01
      | Backward -> 0xFF
      | Opposite -> 0x0
    in
    let spec = Data8 :: Data8 :: Data8 :: Nil in
    let data = encode spec layer (port_bitmask ports) polarity in
    send_command conn 0xA7 data

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
  type input_type =
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

  let name_of_input_type = function
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

  let input_type_of_int = function
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

  let read_si conn ?(layer=0) ?(input_type=0) ?(mode=0) port =
    let spec = Data8 :: Data8 :: Data8 :: Data8 :: Nil in
    let data = encode spec layer port input_type mode in
    send_command conn 0x9d data

end
