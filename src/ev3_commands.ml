module Comm = Ev3_comm
open Ev3_protocol

let message_id = ref 0
(* Helper to construct a send function *)
let send_command ?(sync=false) conn opcode spec =
  let spec = Raw16 :: Raw16 :: Raw8 :: Raw8 :: Raw8 :: Raw8 :: spec in
  let length = Ev3_protocol.length spec in
  let b = Buffer.create length in
  let id = !message_id in
  incr message_id;
  let reply = if sync then 0x00 else 0x80 in
  encode spec b (Comm.send ~sync conn) (length - 2) id reply 0x0 0x0 opcode


module Sound = struct
  let tone conn ~vol ~freq ~ms =
    send_command conn ~sync:true 0x94 (Data8 :: Data8 :: Data16 :: Data16 :: Nil) 0x01 vol freq ms
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
    send_command conn ~sync:true 0xA1 (Data8 :: Data8 :: Data8 :: Nil) layer (port_bitmask ports) (int_of_motor_type motor_type)

  let start conn ?(layer=0) ~ports =
    send_command conn ~sync:true 0xA6 (Data8 :: Data8 :: Nil) layer (port_bitmask ports)

  let stop conn ?(layer=0) ~ports ~break =
    send_command conn ~sync:true 0xA3 (Data8 :: Data8 :: Data8 :: Nil) layer (port_bitmask ports) (if break then 0x01 else 0x0)

  let set_speed conn ?(layer=0) ~ports ~speed =
    send_command conn ~sync:true 0xA4 (Data8 :: Data8 :: Data8 :: Nil) layer (port_bitmask ports) speed

  let set_power conn ?(layer=0) ~ports ~power =
    send_command conn ~sync:true  0xA4 (Data8 :: Data8 :: Data8 :: Nil) layer (port_bitmask ports) power

  let time_power conn ?(layer=0) ~ports ~power ~rampup_ms ~run_ms ~rampdown_ms ~break =
    send_command conn ~sync:true 0xAD (Data8 :: Data8 :: Data8 :: Data32 :: Data32 :: Data32 :: Data8 :: Nil) layer (port_bitmask ports) power rampup_ms run_ms rampdown_ms (if break then 0x01 else 0x0)

  let time_speed conn ?(layer=0) ~ports ~speed ~rampup_ms ~run_ms ~rampdown_ms ~break =
    send_command conn ~sync:true 0xAF (Data8 :: Data8 :: Data8 :: Data32 :: Data32 :: Data32 :: Data8 :: Nil) layer (port_bitmask ports) speed rampup_ms run_ms rampdown_ms (if break then 0x01 else 0x0)

end
