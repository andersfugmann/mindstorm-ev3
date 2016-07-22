let sleepf sec =
  ignore (Unix.select [] [] [] sec)

let message_id = ref 0
(* Helper to construct a send function *)
let helper spec ~sync opcode =
  let spec =
    let open Protocol in
    Raw16 :: Raw16 :: Raw8 :: Raw8 :: Raw8 :: Raw8 :: spec
  in
  let length = Protocol.length spec in
  fun () ->
    let b = Buffer.create length in
    let encode = Protocol.encode spec b in


    let id = !message_id in
    incr message_id;
    let reply = if sync then 0x00 else 0x80 in
    encode (length - 2) id reply 0x0 0x0 opcode


module Sound = struct
  let tone =
    let f =
      let open Protocol in
      helper ~sync:true (Data8 :: Data8 :: Data16 :: Data16 :: Nil) 0x94
    in

    fun conn ~vol ~freq ~ms ->
      let msg = f () 0x01 vol freq ms in
      (* Send it now *)
      Comm.send conn msg;
      Comm.recv conn;

      sleepf (((float) ms) /. 1000.0)
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

  let char_of_range = function
    | n when n < 0 ->
      127 + (n * (-1))
    | n -> n

  let set_type =
    let f =
      let open Protocol in
      helper ~sync:true (Data8 :: Data8 :: Data8 :: Nil) 0xA1
    in
    fun conn ~layer ~ports ~motor_type ->
      let msg = f () layer (port_bitmask ports) (int_of_motor_type motor_type) in
      Comm.send conn msg;
      Comm.recv conn


  let start =
    let f =
      let open Protocol in
      helper ~sync:true (Data8 :: Data8 :: Nil) 0xA6
    in
    fun conn ~layer ~ports ->
      let msg = f () layer (port_bitmask ports) in
      Comm.send conn msg;
      Comm.recv conn

  let stop =
    let f =
      let open Protocol in
      helper ~sync:true (Data8 :: Data8 :: Data8 :: Nil) 0xA3
    in
    fun conn ~layer ~ports ~force ->
      let break = if force then 1 else 0 in
      let msg = f () layer (port_bitmask ports) break in
      Comm.send conn msg;
      Comm.recv conn

  let set_speed =
    let f =
      let open Protocol in
      helper ~sync:true (Data8 :: Data8 :: Data8 :: Nil) 0xA4
    in
    fun conn ~layer ~ports ~speed ->
      let msg = f () layer (port_bitmask ports) (char_of_range speed) in
      Comm.send conn msg;
      Comm.recv conn

  let set_power =
    let f =
      let open Protocol in
      helper ~sync:true (Data8 :: Data8 :: Data8 :: Nil) 0xA4
    in
    fun conn ~layer ~ports ~power ->
      let msg = f () layer (port_bitmask ports) (char_of_range power) in
      Comm.send conn msg;
      Comm.recv conn

end
