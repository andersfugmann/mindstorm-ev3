let message_id = ref 0
(* Helper to construct a send function *)
let helper spec opcode opcmd =
  let spec =
    let open Protocol in
    Raw16 :: Raw16 :: Raw8 :: Raw8 :: Raw8 :: Raw8 :: Raw8 :: spec
  in
  let length = Protocol.length spec in
  fun () ->
    let b = Buffer.create length in
    let encode = Protocol.encode spec b in


    let id = !message_id in
    incr message_id;
    encode (length - 2) id 0x80 0x0 0x0 opcode opcmd

let tone =
  let f =
    let open Protocol in
    helper (Data8 :: Data16 :: Data16 :: Nil) 0x94 0x01
  in

  fun conn ~vol ~freq ~ms ->
    let msg = f () vol freq ms in
    (* Send it now *)
    Comm.send conn msg
