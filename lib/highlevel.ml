(* Highlevel commands

   The commands assumes PortA and PortB to be wheels and PortC is an
   auxillary motor.

*)

open Commands

(** Move motor A and B synchoniously forward *)
type direction = Forward | Reverse
let move conn ~direction ~ms =
  let speed = match direction with
    | Forward -> 100
    | Reverse -> -100
  in
  (* should synchronize the motors *)
  Output.time_speed conn ~layer:0 ~ports:[Output.PortB; Output.PortC] ~speed ~rampup_ms:10 ~rampdown_ms:10 ~run_ms:ms ~break:false

(** Essentially just move one of the motors *)
type turn = Left | Right
let turn conn ~turn ~ms =
  let port = match turn with
    | Left -> Output.PortB
    | Right -> Output.PortC
  in
  (* We could inverse the polarity of the motor, and then turn on own axis *)
  Output.time_speed conn ~layer:0 ~ports:[port] ~speed:100 ~rampup_ms:10 ~rampdown_ms:10 ~run_ms:ms ~break:true

(** Turn the auxillary motor *)
let aux conn ~rotate ~ms  =
  let speed = match rotate with
    | Left -> 100
    | Right -> -100
  in
  Output.time_speed conn ~layer:0 ~ports:[Output.PortA] ~speed ~rampup_ms:10 ~rampdown_ms:10 ~run_ms:ms ~break:false


(** Read the sensor *)
type sensor = Near | Far | Nothing
let read_sensor _conn = ()
