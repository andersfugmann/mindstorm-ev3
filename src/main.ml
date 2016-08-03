open Ev3
let addr = "00:16:53:46:E7:F0"

let rec repeat f x =
  repeat f (f x)

let sleepf sec =
  ignore (Unix.select [] [] [] sec)

type command = Forward | Reverse | Left | Right | Spin | Beep | Unknown of int

let read_command () =
  match input_byte stdin with
  | 97 -> Left
  | 100 -> Right
  | 119 -> Forward
  | 120 -> Reverse
  | 32 -> Spin
  | 98 -> Beep
  | n -> Unknown n

let process_command conn = function
  | Left -> Highlevel.turn conn ~turn:Highlevel.Left ~ms:250
  | Right -> Highlevel.turn conn ~turn:Highlevel.Right ~ms:250
  | Forward -> Highlevel.move conn ~direction:Highlevel.Forward ~ms:250
  | Reverse -> Highlevel.move conn ~direction:Highlevel.Reverse ~ms:250
  | Spin -> Highlevel.aux conn ~rotate:Highlevel.Left ~ms:250
  | Beep -> Commands.Sound.tone conn ~vol:100 ~freq:440 ~ms:250
  | Unknown n -> Printf.printf "Unknown command: %d\n%!" n


let () =
  let conn = Comm.connect addr in
  print_endline "Connected";
  let termio = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
    Unix.{ termio with c_icanon = false; c_echo = false; c_istrip = false};
  repeat (fun () -> read_command () |> process_command conn) ()


(*


let () =
  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  sleepf 0.2;
  Commands.Sound.tone conn ~vol:2 ~freq:1200 ~ms:200;
  sleepf 0.2;
  Commands.Sound.tone conn ~vol:2 ~freq:800 ~ms:200;
  sleepf 0.2;
  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  sleepf 0.2;
  Commands.Sound.tone conn ~vol:2 ~freq:1200 ~ms:200;
  sleepf 0.2;
  Commands.Sound.tone conn ~vol:2 ~freq:1400 ~ms:200;
  sleepf 0.2;

  Commands.Output.set_type conn ~layer:0 ~ports:[Commands.Output.All] ~motor_type:Commands.Output.Medium;

  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~break:false;
  Commands.Output.set_speed conn ~layer:0 ~ports:[Commands.Output.PortA] ~speed:(-50);
  Commands.Output.start conn ~layer:0 ~ports:[Commands.Output.PortA]; (* Syncro *)
  sleepf 1.0;

  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~break:false;
  Commands.Output.set_speed conn ~layer:0 ~ports:[Commands.Output.All] ~speed:(-50);
  Commands.Output.start conn ~layer:0 ~ports:[Commands.Output.All]; (* Syncro *)
  sleepf 1.0;

  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~break:false;
  Commands.Output.set_speed conn ~layer:0 ~ports:[Commands.Output.PortB] ~speed:(-50);
  Commands.Output.start conn ~layer:0 ~ports:[Commands.Output.PortB]; (* Syncro *)
  sleepf 1.0;

  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~break:false;
  Commands.Output.set_speed conn ~layer:0 ~ports:[Commands.Output.All] ~speed:(-50);
  Commands.Output.start conn ~layer:0 ~ports:[Commands.Output.All]; (* Syncro *)
  sleepf 1.0;


  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~break:false;
  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:1000;
  print_endline "Done";
  ()
*)
