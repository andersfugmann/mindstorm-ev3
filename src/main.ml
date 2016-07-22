let addr = "00:16:53:46:E7:F0"

let () =
  let conn = Comm.connect addr in
  (* Comm.recv conn; *)
  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  (*
  Commands.Sound.tone conn ~vol:2 ~freq:1200 ~ms:200;
  Commands.Sound.tone conn ~vol:2 ~freq:800 ~ms:200;
  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Sound.tone conn ~vol:2 ~freq:1200 ~ms:200;
  Commands.Sound.tone conn ~vol:2 ~freq:1400 ~ms:200;
  *)
  Commands.Output.set_type conn ~layer:0 ~ports:[Commands.Output.All] ~motor_type:Commands.Output.Medium;

  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~force:false;
  Commands.Output.set_speed conn ~layer:0 ~ports:[Commands.Output.PortA] ~speed:(-50);
  Commands.Output.start conn ~layer:0 ~ports:[Commands.Output.PortA]; (* Syncro *)
  (* Thread.delay 1.0; *)

  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~force:false;
  Commands.Output.set_speed conn ~layer:0 ~ports:[Commands.Output.All] ~speed:(-50);
  Commands.Output.start conn ~layer:0 ~ports:[Commands.Output.All]; (* Syncro *)
  (* Thread.delay 1.0; *)

  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~force:false;
  Commands.Output.set_speed conn ~layer:0 ~ports:[Commands.Output.PortB] ~speed:(-50);
  Commands.Output.start conn ~layer:0 ~ports:[Commands.Output.PortB]; (* Syncro *)
  (* Thread.delay 1.0; *)

  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~force:false;
  Commands.Output.set_speed conn ~layer:0 ~ports:[Commands.Output.All] ~speed:(-50);
  Commands.Output.start conn ~layer:0 ~ports:[Commands.Output.All]; (* Syncro *)
  (* Thread.delay 1.0; *)


  Commands.Output.stop conn ~layer:0 ~ports:[Commands.Output.All] ~force:false;
  Commands.Sound.tone conn ~vol:2 ~freq:1000 ~ms:200;
  print_endline "Done";
  ()
