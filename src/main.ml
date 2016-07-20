let addr = "00:16:53:46:E7:F0"

let () =
  let conn = Comm.connect addr in
  (* Comm.recv conn; *)
  Commands.tone conn ~vol:2 ~freq:1000 ~ms:1000;
  Thread.delay 0.200;
  Commands.tone conn ~vol:2 ~freq:1200 ~ms:1000;
  Thread.delay 0.200;
  Commands.tone conn ~vol:2 ~freq:800 ~ms:1000;
  Thread.delay 0.200;
  Commands.tone conn ~vol:2 ~freq:1000 ~ms:1000;
  Thread.delay 0.200;
  Commands.tone conn ~vol:2 ~freq:1200 ~ms:1000;
  Thread.delay 0.200;
  Commands.tone conn ~vol:2 ~freq:1400 ~ms:1000;
  print_endline "Done"
