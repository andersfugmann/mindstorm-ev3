type _ elem =
  | Raw8 : int elem
  | Raw16 : int elem
  | Raw32 : int elem
  | Data8 : int elem
  | Data16 : int elem
  | Data32 : int elem

let rec write : type a. a elem -> Buffer.t -> a -> unit = function
  | Raw8 -> begin fun t -> function
      | v when v < 0 ->
        let v = 255 + v in Buffer.add_char t (Char.chr (v land 0xFF))
      | v -> Buffer.add_char t (Char.chr v)
    end
  | Raw16 -> fun t v ->
    write Raw8 t (v land 0xFF);
    write Raw8 t (v lsr 8 land 0xFF)
  | Raw32 -> fun t v ->
    write Raw16 t (v land 0xFFFF);
    write Raw16 t (v lsr 16 land 0xFFFF)
  | Data8 -> fun t ->
    write Raw8 t 0x81;
    write Raw8 t
  | Data16 -> fun t ->
    write Raw8 t 0x82;
    write Raw16 t
  | Data32 -> fun t ->
    write Raw8 t 0x83;
    write Raw32 t


type (_, _, _) spec =
  | Nil : ('b, 'c, 'b) spec
  | (::) : 'a elem * ('b, 'c, 'd) spec -> (('a -> 'b), 'c, 'd) spec

let rec encode: type b a. (b, Buffer.t, a) spec -> Buffer.t -> (Buffer.t -> a) -> b = function
  | x :: xs -> fun buffer f v ->
    write x buffer v; encode xs buffer f
  | Nil -> fun buff f -> f buff

let rec elem_size: type a. a elem -> int = function
  | Raw8   -> 1
  | Raw16  -> 2
  | Raw32  -> 4
  | Data8  -> elem_size Raw8 + 1
  | Data16 -> elem_size Raw16 + 1
  | Data32 -> elem_size Raw32 + 1

let rec length: type a b c. (a, b, c) spec -> int = function
  | x :: xs -> elem_size x + length xs
  | Nil -> 0
