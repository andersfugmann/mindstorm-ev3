type _ elem =
  | Raw8 : int elem
  | Raw16 : int elem
  | Raw32 : int elem
  | Data8 : int elem
  | Data16 : int elem
  | Data32 : int elem
  | Float : float elem
  | Array : 'a elem * int -> 'a array elem

let rec elem_size: type a. a elem -> int = function
  | Raw8   -> 1
  | Raw16  -> 2
  | Raw32  -> 4
  | Data8  -> elem_size Raw8 + 1
  | Data16 -> elem_size Raw16 + 1
  | Data32 -> elem_size Raw32 + 1
  | Float -> elem_size Raw32 + 1
  | Array (arr_type, len) ->
    elem_size arr_type * len

type (_, _) spec =
  | Nil : ('a, 'a) spec
  | (::) : 'a elem * ('b, 'c) spec -> (('a -> 'b), 'c) spec

let rec length: type a b. (a, b) spec -> int = function
  | x :: xs -> elem_size x + length xs
  | Nil -> 0

let rec write : type a. a elem -> Bytes.t -> int -> a -> unit = function
  | Raw8 -> fun buffer offset v ->
    let v = match v with
      | v when v < 0 -> (255 + v) land 0xFF
      | v -> v
    in
    EndianBytes.LittleEndian.set_int8 buffer offset v
  | Raw16 -> fun buffer offset v ->
    EndianBytes.LittleEndian.set_int16 buffer offset v
  | Raw32 -> fun buffer offset v ->
    EndianBytes.LittleEndian.set_int32 buffer offset (Int32.of_int v)
  | Data8 -> fun buffer offset v ->
    write Raw8 buffer offset 0x81;
    write Raw8 buffer (offset + 1) v
  | Data16 -> fun buffer offset v ->
    write Raw8 buffer offset 0x82;
    write Raw16 buffer (offset + 1) v
  | Data32 -> fun buffer offset v ->
    write Raw8 buffer offset 0x83;
    write Raw32 buffer (offset + 1) v
  | Float -> fun buffer offset v ->
    write Raw8 buffer offset 0x84; (* Validate this *)
    EndianBytes.LittleEndian.set_float buffer (offset + 1) v
  | Array (arr_type, _len) -> fun buffer offset arr ->
    let elem_size = elem_size arr_type in
    Array.iteri (fun i v -> write arr_type buffer (offset + i * elem_size) v) arr

let rec read : type a. a elem -> string -> int -> a = function
  | Raw8 -> fun buffer offset ->
    EndianString.LittleEndian.get_int8 buffer offset
  | Raw16 -> fun buffer offset ->
    EndianString.LittleEndian.get_int16 buffer offset
  | Raw32 -> fun buffer offset ->
    EndianString.LittleEndian.get_int32 buffer offset
    |> Int32.to_int
  | Data8 -> fun buffer offset ->
    assert ((read Raw8 buffer offset) = 0x81);
    read Raw8 buffer (offset + 1)
  | Data16 -> fun buffer offset ->
    assert ((read Raw8 buffer offset) = 0x82);
    read Raw16 buffer (offset + 1)
  | Data32 -> fun buffer offset ->
    assert ((read Raw8 buffer offset) = 0x83);
    read Raw32 buffer (offset + 1)
  | Float -> fun buffer offset ->
    assert ((read Raw8 buffer offset) = 0x84); (* Validate *)
    EndianString.LittleEndian.get_float buffer (offset + 1)
  | Array (arr_type, len) -> fun buffer offset ->
    let elem_size = elem_size arr_type in
    Array.init len (fun i -> read arr_type buffer (offset + elem_size * i))

let encode spec =
  let rec encode: type a. (a, Bytes.t) spec -> Bytes.t -> int -> a = function
  | x :: xs -> fun buffer offset v ->
    write x buffer offset v;
    encode xs buffer (offset + elem_size x)
  | Nil -> fun buffer _ -> buffer
  in
  let length = length spec in
  let buffer = Bytes.create length in
  encode spec buffer 0

let decode spec data func =
  let rec decode: type a b. (a, b) spec -> string -> int -> (a -> b) = function
    | x :: xs -> fun buffer offset acc ->
      let v = read x buffer offset in
      decode xs buffer (offset + elem_size x) (acc v)
    | Nil -> fun _ _ acc -> acc
  in
  decode spec data 0 func
