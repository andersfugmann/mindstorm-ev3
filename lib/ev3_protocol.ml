type _ elem =
  | Raw8 : int elem
  | Raw16 : int elem
  | Raw32 : int elem
  | Data8 : int elem
  | Data16 : int elem
  | Data32 : int elem
  | Float : float elem
  | Array : 'a elem * int -> 'a array elem

let rec write : type a. a elem -> Bytes.t -> int -> a -> int = function
  | Raw8 -> fun buffer offset v ->
    let v = match v with
      | v when v < 0 -> (255 + v) land 0xFF
      | v -> v
    in
    EndianBytes.LittleEndian.set_int8 buffer offset v;
    1
  | Raw16 -> fun buffer offset v ->
    EndianBytes.LittleEndian.set_int16 buffer offset v;
    2
  | Raw32 -> fun buffer offset v ->
    EndianBytes.LittleEndian.set_int32 buffer offset (Int32.of_int v);
    4
  | Data8 -> fun buffer offset v ->
    let l1 = write Raw8 buffer offset 0x81 in
    let l2 = write Raw8 buffer (offset + l1) v in
    l1 + l2
  | Data16 -> fun buffer offset v ->
    let l1 = write Raw8 buffer offset 0x82 in
    let l2 = write Raw16 buffer (offset + l1) v in
    l1 + l2
  | Data32 -> fun buffer offset v ->
    let l1 = write Raw8 buffer offset 0x83 in
    let l2 = write Raw32 buffer (offset + l1) v in
    l1 + l2
  | Float -> fun buffer offset v ->
    let l1 = write Raw8 buffer offset 0x84 in (* Validate this *)
    EndianBytes.LittleEndian.set_float buffer (offset + l1) v;
    l1 + 4
  | Array (arr_type, _len) -> fun buffer offset arr ->
    Array.fold_left (fun acc v -> acc + write arr_type buffer acc v) offset arr

(* Read from the buff
let rec read : type a. a elem -> Buffer.t -> a =
  | Raw8 ->
*)
type (_, _) spec =
  | Nil : ('a, 'a) spec
  | (::) : 'a elem * ('b, 'c) spec -> (('a -> 'b), 'c) spec

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

let rec length: type a b. (a, b) spec -> int = function
  | x :: xs -> elem_size x + length xs
  | Nil -> 0

let encode spec =
  let rec encode: type a. (a, Bytes.t) spec -> Bytes.t -> int -> a = function
  | x :: xs -> fun buffer offset v ->
    let l = write x buffer offset v in
    encode xs buffer (offset + l)
  | Nil -> fun buffer _ -> buffer
  in
  let length = length spec in
  let buffer = Bytes.create length in
  encode spec buffer 0
