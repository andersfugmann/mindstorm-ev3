open Core
type _ elem =
  | Raw : int -> Bytes.t elem
  | Raw8 : int elem
  | Raw16 : int elem
  | Raw32 : int elem
  | Data8 : int elem
  | Data16 : int elem
  | Data32 : int elem
  | Float : float elem
  | Array : 'a elem * int -> 'a array elem
  | String : int -> string elem

let rec elem_size: type a. a elem -> int = function
  | Raw n  -> n
  | Raw8   -> 1
  | Raw16  -> 2
  | Raw32  -> 4
  | Data8  -> elem_size Raw8 + 1
  | Data16 -> elem_size Raw16 + 1
  | Data32 -> elem_size Raw32 + 1
  | Float -> elem_size Raw32 + 1
  | Array (arr_type, len) ->
    elem_size arr_type * len
  | String n -> n + 1 (* Zero terminated *)

type (_, _) spec =
  | Nil : ('a, 'a) spec
  | (::) : 'a elem * ('b, 'c) spec -> (('a -> 'b), 'c) spec

let rec elements: type a b. (a, b) spec -> int = function
  | Nil -> 0
  | _ :: xs -> 1 + (elements xs)

let rec length: type a b. (a, b) spec -> int = function
  | x :: xs -> elem_size x + length xs
  | Nil -> 0

let rec write : type a. a elem -> Bytes.t -> int -> a -> unit = function
  | Raw len -> fun buffer offset v ->
    Bytes.blit ~src:v ~src_pos:0 ~dst:buffer ~dst_pos:offset ~len
  | Raw8 -> fun buffer offset v ->
    let v = match v with
      | v when v < 0 -> (255 + v) land 0xFF
      | v -> v
    in
    EndianBytes.LittleEndian.set_int8 buffer offset v
  | Raw16 -> fun buffer offset v ->
    EndianBytes.LittleEndian.set_int16 buffer offset v
  | Raw32 -> fun buffer offset v ->
    EndianBytes.LittleEndian.set_int32 buffer offset (Int32.of_int_exn v)
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
    Array.iteri ~f:(fun i v -> write arr_type buffer (offset + i * elem_size) v) arr
  | String _n -> failwith "Cannot send strings"

let rec read : type a. a elem -> string -> int -> a = function
  | Raw len -> fun buffer offset ->
    String.sub buffer ~pos:offset ~len |> Bytes.of_string
  | Raw8 -> fun buffer offset ->
    EndianString.LittleEndian.get_int8 buffer offset
  | Raw16 -> fun buffer offset ->
    EndianString.LittleEndian.get_int16 buffer offset
  | Raw32 -> fun buffer offset ->
    EndianString.LittleEndian.get_int32 buffer offset
    |> Int32.to_int_exn
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
    Array.init len ~f:(fun i -> read arr_type buffer (offset + elem_size * i))
  | String n -> fun buffer offset ->
    String.sub buffer ~pos:offset ~len:n

let encode func spec =
  let rec encode: type a b. (a, b) spec -> (Bytes.t -> b) -> Bytes.t -> int -> a = function
  | x :: xs -> fun func buffer offset v ->
    write x buffer offset v;
    encode xs func buffer (offset + elem_size x)
  | Nil -> fun func buffer _ -> func buffer
  in
  let length = length spec in
  let buffer = Bytes.create length in
  encode spec func buffer 0


let decode spec data func =
  let rec decode: type a b. (a, b) spec -> string -> int -> (a -> b) = function
    | x :: xs -> fun buffer offset acc ->
      let v = read x buffer offset in
      decode xs buffer (offset + elem_size x) (acc v)
    | Nil -> fun _ _ acc -> acc
  in
  decode spec data 0 func

let return_arg_offsets spec =
  let rec inner: type a b. int -> (a, b) spec -> int list = fun offset -> function
    | x :: xs ->
      let size = elem_size x in
      0x60 + offset :: inner (offset + size) xs
    | Nil -> []
  in
  inner 0 spec
