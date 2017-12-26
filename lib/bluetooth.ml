open Ctypes
open Unsigned

module C = struct
  type sockaddr_rc
  let sa_family_t = uint16_t
  let bdaddr_t = array 6 uint8_t

  let sockaddr_rc = structure "sockaddr_rc"
  let rc_family = field sockaddr_rc "rc_family" sa_family_t
  let rc_bdaddr = field sockaddr_rc "rc_bdaddr" bdaddr_t
  let rc_channel = field sockaddr_rc "rc_channel" uint8_t
  let () = seal (sockaddr_rc : sockaddr_rc structure typ)

  (* int socket(int domain, int type, int protocol); *)
  let socket = Foreign.foreign "socket" ~check_errno:true (int @-> int @-> int @-> returning int)

  (* int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen); *)
  let connect = Foreign.foreign "connect" ~check_errno:true (int @-> ptr sockaddr_rc @-> int @-> returning int)
end

let af_bluetooth =  31
let btproto_rfcomm = 3
let sock_stream = 1

(** Convert a string to a bluetooth address *)
let str2ba addr =
  let re = Str.regexp ":" in
  String.lowercase_ascii addr
  |> Str.split re
  |> List.map (fun s -> Scanf.sscanf s "%x" (fun h -> h))
  |> List.map UInt8.of_int
  |> List.rev
  |> CArray.of_list uint8_t

let connect bd_addr_str : Unix.file_descr =
  (* We need to return a file descr *)
  let sock = C.socket af_bluetooth sock_stream btproto_rfcomm in
  (* Need obj_magic to convert to a unix file descr *)
  let sockaddr_rc = make C.sockaddr_rc in
  setf sockaddr_rc C.rc_family (UInt16.of_int af_bluetooth);
  setf sockaddr_rc C.rc_channel (UInt8.of_int 1);
  setf sockaddr_rc C.rc_bdaddr (str2ba bd_addr_str);

  let _rc = C.connect sock (addr sockaddr_rc) (sizeof C.sockaddr_rc) in
  Obj.magic sock (* Yikes *)
