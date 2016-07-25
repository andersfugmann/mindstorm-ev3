open Ctypes

module C = struct
  let bluetooth_lib = Dl.dlopen  ~filename:"libbluetooth.so" ~flags:[Dl.RTLD_NOW]
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

  (* int str2ba(const char *str, bdaddr_t *ba); *)
  let str2ba = Foreign.foreign ~from:bluetooth_lib "str2ba" ~check_errno:true (string @-> ptr bdaddr_t @-> returning int)

  (* int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen); *)
  let connect = Foreign.foreign "connect" ~check_errno:true (int @-> ptr sockaddr_rc @-> int @-> returning int)
end

let af_bluetooth =  31
let btproto_rfcomm = 3
let sock_stream = 1

let connect bd_addr_str : Unix.file_descr =

  (* We need to return a file descr *)
  let sock = C.socket af_bluetooth sock_stream btproto_rfcomm in
  (* Need obj_magic to convert to a unix file descr *)
  let sockaddr_rc = make C.sockaddr_rc in
  setf sockaddr_rc C.rc_family (Unsigned.UInt16.of_int af_bluetooth);
  setf sockaddr_rc C.rc_channel (Unsigned.UInt8.of_int 1);

  (* Call str2ba *)
  let _rc = C.str2ba bd_addr_str (sockaddr_rc @. C.rc_bdaddr) in

  let _rc = C.connect sock (addr sockaddr_rc) (sizeof C.sockaddr_rc) in
  Obj.magic sock