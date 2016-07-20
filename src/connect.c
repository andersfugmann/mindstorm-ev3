#include <unistd.h>
#include <sys/socket.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>
#include <bluetooth/hci.h>
#include <bluetooth/hci_lib.h>

#include <caml/config.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/unixsupport.h>

CAMLexport
value ocaml_mindstorm_connect(value vdest)
{
  /* noalloc */
  int sock;
  int status;
  struct sockaddr_rc addr = {0};

  sock = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);
  addr.rc_family = AF_BLUETOOTH;
  addr.rc_channel = (uint8_t) 1;
  str2ba(String_val(vdest), &addr.rc_bdaddr);
  status = connect(sock, (struct sockaddr *)&addr, sizeof(addr));
  /* uerror available because we link with unix.cm[x]a */
  if (status < 0) uerror("Mindstorm.*.connect_bluetooth", vdest);

  /* a OCaml Unix.file_descr is just an int underneath (see
   * e.g. socket.c in the CVS directory of unix module). */
  return(Val_int(sock));
}
