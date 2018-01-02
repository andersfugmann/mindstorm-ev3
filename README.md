# mindstorm-ev3
Ocaml library for controlling Lego Mindstorm EV3 over bluetooth (linux).

Pair with the brick (linux)
===========================

To connect to the mindstorm, first pair with the brick:
```
$ bluetoothctl
[bluetooth]# list
Controller 5C:59:48:C8:4E:87 zaphod [default]
[bluetooth]# scan on
[bluetooth]# devices
Device 00:16:53:46:E7:F0 EV3
[bluetooth]# pair 00:16:53:46:E7:F0
# Follow the instructions on the EV3 brick
# (press center key a couple of times)
# and enter the passcode when prompted here
[bluetooth]# paired-devices
Device 00:16:53:46:E7:F0 EV3
```

if bluetooh does not work try either of the two:
```
$ sudo modprobe btusb
$ sudo hciconfig hci0 up
```

Note that systemd and/or connman might take control of the bluettooth
controller. Refer to these programs to make them release the
bluetooth controller.

Connecting
==========

The address (e.g. 00:16:53:46:E7:F0) of the brick is needed in order to connect to the brick:

You need to specify the ID of the ev3 mindstorm brick.

Sample Programs
===============

Remote: Acts as a remote. After connecting
lets the user control motors by using. Use wxad keys to move. Space to start rotor and b for beep.
