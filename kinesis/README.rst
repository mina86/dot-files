Kinesis Advantage
=================

Every now and then I end up having to configure a Kinesis Advantage
keyboard to suit my taste.  Since I remap quite a few keys, this is
getting progressively more annoying each time I do this.  To make it
easier, here are the steps needed to configure Kinesis Advantage
keyboard the way I like.

Advantage2
~~~~~~~~~~

Advantage2 keyboard allows programming through the USB connection
which simplifies setting things up.  To configure the keyboard:

  - enable Power User Mode by pressing Program + Shift + Esc (LEDs
    will flash four times) and V-drive by pressing Program + F1;

  - a new USB storage device should show up, mount it;

  - copy ``qwerty.txt`` and ``state.txt`` files into ``active``
    directory on the keyboard (overwriting existing files);

  - unmount the drive;

  - disable V-drive by pressing Program + F1 and PUM by pressing
    Program + Shift + Esc.

Advantage
~~~~~~~~~

The original Advantage keyboard does not support programming through
USB and requires all configuration to be done manually.

Basic configuration
-------------------

Reset settings:
  - unplug,
  - hold F7,
  - plug back,
  - wait a few seconds,
  - release F7.
  LEDs should blink several times.

Disable audible tone and key clicks:
  - Program + ``-``
  - Program + top-right ``|\``

Check settings:
  - ``=`` + ``s``
  Should output ``v3.2[]``

Turn PC Non-Windows mode:
  - ``=`` + ``p``
  Settings should now be ``[SL K H x e]``

Pipe/backslash key handling:
  - ``=`` + ``v``
  Settings should now be ``[SL V K H x e]``


Remapping
---------

Start remapping with:
  - Program + Remap,
  LEDs should start blinking.

Numbers row
~~~~~~~~~~~

Shift numbers by one:
  - ``-``, ``0``,
  - ``0``, ``9``,
  - … and so on …
  - ``2``, ``1``,
  - ``1``, ``-``,
  This makes sense for Programmer Dvorak layout and moves all odd
  numbers to the left and all even numbers to the right side.  Without
  this, ``9`` would be on the right side of the keyboard.

Thumb keys
~~~~~~~~~~

::

           +------+------+       +------+------+
           | Left |      |       |      | Rht  |
           | Ctrl | Alt  |       | Win  | Ctrl |
    +------+------+------+       +------+------+------+
    |      |      |      |       | Page |      |      |
    |      |      | Home |       |  Up  |      |      |
    |  BS  | DEL  +------+       +------+ RET  | SPC  |
    |      |      | End  |       | Page |      |      |
    |      |      |      |       | Down |      |      |
    +------+------+------+       +------+------+------+

Left side:
  - BS, Left Ctrl,
  - DEL, Alt,
  - RET, BS,
  - Alt, DEL,

Right side:
  - Page Up, Win,
  - Page Down, Page Up,
  - Keypad, Scroll Lock (right Win key), Keypad, Page Down,
  - Keypad, bottom-left ``|\`` (Insert key), Keypad, Rht Ctrl,
  - Keypad, Win (Right Alt), Keypad, RET.

Make caps lock into control:
  - Left Ctrl, Caps Lock,

Bottom row
~~~~~~~~~~

::

    +------+------+------+------+        +------+------+------+------+
    | ~    | |    |      |      |        |      |      | {    | }    |
    |  `   |  \   | Left | Right|        | Up   | Down |  [   |  ]   |
    +------+------+------+------+        +------+------+------+------+

Left side:
  - Keypad, PrnScr (left Win key), Keypad, ``~```,
  - Left, bottom-left ``|\``,
  - Right, Left,
  - ``+=``, Right,

Right side:
  - Up, ``{[``,
  - Down, ``}]``,
  - ``}]``, Down,
  - top-right ``|\``, Up

Others:
  - ``[{``, top-right ``|\``,
  - ``~```, ``+=``,

Finish remapping with:
  - Program + Remap.
