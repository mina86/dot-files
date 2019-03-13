#!/bin/sh

if [ "$(id -u)" != 0 ]; then
	echo 'Must be run as root' >&2
	exit 1
fi

set -eux

user=$(getent passwd 1000)
user=${user%%:*}
if [ -z "$user" ]; then
	echo 'Unable to determine user name' >&2
	exit 1
fi


# Add current user to sudo; ask root password
apt install sudo
grep rootpw   </etc/sudoers || echo "Defaults	rootpw"              >>/etc/sudoers
grep "^$user" </etc/sudoers || echo "$user	ALL=(root:root) ALL" >>/etc/sudoers

# Shrink initramfs
apt install busybox
sed -i 's/^MODULES=.*/MODULES=dep/' /etc/initramfs-tools/initramfs.conf

# Boot into console
sed -i '
	s/^GRUB_TIMEOUT=.*/GRUB_TIMEOUT=2/
	s/^GRUB_CMDLINE_LINUX_DEFAULT=.*/GRUB_CMDLINE_LINUX_DEFAULT=systemd.show_status=1/
	s/^GRUB_CMDLINE_LINUX=.*/GRUB_CMDLINE_LINUX=text/
	s/^#\?GRUB_TERMINAL=.*/GRUB_TERMINAL=console/
' /etc/default/grub
update-grub
systemctl set-default multi-user.target

# Keyboard layout in console
cat >/etc/default/keyboard <<EOF
# Consult the keyboard(5) manual page.
XKBMODEL=pc105
XKBLAYOUT=pl,pl
XKBVARIANT=dvp,
XKBOPTIONS=ctrl:nocaps,grp:shifts_toggle,grp_led:scroll,nbsp:level3n,compose:lctrl-altgr,compose:rctrl-algtgr
BACKSPACE=guess
EOF

# Auto login
dir=/etc/systemd/system/getty@tty1.service.d
mkdir -p "/etc/systemd/system/getty@tty1.service.d"
echo >/etc/systemd/system/getty@tty1.service.d/override.conf "
[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin $user --noclear %I 38400 linux
TTYVTDisallocate=no"

# Fun issue file
>/etc/issue printf '
 _____ _____ _____                              _____ _____ _____ _____
|   | |   __|  _  | ... %shemtrails %sontrol ... |   __|     |  |  |     |
| | | |__   |     | Regional %sonitoring %system |  |  |   --|     |  |  |
|_|___|_____|__|__| ....... Node %s/%02d ....... |_____|_____|__|__|__  _|
                                                       Scarborough  |__|

' C C M S "$(tr -dc a-z </dev/urandom |head -c2)" "$(shuf -i0-99 -n1)"

# Install and fix Sawfish
apt install sawfish
rm -f -- /etc/X11/sawfish/site-init.d/00debian.jl \
         /etc/X11/sawfish/site-init.d/00menu.jl
