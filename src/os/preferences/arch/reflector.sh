#!/bin/bash

cd "$(dirname "${BASH_SOURCE[0]}")" \
    && . "../../utils.sh"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_in_purple "\n   Reflector setup\n\n"

hook="
[Trigger]
Type = Package
Operation = Upgrade
Target = pacman-mirrorlist

[Action]
Description = Updating pacman-mirrorlist with reflector and removing pacnew...
When = PostTransaction
Depends = reflector
Exec = /bin/sh -c 'reflector --latest 200 --fastest 10 --sort rate --save /etc/pacman.d/mirrorlist; rm -f /etc/pacman.d/mirrorlist.pacnew'
"

service="
[Unit]
Description=Pacman mirrorlist update
Wants=network-online.target
After=network-online.target

[Service]
Type=oneshot
ExecStart=/usr/bin/reflector --latest 200 --fastest 10 --sort rate --save /etc/pacman.d/mirrorlist

[Install]
RequiredBy=multi-user.target
"

timer="
[Unit]
Description=Run reflector weekly

[Timer]
OnCalendar=Mon *-*-* 7:00:00
RandomizedDelaySec=15h
Persistent=true

[Install]
WantedBy=timers.target
"

execute "sudo mkdir -p '/etc/pacman.d/hooks' && sudo touch '/etc/pacman.d/hooks/mirrorupgrade.hook' && echo \"$hook\" | sudo tee '/etc/pacman.d/hooks/mirrorupgrade.hook'" \
    "Creating mirror hook"

execute "sudo touch '/etc/systemd/system/reflector.service' && echo \"$service\" | sudo tee '/etc/systemd/system/reflector.service'" \
    "Creating service"

execute "sudo touch '/etc/systemd/system/reflector.timer' && echo \"$timer\" | sudo tee '/etc/systemd/system/reflector.timer'" \
    "Creating timer"

execute "sudo systemctl daemon-reload && sudo systemctl enable reflector.timer" \
    "Activating reflector"
