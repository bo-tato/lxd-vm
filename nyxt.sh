#!/bin/bash

set -eu

wait_lxd_vm_agent () {
# hack to wait for lxd VM agent to come online
while ! lxc info nyxt | grep -q 'CPU usage'
do
    sleep 1
done
}

lxc init images:archlinux nyxt --vm -c limits.cpu=4 -c limits.memory=4GiB -c security.secureboot=false -c raw.qemu="-spice unix,addr=/tmp/nyxt.sock,disable-ticketing"

USER=$(id -nu 1000)
lxc config device add nyxt shared disk source=/home/$USER/nyxt_shared path=/home/nyxt/shared

lxc start nyxt
wait_lxd_vm_agent

lxc exec nyxt bash <<'EOF'
systemctl start systemd-networkd-wait-online
pacman --noconfirm -Sy i3-wm xorg-server xorg-xinit gnome-terminal i3status rofi rofi-pass \
                       fish emacs-nativecomp vim xorg-xrandr chromium fzf zoxide git ripgrep \
                       fd bat sbcl base-devel pyright clojure wget httpie xclip dunst openssh \
                       shfmt shellcheck python-black python-isort marked plocate zip feh \
                       libyaml `# needed for ruby` \
                       inotify-tools `# needed for CIEL` \
                       `# needed for nyxt` \
                       webkit2gtk gobject-introspection glib-networking \
                       gsettings-desktop-schemas enchant libfixposix
useradd -s /bin/bash -m nyxt
chown nyxt:nyxt /home/nyxt
mkdir -p /etc/systemd/system/getty@tty1.service.d/
cat > /etc/systemd/system/getty@tty1.service.d/autologin.conf <<CONF
[Service]
ExecStart=
ExecStart=-/sbin/agetty -o '-p -f -- \\u' --noclear --autologin nyxt  %I $TERM
CONF

su -l nyxt
cat >> .bash_profile <<'CONF'
export PATH="$PATH:$HOME/.config/emacs/bin/"
source /home/nyxt/.rvm/scripts/rvm

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	exec startx
fi
CONF
cat > ~/.Xmodmap <<CONF
remove mod1 = Alt_L
add mod3 = Alt_L
CONF

mkdir -p $(dirname $(bat --config-file))
echo '--theme=ansi' > $(bat --config-file)

# ruby
gpg --keyserver keyserver.ubuntu.com --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB
curl -sSL https://get.rvm.io | bash -s master
source /home/nyxt/.rvm/scripts/rvm
rvm install ruby-3.2.2 --docs
gem install pry rubocop pry-doc mechanize parallel retries ruby-progressbar hashie webrick
cat > ~/.rubocop.yml <<CONF
Style/FrozenStringLiteralComment:
  Enabled: false
AllCops:
  TargetRubyVersion: 3.2
CONF

fish
fish_config theme choose "Mono Lace"
set -U fish_greeting
set -U fish_color_cwd normal
set -U fish_color_user black
set -U fish_pager_color_selected_background --background=brgreen
set -U fish_pager_color_description normal
set -U -x EDITOR vim
set -Ux FZF_DEFAULT_OPTS "--bind alt-space:jump-accept --color light"
EOF

lxc exec nyxt -- su -l nyxt <<EOF
cat > ~/.xinitrc <<'CONF'
export \$(dbus-launch)
setxkbmap -option caps:escape
xmodmap ~/.Xmodmap
# get host resolution
xrandr -s $(xrandr | grep ' connected' | grep -oP '\d+x\d+')
feh --bg-scale Gnu_wallpaper.png
# ~/.config/emacs/bin/doom run &
exec i3
CONF

mkdir bin
fish
fish_add_path bin

git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
yes | ~/.config/emacs/bin/doom install

mkdir -p ~/.config/i3/
mkdir -p ~/.config/fish/functions/
EOF

lxc restart nyxt
wait_lxd_vm_agent
# wait for X to start
lxc exec nyxt -- bash -c 'while ! pgrep Xorg > /dev/null; do sleep 1; done'
# load gnome terminal settings
lxc exec nyxt -- su -c 'export $(dbus-launch) && DISPLAY=:0 dconf load /org/gnome/terminal/legacy/' nyxt < gnome-terminal.dconf

lxc file push i3_config nyxt/home/nyxt/.config/i3/config
lxc file push config.fish nyxt/home/nyxt/.config/fish/
lxc file push fish_functions/* nyxt/home/nyxt/.config/fish/functions/
lxc file push Gnu_wallpaper.png nyxt/home/nyxt/
lxc file push pry_pipe nyxt/home/nyxt/bin/
lxc file push pryrc nyxt/home/nyxt/.pryrc
lxc file push sbclrc nyxt/home/nyxt/.sbclrc
lxc file push gitconfig nyxt/home/nyxt/.gitconfig
lxc file push bin/* nyxt/home/nyxt/bin/
lxc file push -r doom.d/* nyxt/home/nyxt/.config/doom/

lxc exec nyxt -- su -l nyxt <<'EOF'
cd bin
for name in r rl rj rh
do
    ln pry_pipe $name
done
cd

mkdir -p ~/shared/org/
~/.config/emacs/bin/doom sync < /dev/null
mkdir -p ~/.config/emacs/.local/etc/docsets/
~/.config/emacs/bin/doomscript ~/.config/doom/postinstall.el

curl https://raw.githubusercontent.com/rudolfochrist/ql-https/master/install.sh | bash

# CIEL
cd ~/common-lisp/
wget https://asdf.common-lisp.dev/archives/asdf-3.3.5.tar.gz
tar -xvf asdf-3.3.5.tar.gz
mv asdf-3.3.5 asdf
rm asdf-3.3.5.tar.gz
git clone https://github.com/bo-tato/CIEL ~/quicklisp/local-projects/CIEL
cd ~/quicklisp/local-projects/CIEL && make ql-deps
mkdir ~/lisp-cores
make build && mv bin/ciel ~/bin/
make image && mv ciel-core ~/lisp-cores/

# nyxt
git clone --recurse-submodules https://github.com/atlas-engineer/nyxt ~/common-lisp/nyxt
cd ~/common-lisp/nyxt
make all

# burp
mkdir -p ~/AUR
cd ~/AUR
git clone https://aur.archlinux.org/burpsuite-pro.git
cd burpsuite-pro
makepkg -c
EOF

lxc exec nyxt bash <<'EOF'
ln -s /home/nyxt/common-lisp/nyxt/nyxt /usr/local/bin/
pacman --noconfirm -U /home/nyxt/AUR/burpsuite-pro/*.zst
rm /home/nyxt/AUR/burpsuite-pro/*.{zst,jar}
EOF

lxc exec nyxt updatedb
