Scripts for automating the creation and setup of lxd containers and virtual machines.

# `nyxt.sh`

This creates a virtual machine for web application security research with i3,
fish, burpsuite, chromium, nyxt browser, emacs, and the scripting languages I
use (python, ruby, clojure, and common lisp). I run it with:
``` sh
sudo lxc start nyxt
sudo chown $USER /tmp/nyxt.sock
spicy --uri=spice+unix:///tmp/nyxt.sock
sudo lxc stop nyxt
```
