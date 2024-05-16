# Setup

```
  # macOS
  bash -c "$(curl -LsS https://raw.github.com/jdominpa/dotfiles/master/setup/setup.sh)"

  # Linux
  bash -c "$(wget -qO - https://raw.github.com/jdominpa/dotfiles/master/setup/setup.sh)"
```

**NOTE**: in an arch linux system the setup script can be run in a tty.
Because of that, `setup` won't execute the `update_content` script which you will have to run after restarting the system.

# Running linux GUI programs with WSL and VcXsrv

Install [VcXsrv](https://sourceforge.net/projects/vcxsrv/) and put this in `.zshenv`:

```
  export DISPLAY=$(ip route | awk '{print $3; exit}'):0
```

# License

The code is available under the [GNU General Public License v3.0](https://github.com/jdominpa/dotfiles/blob/master/LICENSE).
