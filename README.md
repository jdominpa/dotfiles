#+TITLE: [[https://github.com/jdominpa][Personal dotfiles]]
#+AUTHOR: Joan Domingo Pasarin
#+EMAIL: jdomingopasarin@icloud.com

# Dotfiles

These are the base dotfiles that I start with when I set up a new environment.
They are heavily based on [Cătălin Mariș dotfiles](https://github.com/alrra/dotfiles).
For more specific local needs I use the `.local` files described in the [Local Settings](#Local Settings) section.

# Setup

To set up the `dotfiles` just run the appropriate snippet in the terminal:

(**DO NOT** run the `setup` snippet if you do not fully understand [what it does](https://github.com/jdominpa/dotfiles/src/os/setup.sh))

```
  # macOS
  bash -c "$(curl -LsS https://raw.github.com/jdominpa/dotfiles/master/setup/setup.sh)"

  # Linux
  bash -c "$(wget -qO - https://raw.github.com/jdominpa/dotfiles/master/setup/setup.sh)"
```

**NOTE**: in an arch linux system the setup script can be run in a tty.
Because of that, `setup` won't execute the `update_content` script which you will have to run after restarting the system.

The setup process will:

1. Download the dotfiles on your computer (by default it will suggest `~/Projects/dotfiles`).
2. [Symlink](https://github.com/jdominpa/src/os/create_symbolic_links.sh) the [git](https://github.com/jdominpa/src/git), [shell](https://github.com/jdominpa/src/shell), [zsh](https://github.com/jdominpa/src/zsh), [emacs](https://github.com/jdominpa/dotfiles/tree/master/src/emacs/emacs.d) and [neovim](https://github.com/jdominpa/dotfiles/tree/master/src/neovim/config/nvim) files.
3. Install applications/command-line tools for [macOS](https://github.com/jdominpa/dotfiles/tree/master/src/os/install/macos) / [Arch](https://github.com/jdominpa/dotfiles/tree/master/src/os/install/arch) / [Ubuntu](https://github.com/jdominpa/dotfiles/tree/master/src/os/install/ubuntu) / [Ubuntu WSL](https://github.com/jdominpa/dotfiles/tree/master/src/os/install/ubuntu-wsl).
4. Set custom [macOS](https://github.com/jdominpa/dotfiles/tree/master/src/os/preferences/macos) / [Arch](https://github.com/jdominpa/dotfiles/tree/master/src/os/preferences/arch) / [Ubuntu](https://github.com/jdominpa/dotfiles/tree/master/src/os/preferences/ubuntu) preferences.

## Running linux GUI programs with WSL2

To run programs like GUI Emacs through WSL2 first you have to install a
graphical X server. These are some commonly used ones:

- [VcXsrv](https://sourceforge.net/projects/vcxsrv/) (free open-source X server)
- [X410](https://x410.dev) (paid X server available on Microsoft Store)

After installing an X server you need to set the `DISPLAY` environment variable on Linux to use the Windows host's IP address,
as WSL2 and the Windows host are not in the same network device.
To do this, put the following in your `bashrc` / `zshrc`:

```
  export DISPLAY=$(ip route | awk '{print $3; exit}'):0
```

# Customize

## Local Settings

The `dotfiles` can be easily extended to suit additional local requirements by using the following files:

### `~/.zsh.local`

The `~/.zsh.local` file will be automatically sourced after all the other [zsh related files](https://github.com/jdominpa/dotfiles/tree/master/src/zsh_shell),
thus, allowing its content to add to or overwrite the existing aliases, settings, PATH, etc.

Here is a very simple example of a `~/.zsh.local` file:

```
  #!/bin/zsh

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # Set local aliases.

  alias starwars="telnet towel.blinkenlights.nl"

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  # Set PATH additions.

  PATH="$PATH:$HOME/Projects/dotfiles/src/bin"

  export PATH
```

### `~/.gitconfig.local`

The `~/.gitconfig.local` file will be automatically included after the configurations from `~/.gitconfig`,
thus, allowing its content to overwrite or add to the existing git configurations.

_Note_: Use `~/.gitconfig.local` to store sensitive information such as the git user credentials, e.g.:

```
  [commit]

      # Sign commits using GPG.
      # https://help.github.com/articles/signing-commits-using-gpg/

      gpgsign = true


  [user]

      name = Joan Domingo Pasarin
      email = jdominpa@example.com
      signingkey = XXXXXXXX
```

### `~/.config/nvim/init.vim.local`

The `~/.config/nvim/init.vim.local` file will be automatically sourced after `~/.config/nvim/init.vim`,
thus, allowing its content to add or overwrite the settings from `~/.config/nvim/init.vim`.

## Forks

If you decide to fork this project, do not forget to substitute my username with your own in the [setup snippets](#Setup)
and in the [setup script](https://github.com/jdominpa/dotfiles/blob/master/src/os/setup.sh).

# License

The code is available under the [GNU General Public License v3.0](https://github.com/jdominpa/dotfiles/blob/master/LICENSE).
