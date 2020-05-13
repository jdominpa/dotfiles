# [Joan Domingo Pasarin](https://github.com/jdominpa)’s dotfiles

[![Build Status](https://github.com/jdominpa/dotfiles/workflows/CI/badge.svg)](https://github.com/jdominpa/dotfiles/actions)

These are the base dotfiles that I start with when I set up a new
environment. They are heavily based on [Cătălin Mariș
dotfiles](https://github.com/alrra/dotfiles/).  For more specific
local needs I use the `.local` files described in the [`Local
Settings`](#local-settings) section.

## Table of Contents

* [Setup](#setup)
* [Customize](#customize)
  * [Forks](#forks)
  * [Local Settings](#local-settings)
    * [`~/.bash.local`](#bashlocal)
    * [`~/.gitconfig.local`](#gitconfiglocal)
    * [`~/.config/nvim/init.vim.local`](#confignviminitvimlocal)
* [Update](#update)
* [License](#license)

## Setup

To set up the `dotfiles` just run the appropriate snippet in the
terminal:

(:warning: **DO NOT** run the `setup` snippet if you do not fully
understand [what it does][setup]. Seriously, **DON'T**!)

| OS      | Snippet                                                                                   |
|:--------|:------------------------------------------------------------------------------------------|
| `macOS` | `bash -c "$(curl -LsS https://raw.github.com/jdominpa/dotfiles/master/src/os/setup.sh)"`  |
| `Linux` | `bash -c "$(wget -qO - https://raw.github.com/jdominpa/dotfiles/master/src/os/setup.sh)"` |

**NOTE**: in an arch linux system the setup script can be run in a
tty. Because of that, `setup` won't execute the `update_content`
script which you will have to run after restarting the system.

The setup process will:

* Download the dotfiles on your computer (by default it will
  suggest `~/Projects/dotfiles`)
* [Symlink][symlink] the
  [`git`](src/git),
  [`shell`](src/shell),
  [`zsh`](src/zsh_shell),
  [`emacs`](src/emacs/emacs.d) and
  [`neovim`](src/neovim/config/nvim) files
* Install applications / command-line tools for
  [`macOS`](src/os/install/macos) /
  [`Arch`](src/os/install/arch) /
  [`Ubuntu`](src/os/install/ubuntu)
* Set custom
  [`macOS`](src/os/preferences/macos) /
  [`Arch`](src/os/preferences/arch) /
  [`Ubuntu`](src/os/preferences/ubuntu) preferences

## Customize

### Local Settings

The `dotfiles` can be easily extended to suit additional local
requirements by using the following files:

#### `~/.zsh.local`

The `~/.zsh.local` file will be automatically sourced after all the
other [`zsh` related files](src/zsh_shell), thus, allowing its content to add
to or overwrite the existing aliases, settings, PATH, etc.

Here is a very simple example of a `~/.zsh.local` file:

```bash
#!/bin/zsh

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Set local aliases.

alias starwars="telnet towel.blinkenlights.nl"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Set PATH additions.

PATH="$PATH:$HOME/Projects/dotfiles/src/bin"

export PATH

```

#### `~/.gitconfig.local`

The `~/.gitconfig.local` file will be automatically included after the
configurations from `~/.gitconfig`, thus, allowing its content to
overwrite or add to the existing `git` configurations.

__Note:__ Use `~/.gitconfig.local` to store sensitive information such
as the `git` user credentials, e.g.:

```bash
[commit]

    # Sign commits using GPG.
    # https://help.github.com/articles/signing-commits-using-gpg/

    gpgsign = true


[user]

    name = Joan Domingo Pasarin
    email = jdominpa@example.com
    signingkey = XXXXXXXX
```

#### `~/.config/nvim/init.vim.local`

The `~/.config/nvim/init.vim.local` file will be automatically sourced
after `~/.config/nvim/init.vim`, thus, allowing its content to add or
overwrite the settings from `~/.config/nvim/init.vim`.

### Forks

If you decide to fork this project, do not forget to substitute my
username with your own in the [`setup` snippets](#setup) and [in the
`setup` script][setup].

## Update

To update the dotfiles you can either run the [`setup` script][setup]
or, if you want to just update one particular part, run the
appropriate [`os` script](src/os).

## License

The code is available under the [GPL-3.0 license][license].

<!-- Link labels: -->

[dotfiles Cătălin]: https://github.com/alrra/dotfiles
[github Cătălin]: https://github.com/alrra
[dotfiles Mathias]: https://github.com/mathiasbynens/dotfiles
[github Mathias]: https://github.com/mathiasbynens
[license]: LICENSE
[setup]: src/os/setup.sh
[shell]: src/shell
[symlink]: src/os/create_symbolic_links.sh
