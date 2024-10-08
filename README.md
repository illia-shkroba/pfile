# PFile

**PFile** is a CLI program that manages different sets of filesystem's objects
(directories, directory links, files, file links) called "profiles".

With **PFile** you could define multiple profiles (especially for the same sets
of files) and switch between them.

## Usage

Let's say you want to manage multiple versions of your Shell's history and
Browser's history/cookies. One version is for your private use and the second is
for your work.

With **PFile** you could use the `new` command to create a new profile:

```sh
pfile new private -- ~/.cache/zsh/ ~/.config/BraveSoftware/
```

This command would __move__ the *\~/.cache/zsh/* and *\~/.config/BraveSoftware/*
into the new profile called "private". After running the `new` command, the
*\~/.cache/zsh/* and *\~/.config/BraveSoftware/* are gone from their original
locations. But no worries, they are still available inside of the
*\~/.local/share/pfile/profiles/private/absolute/* directory.

After creating the "private" profile, you could use the `switch` command to
switch to it:

```sh
pfile switch private
```

This would create the following links in place of the original directories:

```sh
readlink -e -- ~/.cache/zsh/ ~/.config/BraveSoftware/
# /home/is/.local/share/pfile/profiles/private/absolute/home/is/.cache/zsh
# /home/is/.local/share/pfile/profiles/private/absolute/home/is/.config/BraveSoftware
```

The *\~/.cache/zsh/* and *\~/.config/BraveSoftware/* have become links pointing
at "private" profile's entries:

```sh
ls "$(readlink -e -- ~/.cache/zsh/)"
# history
ls "$(readlink -e -- ~/.config/BraveSoftware/)"
# Brave-Browser
```

Now let's try to create an another profile called "work" for the same files:

```sh
pfile new work -- ~/.cache/zsh/ ~/.config/BraveSoftware/
```

This command would __copy__ the contents of *\~/.cache/zsh/* and
*\~/.config/BraveSoftware/* into the new profile called "work". The "private"
profile will remain untouched. The `new` command performed __copy__ this time
instead of __move__, because it has detected that the provided filesystem's
objects are symbolic __links__.

You could check which profile is currently set with the `which` command:

```sh
pfile which
# private
```

You can see that the `new` command did not perform a switch. If you want to
switch to the "work" profile, you could use the `switch` command:

```sh
pfile switch work
pfile which
# work
```

If you don't want to copy the contents of *\~/.cache/zsh/* and
*\~/.config/BraveSoftware/* into a new profile, you could pass the `-e` (or
`--empty`) option to the `new` command. This option would cause `new` command to
create empty directories for *\~/.cache/zsh/* and *\~/.config/BraveSoftware/*
inside of the new profile. Note that the `--empty` option only affects links.
Other directories and files you provide would be moved to the new profile.

```sh
pfile new -e work2 -- ~/.cache/zsh/ ~/.config/BraveSoftware/
pfile switch work2
```

You could try now to reopen your Shell/Browser. You will notice that there is no
history.

If you want to bring the directories back to their original locations, you could
use the `unpack` command:

```sh
pfile unpack
pfile which
# No current profile set. Use `pfile new` to create a profile and `pfile switch` to switch to it.
```

After using the `unpack` command, the directories are brought back to their
original locations, but their copies are still kept inside of the profile:

```sh
readlink -e -- ~/.cache/zsh/ ~/.config/BraveSoftware/
# /home/is/.cache/zsh
# /home/is/.config/BraveSoftware

readlink -e -- /home/is/.local/share/pfile/profiles/work2/absolute/home/is/.cache/zsh
# /home/is/.local/share/pfile/profiles/work2/absolute/home/is/.cache/zsh
readlink -e -- /home/is/.local/share/pfile/profiles/work2/absolute/home/is/.config/BraveSoftware
# /home/is/.local/share/pfile/profiles/work2/absolute/home/is/.config/BraveSoftware
```

If you would try to switch back to the "private" profile, you would get this
error:

```sh
pfile switch private
# Unable to link origin "/home/is/.cache/zsh" to entry "/home/is/.local/share/pfile/profiles/private/absolute/home/is/.cache/zsh" because the origin is occupied.
```

This happens because *\~/.cache/zsh/* has become a directory instead of
a directory link pointing at the profile's entry. Thus, **PFile** refuses to
overwrite it. You could remove (or move) the directory manually or if you want
to forcibly remove it with **PFile**, you could use `-f` (or
`--force-remove-occupied`) option of the `switch` command:

```sh
pfile switch -f private
```

This would forcibly remove both:

* Filesystem's objects where the current profile's links are expected to be
  placed (only if the current profile is set).
* Filesystem's objects where the next profile's (the one you provide to the
  `switch` command) links are expected to be placed.

## Installation

You can install it via:

* [AUR](https://aur.archlinux.org/packages/pfile): `yay -Sy pfile`
* [Stack](https://docs.haskellstack.org/en/stable/): `stack install pfile`
* or build from source with [Stack](https://docs.haskellstack.org/en/stable/):

```sh
git clone https://github.com/illia-shkroba/pfile.git && cd pfile
stack install
```

## Uninstallation

You can uninstall it (if you have installed it) via:

* AUR: `yay -Rcn pfile`
* Stack (or built from source): `rm -f "$(stack path --local-bin)/pfile"`

Be sure to restore filesystem's objects that were used to create profiles. Those
could be found in **PFile** data home directory: `"$XDG_DATA_HOME/pfile"`. You
can delete the directory afterwards.

## Motivation

I work as an academic teacher and often use my terminal during lectures. When
using keybindings like *CTRL-R* in my Shell to search for a recent command from
my history, sometimes my private commands could pop-up like `pass insert
<some-service>@<my-account>` or `KEY=<my-private-key> ansible...` or `mpv
'https://www.youtube.com/watch?v=dQw4w9WgXcQ'`. The same goes for my Browser's
history.

At first, I wanted to solve this issue by creating a separate user on my system
specifically for lecture purposes. But then I've realized that:

1. All of my personal configs that I wish to use during lectures like Neovim,
   Xmonad and Qutebrowser configs should be copied to the new user's home
   directory.
2. When I change one of the configs on my main user, I should `rsync` the new
   configs to the other user.
3. Permissions of resources (e.g.: directories and files) that I use on both
   users should be handled.

The **PFile** program solves the issue for me.
