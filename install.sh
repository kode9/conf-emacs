#!usr/bin/env sh

################################################################################
## Simple installation script for my Emacs configuration.
## https://git.the-pluc.net/conf-emacs.git
##
## This will download the latest version of my emacs configuration and
## put it in ~/.emacs.d. If you  already have an ~/.emacs.d folder, it
## will ask you if you want to delete or back it up.
##
## Usage:
##   wget https://git.the-pluc.net/conf-emacs.git/raw/cask/install.sh -O - | sh
##
## TODO:
##   - Change branch from 'cask' to 'master' after merge
##   - Check emacs version
################################################################################

readonly GIT=$(which git)
readonly TAR=$(which tar)
readonly WGET=$(which wget)
readonly EMACS=$(which emacs)

readonly EMACSD="$HOME/.emacs.d"
readonly TARBALL_URL='https://git.the-pluc.net/index.php/conf-emacs.git/tarball/cask'
readonly TARBALL='/tmp/conf-emacs.tar'
readonly CASK_REPO='https://github.com/cask/cask.git'

_check_bin() {
    local name=$1
    shift
    local bin=$1

    if ! test -x "$bin"; then
	echo "Command '$name' could not be found. Aborting."
	exit 1
    else
	echo "Found '$name': [$bin]."
    fi
}

_print_version() {
    local version=$("$EMACS" --version | head -n 1)
    echo "Emacs version: $version"
}

_delete_folder() {
    echo "Deleting $EMACSD..."
    rm -rf "$EMACSD"
}


_backup_folder() {
    local backup="$EMACSD-backup-$(date -I).tar.xz"
    echo "Backing up $EMACSD to $backup"
    "$TAR" -czf "$backup" "$EMACSD" && _delete_folder "$EMACSD"
}

_check_folder() {
    if test -d "$EMACSD" || test -h "$EMACSD"; then
	printf "Folder ~/.emacs.d already exists, What do you want to do? [b]ackup / [d]elete / [q]quit: "
	read choice
	if [ "$choice" = b ] || [ "$choice" = B ]; then
	    _backup_folder "$EMACSD"
	elif [ "$choice" = d ] || [ "$choice" = D ]; then
	    _delete_folder "$EMACSD"
	else
	    echo 'Quitting...'
	    exit 1
	fi
    fi
}

_install() {
    echo "Creating empty $EMACSD..."
    mkdir -p "$EMACSD"

    echo "Downlading and extracting tarball..."
    "$WGET" --quiet "$TARBALL_URL" -O - | "$TAR" -C "$EMACSD" -xf -

    cd "$EMACSD"

    # Submodules are not included in tarballs, need to download them
    echo "Downloading Cask..."
    "$GIT" clone --quiet --depth 1 -- "$CASK_REPO" cask
    export PATH="$PATH:$PWD/cask/bin"

    local CASK=$(which cask)
    _check_bin 'cask' "$CASK"

    echo "Initializing cask packages... It may take a while..."
    cask install >/dev/null 2>&1

    echo "Starting emacs..."
    nohup "$EMACS" --no-site-file --no-splash >/dev/null 2>&1 </dev/null &

    echo "Done."
}

main() {
    _check_bin 'git' "$GIT"
    _check_bin 'tar' "$TAR"
    _check_bin 'wget' "$WGET"
    _check_bin 'emacs' "$EMACS"

    _print_version

    _check_folder
    _install

    exit 0
}

main

echo "Something went wrong!"
exit 1
