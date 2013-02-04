#!/usr/bin/env bash

# Check for an interactive session
[ -z "$PS1" ] && return

# Integrate QT applications with GTK theme
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Set default text editor
export EDITOR="vim"

# Ignore duplicates in bash_history
HISTCONTROL=ignoreboth

# Ignore these commands in history file
HISTIGNORE=ls:ll:la:pwd:exit:df:clear

# Set PATH to include user's bin dir if it exists
if [ -d "$HOME/.bin" ] ; then
    PATH="$HOME/.bin:$PATH"
fi

# Disable Ctrl+S to suspend terminal
stty -ixon

# ------------------------
# Prompt
# ------------------------
# Virtualenv prompt info
function virtualenv_prompt_info {
    [[ $VIRTUAL_ENV ]] && echo "[$(basename $VIRTUAL_ENV)] "
}

# Git prompt info
function parse_git_dirty {
  [[ -n $(git status -s ${SUBMODULE_SYNTAX}  2> /dev/null) ]] && echo "*"
}
function git_prompt_info {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || \
  ref=$(git rev-parse --short HEAD 2> /dev/null) || return
  echo "(${ref#refs/heads/}$(parse_git_dirty))"
}

# Color bash prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\] $(virtualenv_prompt_info)\[\033[01;33m\]$(git_prompt_info)\[\033[00m\]
$ '

# ------------------------
# Aliases
# ------------------------
alias ls=$([ `uname` == "Darwin" ] && echo "ls -G" || echo "ls --color=auto")
alias la='ls -alh'
alias grep='grep --color=auto'
alias e='emacsclient -t'
