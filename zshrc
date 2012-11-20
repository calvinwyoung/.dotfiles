#!/usr/bin/env zsh

# Initialize oh-my-zsh
ZSH=$HOME/.oh-my-zsh
COMPLETION_WAITING_DOTS='true'
plugins=(git python command-not-found dircycle)
source $ZSH/oh-my-zsh.sh

# History
export HISTFILE=~/.zsh_history
export HISTSIZE=1000
export SAVEHIST=1000
setopt histignorealldups

# Misc environment variables
export EDITOR='emacsclient -t'
if [ -d ~/bin ]; then
    export PATH=~/bin:"${PATH}"
fi

# Allow using # in the command line
setopt interactivecomments

# Disable zsh auto-correcting because it kind of sucks
unsetopt correct_all

# Use Emacs-style keys
bindkey -e
bindkey '\C-x\C-e' edit-command-line

# Disable Ctrl+S to suspend terminal
stty -ixon

# ------------------------
# Set up prompt
# ------------------------
# virtualenv prompt info
function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo "%F{white}["`basename $VIRTUAL_ENV`"]%f "
}

# Git prompt variables
ZSH_THEME_GIT_PROMPT_PREFIX="%F{yellow}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%f"

# Prompt string
PROMPT='%B%F{green}┌─[%n@%m]%f %F{blue}%~%f $(virtualenv_info)$(git_prompt_info)
%F{green}└─[$]%f%b '
RPROMPT='%t'

# ------------------------
# Set up virtualenvwrapper
# ------------------------
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/code
export VIRTUAL_ENV_DISABLE_PROMPT=1
source /usr/local/bin/virtualenvwrapper.sh

# ------------------------
# Aliases
# ------------------------
alias ls='ls --color=auto'
alias la='ls -alh'
alias grep='grep --color=auto'
alias e='emacsclient -t'
