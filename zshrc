# History
export HISTFILE=~/.zsh_history
export HISTSIZE=1000
export SAVEHIST=1000
setopt histignorealldups

# Allow using # in the command line
setopt interactivecomments

# Use Emacs-style keys
bindkey -e

# Misc environment variables
export TERM=xterm
export EDITOR="emacsclient -t"
if [ -d ~/bin ]; then
    export PATH=~/bin:"${PATH}"
fi

autoload -U compinit promptinit
compinit
promptinit

# Enable menu-style completion
zstyle ':completion:*' menu select

# Use "/" as a word boundary so ctrl+w only deletes the last directory name
local WORDCHARS=${WORDCHARS//\//}

# Set prompt
export PROMPT=$'%{\e[01;32m%}%B┌─%b%{\e[01;32m%}[%n@%m] %{\e[01;34m%}%~
%{\e[01;32m%}%B└─%b%{\e[01;32m%}[$]%{\e[0m%} '
export RPROMPT=$'%t'

# Disable Ctrl+S to suspend terminal
stty -ixon

# Aliases
alias ls='ls --color=auto'
alias la='ls -alh'
alias grep='grep --color=auto'
alias e='emacsclient -t'
