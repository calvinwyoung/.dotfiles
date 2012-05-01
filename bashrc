# Check for an interactive session
[ -z "$PS1" ] && return

# Color bash prompt
PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# Set terminal title
PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'

# Must change the TERM to a type that Ubuntu recognizes
if [ $TERM == "rxvt-256color" ]; then
    export TERM=rxvt-unicode
fi

# Integrate QT applications with GTK theme
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Set default text editor
export EDITOR="emacsclient -t"

# Ignore duplicates in bash_history
HISTCONTROL=ignoreboth

# Ignore these commands in history file
HISTIGNORE=ls:ll:la:pwd:exit:df:clear

# Set PATH to include user's bin dir if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Disable Ctrl+S to suspend terminal
stty -ixon

# Define aliases
alias ls='ls --color=auto'
alias la='ls -alh'
alias grep='grep --color=auto'
alias e='emacsclient -t'
