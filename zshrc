# Initialize oh-my-zsh
ZSH=$HOME/.oh-my-zsh
COMPLETION_WAITING_DOTS='true'
plugins=(git command-not-found)
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

# Use Emacs-style keys
bindkey -e
bindkey '\C-x\C-e' edit-command-line

# Disable Ctrl+S to suspend terminal
stty -ixon

# Set prompt
PROMPT='%B%F{green}┌─[%n@%m]%f %F{blue}%~%f $(git_prompt_info)
%F{green}└─[$]%f%b '
RPROMPT='%t'
ZSH_THEME_GIT_PROMPT_PREFIX="%F{yellow}("
ZSH_THEME_GIT_PROMPT_SUFFIX=")%f"

# Aliases
alias ls='ls --color=auto'
alias la='ls -alh'
alias grep='grep --color=auto'
alias e='emacsclient -t'
