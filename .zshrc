# Keybindings!

bindkey ';5C' emacs-forward-word
bindkey ';5D' emacs-backward-word

# Aliases!

alias ls='ls --color=auto'
alias la='ls -lAh'
alias ll='ls -lh'
alias lag='ls -lAh | grep'
alias pag='ps -A   | grep'

alias untar='tar -xf'
alias unzip='7za x'

# Aura aliases

alias sas='sudo aura -S'

alias auss='aura -Ss'
alias ausi='aura -Si'

alias sasy='sudo aura -Sy'
alias sasyu='sudo aura -Syu'

alias sar='sudo aura -R'
alias sars='sudo aura -Rs'

# # Set up envoy for ssh

# envoy -t ssh-agent
# source <(envoy -p)

# autocompletion with arrow-key driven interface
zstyle ':completion:*' menu select

# autocompletion for command line switches for aliases
setopt completealiases

setopt HIST_IGNORE_DUPS

autoload -U promptinit && promptinit
prompt adam2

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
autoload -Uz compinit
compinit
# End of lines added by compinstall
