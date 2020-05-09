#
# Executes commands at the start of an interactive session.
#

#
# Completion
#

autoload -Uz compinit

typeset -i updated_at=$(date +'%j' -r ~/.zcompdump 2>/dev/null || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)
if [ $(date +'%j') != $updated_at ]; then
  compinit -i
else
  compinit -C -i
fi

setopt auto_list # automatically list choices on ambiguous completion
setopt auto_menu # automatically use menu completion
setopt always_to_end # move cursor to end if word had one match

zstyle ':completion:*' menu select # select completions with arrow keys
zstyle ':completion:*' group-name '' # group results by category
zstyle ':completion:::::' completer _expand _complete _ignored _approximate # enable approximate matches for completion
zstyle ':completion:*' list-colors "${(@s.:.)LS_COLORS}"

bindkey '^[[Z' reverse-menu-complete # shift+tab to cycle back through completions

zmodload -i zsh/complist

#
# History
#

HISTFILE=$HOME/.zhistory
HISTSIZE=100000
SAVEHIST=$HISTSIZE

#
# Prompt
#

autoload -Uz promptinit
promptinit

#
# Load plugins with antibody
#
source <(antibody init)
antibody bundle zsh-users/zsh-completions
antibody bundle zsh-users/zsh-history-substring-search
antibody bundle zdharma/fast-syntax-highlighting
antibody bundle mafredri/zsh-async
antibody bundle sindresorhus/pure

#
# Vi mode
#

bindkey -v
export KEYTIMEOUT=1
bindkey -v '^?' backward-delete-char # [Backspace] - 'normal' lmao
bindkey '^[[1;5C' forward-word  # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word # [Ctrl-LeftArrow] - move backward one word

# To get better c-w
# From https://unix.stackexchange.com/questions/250690/how-to-configure-ctrlw-as-delete-word-in-zsh
my-backward-delete-word() {
    local WORDCHARS=${WORDCHARS/\//}
    zle backward-delete-word
}
zle -N my-backward-delete-word
bindkey '^W' my-backward-delete-word

#
# Plugin config etc
#

export PURE_PROMPT_SYMBOL='%B>%b'
export PURE_PROMPT_VICMD_SYMBOL='%B<%b'

# history substring on up/down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

#
# Couple of useful functions & aliases
#

ssh-up() {
  if [ -z "$SSH_AUTH_SOCK" ] ; then
    eval `ssh-agent -s`
    ssh-add
  fi
}

dcomp() {
   docker-compose -f $1 --project-directory . $2 "${@:3}"
}

bigboyseason() {
    if [[ $1 = "undo" ]]; then
        xrdb -load ~/.Xresources.small
        swaymsg output eDP-1 enable
        rm -f ~/.config/alacritty/alacritty.yml
        ln -s ~/.config/alacritty/alacritty.small.yml ~/.config/alacritty/alacritty.yml
        sudo systemctl stop bluetooth
    else
        xrdb -load ~/.Xresources
        rm -f ~/.config/alacritty/alacritty.yml
        ln -s ~/.config/alacritty/alacritty.big.yml ~/.config/alacritty/alacritty.yml
        sudo systemctl start bluetooth
        swaymsg output eDP-1 disable
    fi
}

alias restartemacs='killall emacs && emacs --daemon'

initnvm() {
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
    nvm use
}

# Setup proper term information for emacs ansi-term mode
[[ $TERM == eterm-color ]] && export TERM=xterm
export PERL5LIB=~/perl5/lib/perl5:$PERL5LIB
zstyle ':notify:*' notifier /usr/bin/notify-send
