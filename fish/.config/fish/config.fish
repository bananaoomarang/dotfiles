set -g hydro_symbol_prompt ▼
fish_vi_key_bindings
source ~/.asdf/asdf.fish

set -Ux EDITOR vim

if test -z (pgrep ssh-agent)
  eval (ssh-agent -c) > /dev/null
  set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
  set -Ux SSH_AGENT_PID $SSH_AGENT_PID
end

alias poe="poetry run poe"
alias restartemacs="killall emacs && emacs --daemon"

function dstg
    set -l deploy_branch (git branch --show-current)

    git stash
    git fetch
    git checkout $deploy_branch
    git checkout stage
    git reset --hard $deploy_branch
    git push --force-with-lease origin stage
    git checkout $deploy_branch
end

function jwtdecode --description 'Display contents of jwt'
    echo $argv[1] | step-cli crypto jwt inspect --insecure
end

function plexup --description 'Mount drive + start plex'
    sudo mount /dev/sda1 /mnt/wddata
    sudo systemctl start plexmediaserver
end

complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)'
