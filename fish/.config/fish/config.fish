set -g hydro_symbol_prompt ▼
fish_vi_key_bindings
source ~/.asdf/asdf.fish

function fish_mode_prompt
  echo ''
end

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

function clean_branch -d "Remove current git branch + checkout main/master"
    set -f main_branch main

    git rev-parse --verify master
    if test $status = 0
        set -f main_branch master
    end

    set -f delete_branch (git branch --show-current)

    if test $delete_branch = $main_branch
        echo "Cant clean main branch!"
        return
    end

    git checkout master
    git pull origin master
    git branch -D $delete_branch
end

function plexup --description 'Mount drive + start plex'
    sudo mount /dev/sda1 /mnt/wddata
    sudo systemctl start plexmediaserver
end

function testup
  docker-compose -f docker/docker-compose.test.yml -p (basename $PWD) up -d
end

function testdown
    docker-compose -f docker/docker-compose.test.yml -p (basename $PWD) down -v
end

complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)'
