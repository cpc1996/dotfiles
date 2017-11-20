# zgen {{{
# TODO: Auto clone zgen

# load zgen
export ZGEN_RESET_ON_CHANGE=($HOME/.zshrc)
source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then
  echo "Creating a zgen save"

  # prezto options
  zgen prezto editor key-bindings 'emacs'
  zgen prezto prompt theme 'sorin'
  zgen prezto '*:*' color 'yes'
  zgen prezto tmux:auto-start local 'yes'

  # prezto and modules
  zgen prezto
  zgen prezto git
  zgen prezto tmux
  zgen prezto command-not-found
  zgen prezto syntax-highlighting
  zgen prezto history-substring-search
  zgen prezto autosuggestions
  
  # plugins
  zgen load piotryordanov/fzf-mpd
  zgen load changyuheng/zsh-interactive-cd zsh-interactive-cd.plugin.zsh
  zgen load clvv/fasd

  # generate the init script from plugins above
  zgen save
fi
# }}}

# Environment variables {{{
export TERM=xterm-256color
export EDITOR=vim
# }}}

# fasd {{{
export PATH=$PATH:${HOME}/.zgen/clvv/fasd-master
eval "$(fasd --init auto)"
# }}}

# dotfiles manager {{{
# https://news.ycombinator.com/item?id=11070797
alias config='git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
# }}}

# fzf {{{

# Auto-completion and Key bindings {{{
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh
# }}}

# fasd {{{
# fasd & fzf change directory - open best matched file using `fasd` if given argument, filter output of `fasd` using `fzf` else
v() {
    [ $# -gt 0 ] && fasd -f -e ${EDITOR} "$*" && return
    local file
    file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && ${EDITOR} ${file} || return 1
}
unalias z
# fasd & fzf change directory - jump using `fasd` if given argument, filter output of `fasd` using `fzf` else
z() {
    [ $# -gt 0 ] && fasd_cd -d "$*" && return
    local dir
    dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}
# }}}

# fkill - kill process {{{
fkill() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}
# }}}

# pass {{{
# Usage:
#   pass **<TAB>
#   pass -c **<TAB>
# pass completion suggested by @d4ndo (#362)
_fzf_complete_pass() {
  _fzf_complete '+m' "$@" < <(
    pwdir=${PASSWORD_STORE_DIR-~/.password-store/}
    stringsize="${#pwdir}"
    find "$pwdir" -name "*.gpg" -print |
        cut -c "$((stringsize + 1))"-  |
        sed -e 's/\(.*\)\.gpg/\1/'
  )
}
[ -n "$BASH" ] && complete -F _fzf_complete_pass -o default -o bashdefault pass
# }}}

# mpd {{{
fmpc() {
  local song_position
  song_position=$(mpc -f "%position%) %artist% - %title%" playlist | \
    fzf-tmux --query="$1" --reverse --select-1 --exit-0 | \
    sed -n 's/^\([0-9]\+\)).*/\1/p') || return 1
  [ -n "$song_position" ] && mpc -q play $song_position
}
# }}}

# Opening files {{{
# Modified version where you can press
#   - CTRL-O to open with `open` command,
#   - CTRL-E or Enter key to open with the $EDITOR
fo() {
  local out file key
  IFS=$'\n' out=($(fzf-tmux --query="$1" --exit-0 --expect=ctrl-o,ctrl-e))
  key=$(head -1 <<< "$out")
  file=$(head -2 <<< "$out" | tail -1)
  if [ -n "$file" ]; then
    [ "$key" = ctrl-o ] && open "$file" || ${EDITOR:-vim} "$file"
  fi
}
alias fe='EDITOR=emacs fo'
# }}}

# }}}

neofetch
