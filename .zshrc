# zgen {{{

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
  
  # generate the init script from plugins above
  zgen save
fi

# }}}

export TERM=xterm-256color

# dotfiles manager {{{
# https://news.ycombinator.com/item?id=11070797
alias config='git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
# }}}

# fzf {{{
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

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

# }}}

neofetch
