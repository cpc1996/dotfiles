" Example vimrc has some useful stuffs too.
" source $VIMRUNTIME/vimrc_example.vim
runtime! vimrc_example.vim

set relativenumber

" Mac OS X and Windows: both unnamed and unnamedplus.
" Linux: unnamedplus only.
" This works for both.
" https://stackoverflow.com/a/30691754
set clipboard^=unnamed,unnamedplus

set expandtab
set tabstop=2
set shiftwidth=2

