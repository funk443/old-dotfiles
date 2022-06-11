"call plug#begin ('~/.vim/plugged')
"call plug#end ()

set t_Co=256

set encoding=utf-8
set number
set relativenumber
syntax on
set smartindent
set cindent
set cinoptions=>4,n-2,{2,^-2,:2,=2,g0,h2,p5,t0,+2,(0,u0,w1,m1
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set hlsearch

set splitbelow splitright

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <silent> <S-h> :vertical resize +3<CR>
nnoremap <silent> <S-l> :vertical resize -3<CR>
nnoremap <silent> <S-k> : resize +3<CR>
nnoremap <silent> <S-j> : resize -3<CR>
