" Remap space as leader key
noremap <Space> <Nop>
let mapleader = " "
let mapleader = " "

" Change window focus
nnoremap <C-h> <cmd>wincmd h<CR>
nnoremap <C-j> <cmd>wincmd j<CR>
nnoremap <C-k> <cmd>wincmd k<CR>
nnoremap <C-l> <cmd>wincmd l<CR>

" Resize window
nnoremap <C-Up> <cmd>resize +2<CR>
nnoremap <C-Down> <cmd>resize -2<CR>
nnoremap <C-Left> <cmd>vertical resize -2<CR>
nnoremap <C-Right> <cmd>vertical resize +2<CR>

" Quick saving and quitting
nnoremap <leader>w <cmd>write<CR>
nnoremap <leader>q <cmd>quit<CR>
nnoremap <leader>Q <cmd>wqall<CR>

" Copy whole buffer
nnoremap <leader>y <cmd>%yank+<CR>

" Keep selected selection after indenting
vnoremap < <gv
vnoremap > >gv

" Move line up and down
" For some reason using '<cmd>' gives unexpected behavior here
vnoremap J :move '>+1<CR>gv=gv        " Fix indent
vnoremap K :move '>-2<CR>gv=gv        " Fix indent
vnoremap <A-J> :move '>+1<CR>gv-gv    " Don't fix indent
vnoremap <A-K> :move '>-2<CR>gv-gv    " Don't fix indent

" Paste and keep in register
vnoremap p '"_dP'

" Cycle buffers
nnoremap <A-l> :bnext<CR>
nnoremap <A-h> :bprevious<CR>
