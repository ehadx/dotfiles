" print the line number in front of each line
set number

" number of spaces that a <Tab> counts for
set tabstop=4

" number of spaces used for auto-indent
set shiftwidth=4

" expand tab to spaces
set expandtab

" enable auto-indent
set autoindent

" perform smart indentation
set smartindent

" :h smarttab
set smarttab

" always show status bar
set laststatus=2

" no error sound when something wrong happens
set noerrorbells

" use visual effect instead
set visualbell

" get rid of -- MODE --
set noshowmode

" reduce vim's updatetime
set updatetime=1000

" enable mouse usage
set mouse=a

" highlight the line of the cursor
set cursorline

" highlight the column 120 to mark where to cut the line
autocmd BufWinEnter * highlight ColorColumn ctermbg=darkred
set colorcolumn=120

" set syntax highlighting on
syntax on

" xcodedark config
let g:xcodedark_green_comments=1
let g:xcodedark_emph_funcs=1

" set default colorscheme
colorscheme xcodedark

" disable all key mappings
let g:gitgutter_map_keys=0

" get hunk summary from vim-gitgutter
function! GitStatus()
    let [a,m,r] = GitGutterGetHunkSummary()
    return printf('+%d ~%d -%d', a, m, r)
endfunction

" lightline config
let g:lightline = {
\   'colorscheme': 'materia',
\   'active': {
\       'left': [
\           [ 'mode', 'paste' ],
\           [ 'gitbranch', 'readonly', 'relativepath', 'gitsummary' ]
\       ],
\       'right': [
\           [ 'lineinfo' ],
\           [ 'percent', 'lineoverlines' ],
\           [ 'fileformat', 'fileencoding', 'filetype' ]
\       ]
\   },
\   'component_function': {
\       'gitbranch': 'FugitiveHead',
\       'gitsummary': 'GitStatus'
\   },
\   'component': {
\       'lineoverlines': '%l/%L'
\   }
\   }
