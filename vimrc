" vim: foldmethod=marker fenc=utf-8 tw=80 sw=2 sts=2 :
scriptencoding utf-8
set nocompatible        " Use Vim defaults (much better!)
" Bundle configuration {{{1
filetype off
set rtp+=~/.vim/bundle/vundle/
let g:vundle_default_git_proto = 'git'
call vundle#rc()
" Bundles {{{1
Bundle 'gmarik/vundle'
Bundle 'L9'
Bundle 'FuzzyFinder'
Bundle 'scrooloose/syntastic'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-commentary'
Bundle 'jcf/vim-latex'
Bundle 'Shougo/neocomplete.vim'
Bundle 'Shougo/neosnippet.vim'
Bundle 'Shougo/unite.vim'
Bundle 'honza/vim-snippets'
" Bundle 'bling/vim-airline'
Bundle 'bling/vim-bufferline'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'ack.vim'
Bundle 'TaskList.vim'
Bundle 'sjl/gundo.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'rking/ag.vim'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'klen/python-mode'
Bundle 'kana/vim-fakeclip'
" General configuration {{{1
filetype plugin indent on
set shell=/bin/bash
set laststatus=2
set encoding=utf-8
set backspace=indent,eol,start       " Allow backspacing over everything in
                                     " insert mode
set ai                               " Always set auto-indenting on
set history=50                       " keep 50 lines of command history
set ruler                            " Show the cursor position all the time
set relativenumber
set number
set clipboard=unnamed
set showmatch
set incsearch
set showcmd
set wildmenu
set viminfo='20,\"500   " Keep a .viminfo file.
set guifont=PragmataPro\ for\ Powerline:h20
set expandtab
set shiftwidth=4
set tabstop=4
set smarttab
set shiftround
set textwidth=80
set hidden
filetype plugin on
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
augroup texAngleConceal
  autocmd!
  au BufNewFile,BufRead *.tex syn match texMathSymbol '\\langle\>' contained conceal cchar=⟨
  au BufNewFile,BufRead *.tex syn match texMathSymbol '\\rangle\>' contained conceal cchar=⟩
augroup END
set background=light
colo solarized
augroup colorschemeConceal
  autocmd!
  au Colorscheme * hi! link Conceal Normal
augroup END
" General shortcuts {{{1
let mapleader=","
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <silent> <leader>sv :so $MYVIMRC<cr>
nnoremap <silent> <leader>pp :!python3 %<cr>
nnoremap <silent> <leader>p2 :!python %<cr>
" swap arguments separated by commas in function calls
nnoremap <silent> <leader>sw "qdt,dweP"qpb
" quickly switch between alternate buffers
nnoremap <leader>; :e #<cr>
" refactor a numberless environment to one with a label
nnoremap <silent> <leader>ds :call DeleteStar()<cr>j:call AddLabel()<cr>i
function! SearchStartOfEnv()
  execute "normal! ?begin\<CR>"
endfunction
function! DeleteStar()
  call SearchStartOfEnv()
  execute "normal! f*xb*;xNN^"
endfunction
function! AddLabel()
  call SearchStartOfEnv()
  execute "normal! %%w*O\\label{}\<ESC>"
endfunction
" add pairs of brackets in latex
nnoremap <silent> <leader>ap i\left<esc>%i\right<esc>
nnoremap <silent> <leader>abp i\bigl<esc>%i\bigr<esc>
" Correctly indent begin / end environments in LaTeX, cursor has to be at '\' of
" '\begin'
function! LatexIndent()
    normal i%%*EaNBiNEaNVndk]pjVnkdN]pVnk>gvQnN/\\begin
endfunction
nnoremap <leader>ind :call LatexIndent()<CR>
" Compiling with biblatex, --output_safechars supports special chars like 'ß'
nnoremap <leader>lb :!biber --output_safechars %:r<CR>
" Making glossaries
nnoremap <leader>lg :!makeglossaries %:r<CR>
" Easy folding toggle with Space
nnoremap <Space> za
set pastetoggle=<F2>
nnoremap ; :
vnoremap ; :
nnoremap ;; ;
nmap <silent> <leader>/ ;nohlsearch<CR>
map Q gq
set viewoptions-=options
" augroup vimrc
    " autocmd BufWritePost *
    " \   if expand('%') != '' && &buftype !~ 'nofile'
    " \|      mkview
    " \|  endif
    " autocmd BufRead *
    " \   if expand('%') != '' && &buftype !~ 'nofile'
    " \|      silent loadview
    " \|  endif
" augroup END
" Plugin Shortcuts and Configuration {{{1
" neocomplete {{{2
let g:acp_enableAtStartup = 0
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smartcase = 1
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
inoremap <expr><C-g> neocomplete#undo_completion()
inoremap <expr><C-g> neocomplete#complete_common_string()
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplete#close_popup()
inoremap <expr><C-e>  neocomplete#cancel_popup()
" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
let g:neocomplete#manual_completion_start_length = 2
"" neosnippet {{{2
" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : neocomplete#start_manual_complete()
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1

" Tell Neosnippet about the other snippets
let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
" For snippet_complete marker.
if has('conceal')
  set conceallevel=2
endif
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
"" NerdTree {{{2
nmap <silent> <Leader>nt :NERDTreeToggle<CR>
"" FuzzyFinder {{{2
nmap <silent> <Leader>tf :FufFile<CR>
nmap <silent> <Leader>td :FufDir<CR>
nmap <silent> <Leader>tl :FufLine<CR>
"" Latex-suite {{{2
imap <C-space> <Plug>IMAP_JumpForward
"" vim-airline {{{2
let g:airline_powerline_fonts = 1
"" vim-bufferline {{{2
let g:bufferline_echo = 0
" autocmd VimEnter *
"     \ let &statusline='%{bufferline#refresh_status()}'
"     \ .bufferline#get_status_string()
let g:bufferline_rotate = 1
" }}}
" synastic {{{2
let g:syntastic_mode_map = { 'mode': 'active',
                            \ 'active_filetypes': ['php', 'python'],
                            \ 'passive_filetypes': ['tex'] }
" }}}
" " powerline {{{2
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup
set noshowmode
if ! has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
  augroup END
endif
" }}}
" " Python-mode {{{2
let g:pymode_rope = 1
let g:pymode_doc = 1
let g:pymode_doc_key = 'K'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_indent_errors = g:pymode_syntax_all
let g:pymode_syntax_space_errors = g:pymode_syntax_all
" }}}
" " ropevim {{{2
" }}}}}}
" Tab ignore and number column width {{{
" When doing tab completion, give the following files lower priority. You may
" wish to set 'wildignore' to completely ignore files, and 'wildmenu' to enable
" enhanced tab completion. These can be done in the user vimrc file.
set suffixes+=.info,.aux,.log,.dvi,.bbl,.out,.o,.lo
set wildignore=*.info,*.aux,*.log,*.dvi,*.bbl,*.out,*.o,*.lo,*.blg,*.pdf,*.pdfsync,*.bcf,*.gz

" When displaying line numbers, don't use an annoyingly wide number column. This
" doesn't enable line numbers -- :set number will do that. The value given is a
" minimum width to use for the number column, not a fixed size.
if v:version >= 700
  set numberwidth=3
endif
" }}}
" {{{ Modeline settings
" We don't allow modelines by default. See bug #14088 and bug #73715.
" If you're not concerned about these, you can enable them on a per-user
" basis by adding "set modeline" to your ~/.vimrc file.
set modeline
" }}}
" {{{ Locale settings
" Try to come up with some nice sane GUI fonts. Also try to set a sensible
" value for fileencodings based upon locale. These can all be overridden in
" the user vimrc file.
if v:lang =~? "^ko"
  set fileencodings=euc-kr
  set guifontset=-*-*-medium-r-normal--16-*-*-*-*-*-*-*
elseif v:lang =~? "^ja_JP"
  set fileencodings=euc-jp
  set guifontset=-misc-fixed-medium-r-normal--14-*-*-*-*-*-*-*
elseif v:lang =~? "^zh_TW"
  set fileencodings=big5
  set guifontset=-sony-fixed-medium-r-normal--16-150-75-75-c-80-iso8859-1,-taipei-fixed-medium-r-normal--16-150-75-75-c-160-big5-0
elseif v:lang =~? "^zh_CN"
  set fileencodings=gb2312
  set guifontset=*-r-*
endif
" If we have a BOM, always honour that rather than trying to guess.
if &fileencodings !~? "ucs-bom"
  set fileencodings^=ucs-bom
endif
" let g:Tex_TreatMacViewerAsUNIX='1'
" let g:Tex_ViewRuleComplete_pdf='/usr/bin/open -a Skim $*.pdf'
let g:Tex_ViewRule_pdf='Skim'
let g:Tex_CompileRule_pdf = 'lualatex -synctex=1 --interaction=nonstopmode $*'

" Always check for UTF-8 when trying to determine encodings.
if &fileencodings !~? "utf-8"
  " If we have to add this, the default encoding is not Unicode.
  " We use this fact later to revert to the default encoding in plaintext/empty
  " files.
  let g:added_fenc_utf8 = 1
  set fileencodings+=utf-8
endif

" Make sure we have a sane fallback for encoding detection
if &fileencodings !~? "default"
  set fileencodings+=default
endif
" }}}
" {{{ Syntax highlighting settings
" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
" }}}
" {{{ Terminal fixes
if &term ==? "xterm"
  set t_Sb=^[4%dm
  set t_Sf=^[3%dm
  set ttymouse=xterm2
endif

if &term ==? "gnome" && has("eval")
  " Set useful keys that vim doesn't discover via termcap but are in the
  " builtin xterm termcap. See bug #122562. We use exec to avoid having to
  " include raw escapes in the file.
  exec "set <C-Left>=\eO5D"
  exec "set <C-Right>=\eO5C"
endif
" }}}
" {{{ Filetype plugin settings
" Enable plugin-provided filetype settings, but only if the ftplugin
" directory exists (which it won't on livecds, for example).
if isdirectory(expand("$VIMRUNTIME/ftplugin"))
  filetype plugin on

  " Uncomment the next line (or copy to your ~/.vimrc) for plugin-provided
  " indent settings. Some people don't like these, so we won't turn them on by
  " default.
  filetype indent on
endif
" }}}
" {{{ Fix &shell, see bug #101665.
if "" == &shell
  if executable("/bin/bash")
    set shell=/bin/bash
  elseif executable("/bin/sh")
    set shell=/bin/sh
  endif
endif
"}}}
" {{{ Our default /bin/sh is bash, not ksh, so syntax highlighting for .sh
" files should default to bash. See :help sh-syntax and bug #101819.
if has("eval")
  let is_bash=1
endif
" }}}
" {{{ Autocommands
if has("autocmd")

augroup gentoo
  au!

  " Gentoo-specific settings for ebuilds.  These are the federally-mandated
  " required tab settings.  See the following for more information:
  " http://www.gentoo.org/proj/en/devrel/handbook/handbook.xml
  " Note that the rules below are very minimal and don't cover everything.
  " Better to emerge app-vim/gentoo-syntax, which provides full syntax,
  " filetype and indent settings for all things Gentoo.
  au BufRead,BufNewFile *.e{build,class} let is_bash=1|setfiletype sh
  au BufRead,BufNewFile *.e{build,class} set ts=4 sw=4 noexpandtab

  " In text files, limit the width of text to 78 characters, but be careful
  " that we don't override the user's setting.
  autocmd BufNewFile,BufRead *.txt
        \ if &tw == 0 && ! exists("g:leave_my_textwidth_alone") |
        \     setlocal textwidth=78 |
        \ endif

  " When editing a file, always jump to the last cursor position
  autocmd BufReadPost *
        \ if ! exists("g:leave_my_cursor_position_alone") |
        \     if line("'\"") > 0 && line ("'\"") <= line("$") |
        \         exe "normal g'\"" |
        \     endif |
        \ endif

  " When editing a crontab file, set backupcopy to yes rather than auto. See
  " :help crontab and bug #53437.
  autocmd FileType crontab set backupcopy=yes

  " If we previously detected that the default encoding is not UTF-8
  " (g:added_fenc_utf8), assume that a file with only ASCII characters (or no
  " characters at all) isn't a Unicode file, but is in the default encoding.
  " Except of course if a byte-order mark is in effect.
  autocmd BufReadPost *
        \ if exists("g:added_fenc_utf8") && &fileencoding == "utf-8" &&
        \    ! &bomb && search('[\x80-\xFF]','nw') == 0 && &modifiable |
        \       set fileencoding= |
        \ endif

augroup END

endif " has("autocmd")
" }}}
" {{{ vimrc.local
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif
" }}}
