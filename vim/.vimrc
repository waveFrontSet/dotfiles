" vim: foldmethod=marker fenc=utf-8 tw=80 sw=2 sts=2 :
scriptencoding utf-8
set nocompatible        " Use Vim defaults (much better!)
" Bundle configuration {{{1
filetype off
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
call neobundle#rc(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'
" Bundles {{{1
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc', {
      \ 'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }
NeoBundle 'TaskList.vim'
NeoBundle 'Valloric/YouCompleteMe'
NeoBundle 'ack.vim'
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'godlygeek/tabular'
NeoBundle 'jcf/vim-latex'
NeoBundle 'kana/vim-fakeclip'
NeoBundle 'klen/python-mode'
NeoBundle 'michaeljsmith/vim-indent-object'
NeoBundle 'rking/ag.vim'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'sjl/gundo.vim'
NeoBundle 'taku-o/vim-editexisting-ext'
NeoBundle 'tommcdo/vim-exchange'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'vim-scripts/UltiSnips'
NeoBundle 'waveFrontSet/vim-colors-solarized'
" General configuration {{{1
filetype plugin indent on
NeoBundleCheck
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
syntax on
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
let g:netrw_liststyle=3
set omnifunc=syntaxcomplete#Complete
set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
augroup texAngleConceal
  autocmd!
  au BufNewFile,BufRead *.tex syn match texMathSymbol '\\langle\>' contained conceal cchar=⟨
  au BufNewFile,BufRead *.tex syn match texMathSymbol '\\rangle\>' contained conceal cchar=⟩
augroup END
set conceallevel=2
set background=light
colo solarized
augroup colorschemeConceal
  autocmd!
  au Colorscheme * hi! link Conceal Normal
augroup END
let g:html_indent_inctags="head,html,body,p,head,table,tbody,div,script"
let g:html_indent_script1="inc"
let g:html_indent_style1="inc"
" General shortcuts {{{1
nnoremap <leader>ev :sp $MYVIMRC<cr>
nnoremap <silent> <leader>sv :so $MYVIMRC<cr>
nnoremap <silent> <leader>pp :!python3 %<cr>
nnoremap <silent> <leader>p2 :!python %<cr>
" swap arguments separated by commas in function calls (bla, blubb)
nnoremap <silent> <leader>sw yiwWviwp#viwp
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
" Quickly set filetype to htmldjango so that snippets and syntax hl is available
nnoremap <leader>dj :set filetype=htmldjango<CR>
set pastetoggle=<F2>
noremap Q gq
" allows incsearch highlighting for range commands
cnoremap $t <CR>:t''<CR>
cnoremap $T <CR>:T''<CR>
cnoremap $m <CR>:m''<CR>
cnoremap $M <CR>:M''<CR>
cnoremap $d <CR>:d<CR>``
set viewoptions-=options
" Plugin Shortcuts and Configuration {{{1
" netrw {{{2
nnoremap <silent> <Leader>nt :Explore<CR>
" Latex-suite {{{2
" let g:Tex_TreatMacViewerAsUNIX='1'
" let g:Tex_ViewRuleComplete_pdf='/usr/bin/open -a Skim $*.pdf'
let g:Tex_ViewRule_pdf='Skim'
let g:Tex_CompileRule_pdf = 'lualatex -synctex=1 --interaction=nonstopmode $*'
" synastic {{{2
let g:syntastic_mode_map = { 'mode': 'active',
                            \ 'active_filetypes': ['php', 'python'],
                            \ 'passive_filetypes': ['tex'] }
" }}}
" powerline {{{2
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
" Unite {{{2
nnoremap <leader>ur :<C-u>Unite -start-insert file_rec/async:!<CR>
nnoremap <leader>uf :<C-u>Unite -start-insert file<CR>
nnoremap <leader>ub :<C-u>Unite buffer<CR>
nnoremap <leader>/  :<C-u>Unite grep:.<CR>
if executable('ag')
  " Use ag in unite grep source.
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts =
  \ '--line-numbers --nocolor --nogroup --hidden --ignore ' .
  \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
  let g:unite_source_grep_recursive_opt = ''
elseif executable('pt')
  " Use pt in unite grep source.
  " https://github.com/monochromegane/the_platinum_searcher
  let g:unite_source_grep_command = 'pt'
  let g:unite_source_grep_default_opts = '--nogroup --nocolor'
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack-grep')
  " Use ack in unite grep source.
  let g:unite_source_grep_command = 'ack-grep'
  let g:unite_source_grep_default_opts =
  \ '--no-heading --no-color -a -H'
  let g:unite_source_grep_recursive_opt = ''
endif
" }}}
" UltiSnips {{{2
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
" }}}
" YouCompleteMe {{{2
let g:ycm_key_list_select_completion = ['<C-n>']
let g:ycm_key_list_previous_completion = ['<C-p>']
" }}}
" solarized {{{2
call togglebg#map("<F11>")
" Python-mode {{{2
let g:pymode_rope = 1
" Map keys for autocompletion
let g:pymode_rope_autocomplete_map = '<C-Space>'
" Auto create and open ropeproject
let g:pymode_rope_auto_project = 1
" Enable autoimport
let g:pymode_rope_enable_autoimport = 1
" Auto generate global cache
let g:pymode_rope_autoimport_generate = 1
let g:pymode_rope_autoimport_underlineds = 0
let g:pymode_rope_codeassist_maxfixes = 10
let g:pymode_rope_sorted_completions = 1
let g:pymode_rope_extended_complete = 1
let g:pymode_rope_autoimport_modules = ["os","shutil","datetime"]
let g:pymode_rope_confirm_saving = 1
let g:pymode_rope_global_prefix = "<C-x>p"
let g:pymode_rope_local_prefix = "<C-c>r"
let g:pymode_rope_vim_completion = 1
let g:pymode_rope_guess_project = 1
let g:pymode_rope_goto_def_newwin = ""
let g:pymode_rope_always_show_complete_menu = 0
let g:pymode_doc = 1
let g:pymode_doc_key = 'K'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1
let g:pymode_syntax_indent_errors = g:pymode_syntax_all
let g:pymode_syntax_space_errors = g:pymode_syntax_all
" }}}
" Fugitive {{{2
nnoremap UU :if &diff<bar>diffupdate<bar>else<bar>diffthis<bar>endif<cr>
nnoremap Ud :if &diff<bar>diffupdate<bar>else<bar>Gdiff<bar>endif<cr>
nnoremap Us :Gstatus<cr>
nnoremap Ul :Glog<cr>
nnoremap Up :!git p
" }}}
" }}}
" Tab ignore and number column width {{{
" When doing tab completion, give the following files lower priority. You may
" wish to set 'wildignore' to completely ignore files, and 'wildmenu' to enable
" enhanced tab completion. These can be done in the user vimrc file.
set suffixes+=.info,.aux,.log,.dvi,.bbl,.out,.o,.lo
set wildignore=*.info,*.aux,*.log,*.dvi,*.bbl,*.out,*.o,*.lo,*.blg,*.pdf,*.pdfsync,*.bcf,*.gz,*.fdb_latexmk,*.fls

" When displaying line numbers, don't use an annoyingly wide number column. This
" doesn't enable line numbers -- :set number will do that. The value given is a
" minimum width to use for the number column, not a fixed size.
if v:version >= 700
  set numberwidth=3
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
