{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8107" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          monad-par mtl
        ]);
  hls = pkgs.haskell-language-server;
  pls = pkgs.python39Packages.python-lsp-server;
  myVim= pkgs.vim_configurable.customize {
    #name = "vim-with-plugins";
    name = "vi";
    # add here code from the example section
    vimrcConfig={
	customRC = ''
    	  set hidden
	  set mouse=a
    	  set colorcolumn=80
    	  syntax enable
    	  set number
	  set autoindent
	  set hls
          augroup vimrc
            " set python indentation
            au BufNewFile,BufRead *.py,*.rst,*.hs
                \ setlocal tabstop=4|
                \ setlocal softtabstop=4|
                \ setlocal shiftwidth=4|
                \ setlocal expandtab|
                \ setlocal shiftround
            " set js indentation
            au BufNewFile,BufRead *.js,*.html,*.tex
                \ setlocal tabstop=2|
                \ setlocal softtabstop=2|
                \ setlocal shiftwidth=2|
                \ setlocal expandtab|
                \ setlocal shiftround
          augroup END 

	  let g:LanguageClient_serverCommands = { 'haskell': ['haskell-language-server-wrapper', '--lsp'] }
	  nnoremap <F5> :call LanguageClient_contextMenu()<CR>
	  map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
	  map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
	  map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
	  map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
	  map <Leader>lb :call LanguageClient#textDocument_references()<CR>
	  map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
	  map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

	  let g:LanguageClient_serverCommands = {
          \ 'python': ['pylsp']
          \ }
    	'';
    	packages.myVimPackage = with pkgs.vimPlugins; {
    	  # loaded on launch
    	  start = [ LanguageClient-neovim fugitive vim-nix ];
    	  # manually loadable by calling `:packadd $plugin-name`
    	  # opt = [ phpCompletion elm-vim ];
    	  # To automatically load a plugin when opening a filetype, add vimrc lines like:
    	  # autocmd FileType php :packadd phpCompletion
    	};
    };
  }; 
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ myVim hls pls ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
