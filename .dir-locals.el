;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode . ((eval . (with-eval-after-load 'lsp-haskell
                            (setq-local lsp-haskell-formatting-provider "fourmolu"))))))
