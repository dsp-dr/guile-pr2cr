;;; Directory Local Variables for pr2cr Guile project
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((compile-command . "gmake -k ")
         (indent-tabs-mode . nil)
         (fill-column . 80)))
 (scheme-mode . ((eval . (progn
                          (require 'geiser-guile)
                          (setq geiser-active-implementations '(guile))
                          (setq geiser-guile-binary "guile3")
                          (setq geiser-default-implementation 'guile)))
                 (scheme-indent-offset . 2)))
 (org-mode . ((eval . (progn
                       (require 'ob-scheme)
                       (setq org-babel-scheme-command "guile3")
                       (setq org-confirm-babel-evaluate nil)
                       (org-babel-do-load-languages
                        'org-babel-load-languages
                        '((scheme . t)
                          (shell . t)
                          (makefile . t))))))))