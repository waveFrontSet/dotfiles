;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! org2blog)
(package! direnv)
(package! doct)
(package! ob-mermaid)
(package! lilypond :recipe
  (:host github
   :repo "lilypond/lilypond"
   :files ("elisp/*.el")))
