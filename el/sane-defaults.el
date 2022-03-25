
(require 'undo-tree)
(require 'company)
;;(require 'neotree)

(cua-mode)
(global-undo-tree-mode)
(global-company-mode)

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-y") 'undo-tree-redo)
(global-set-key (kbd "C-f") 'isearch-forward)


(rplaca mouse-wheel-scroll-amount 1)
(setq-default cursor-type 'bar)
(setf create-lockfiles nil
      indent-tabs-mode nil
      initial-buffer-choice t
      require-final-newline t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'prog-mode 'display-line-numbers-mode)
(add-hook 'test-mode 'display-line-numbers-mode)

;;(add-hook 'neotree-mode-hook 'display-line-numbers-mode)
