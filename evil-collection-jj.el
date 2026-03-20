;;; evil-collection-jj.el --- Evil bindings for jj-mode -*- lexical-binding: t; -*-

(require 'evil-collection)
(require 'jj-mode nil t)

(defvar jj-mode-map)

(defconst evil-collection-jj-maps '(jj-mode-map))

;;;###autoload
(defun evil-collection-jj-setup ()
  (interactive)
  "Set up Evil bindings for `jj-mode`."
  (evil-set-initial-state 'jj-mode 'normal)

  (evil-collection-define-key 'normal 'jj-mode-map
    ;; Navigation (standard evil-collection-magit style)
    "j" 'evil-next-line
    "k" 'evil-previous-line
    (kbd "C-j") 'magit-section-forward
    (kbd "C-k") 'magit-section-backward
    "gj" 'magit-section-forward-sibling
    "gk" 'magit-section-backward-sibling
    "[" 'magit-section-backward-sibling
    "]" 'magit-section-forward-sibling
    "." 'jj-goto-current

    ;; Section/Buffer Management
    (kbd "RET") 'jj-enter-dwim
    (kbd "TAB") 'magit-section-toggle
    "gr" 'jj-log-refresh
    "gR" 'jj-log-refresh
    "q" 'quit-window

    ;; Basic Operations
    "c" 'jj-commit
    "e" 'jj-edit-changeset
    "u" 'jj-undo
    "N" 'jj-new-transient
    "d" 'jj-describe
    "a" 'jj-abandon
    "x" 'jj-abandon ; Magit convention: x for delete/discard

    ;; Squash (Faithful to jj-mode s/S keys)
    "s" 'jj-squash-transient
    "S" 'jj-squash-into-parent

    ;; Advanced Operations / Transients
    "b" 'jj-bookmark-transient
    "r" 'jj-rebase-transient

    ;; Experimental / Diff
    "D" 'jj-diff
    "E" 'jj-diffedit-emacs
    "M" 'jj-diffedit-smerge
    "?" 'jj-mode-transient))

(provide 'evil-collection-jj)
