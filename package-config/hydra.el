;; HEAVILY based on scimax hydra config.

(use-package major-mode-hydra
  :ensure t
  :after treemacs evil
  ;:bind
  ;("SPC" . major-mode-hydra)
)

(defgroup jakemacs-hydra nil
  "Customization for `jakemacs-hydra'."
  :tag "jakemacs-hydra")

(defcustom jakemacs-hydra-key "SPC"
  "Key to which to bind jakemacs/body."
  :type 'string
  :group 'jakemacs-hydra)

(define-key evil-normal-state-map (kbd "SPC") 'jakemacs/body)

;;; utils

;; package for lexical-let
(eval-when-compile (require 'cl))

;; Lexical closure to encapsulate the stack variable.
(lexical-let ((jakemacs-hydra-stack '()))
  (defun jakemacs-hydra-push (expr)
    "Push an EXPR onto the stack."
    (push expr jakemacs-hydra-stack))

  (defun jakemacs-hydra-pop ()
    "Pop an expression off the stack and call it."
    (interactive)
    (let ((x (pop jakemacs-hydra-stack)))
      (when x
	(call-interactively x))))

  (defun jakemacs-hydra ()
    "Show the current stack."
    (interactive)
    (with-help-window (help-buffer)
      (princ "Jakemacs-hydra-stack\n")
      (pp jakemacs-hydra-stack)))

  (defun jakemacs-hydra-reset ()
    "Reset the stack to empty."
    (interactive)
    (setq jakemacs-hydra-stack '())))

(defmacro jakemacs-open-hydra (hydra)
  "Push current HYDRA to a stack.
This is a macro so I don't have to quote the hydra name."
  `(progn
     (jakemacs-hydra-push hydra-curr-body-fn)
     (call-interactively ',hydra)))

(defun jakemacs-hydra-help ()
  "Show help buffer for current hydra."
  (interactive)
  (with-help-window (help-buffer)
    (with-current-buffer (help-buffer)
      (unless (featurep 'emacs-keybinding-command-tooltip-mode)
	(require 'emacs-keybinding-command-tooltip-mode))
      (emacs-keybinding-command-tooltip-mode +1))
    (let ((s (format "Help for %s\n" hydra-curr-body-fn)))
      (princ s)
      (princ (make-string (length s) ?-))
      (princ "\n"))

    (princ (mapconcat
	    (lambda (head)
	      (format "%s%s"
		      ;;  key
		      (s-pad-right 10 " " (car head))
		      ;; command
		      (let* ((hint (if (stringp (nth 2 head))
				       (concat " " (nth 2 head))
				     ""))
			     (cmd (cond
				   ;; quit
				   ((null (nth 1 head))
				    "")
				   ;; a symbol
				   ((symbolp (nth 1 head))
				    (format "`%s'" (nth 1 head)))
				   ((and (listp (nth 1 head))
					 (eq 'jakemacs-open-hydra (car (nth 1 head))))
				    (format "`%s'" (nth 1 (nth 1 head))))
				   ((listp (nth 1 head))
				    (with-temp-buffer
				      (pp (nth 1 head) (current-buffer))
				      (let ((fill-prefix (make-string 10 ? )))
					(indent-code-rigidly
					 (save-excursion
					   (goto-char (point-min))
					   (forward-line)
					   (point))
					 (point-max) 10))
				      (buffer-string)))
				   (t
				    (format "%s" (nth 1 head)))))
			     (l1 (format "%s%s" (s-pad-right 50 " " (car (split-string cmd "\n"))) hint))
			     (s (s-join "\n" (append (list l1) (cdr (split-string cmd "\n"))))))
			(s-pad-right 50 " " s))))
	    (symbol-value
	     (intern
	      (replace-regexp-in-string
	       "/body$" "/heads"
	       (symbol-name  hydra-curr-body-fn))))
	    "\n"))))

(defhydra jakemacs-base (:color blue)
  "base"
  ("," jakemacs-hydra-pop "back" :color blue)
  ("SPC" helm-M-x "M-x")  ; TODO change to HELM-M-x
  ("C-s" save-buffer "Save")
  ("/" undo-tree-undo "undo" :color red)
  ("?" undo-tree-redo "redo" :color red)
  ("8" (switch-to-buffer "*scratch*") "*scratch*")
  ("." jakemacs-dispatch-mode-hydra "Major mode hydras")
  ("u" (hydra--universal-argument current-prefix-arg) "C-u" :color red))

;;; jakemacs hydra

;;! body functions
;! go to window number
(defun goto-window-number (n)
  ; this is to escape treemacs in case you are inside it
  (ace-window)
  ; go left a bunch
  (setq i 9)
  (while (> i 0)
    (ignore-errors (windmove-left))
    (setq i (1- i)))
  ; go right n times
  (while (> n 1)
    (windmove-right)
    (setq n (1- n))))

;! go to treemacs window
(defun goto-treemacs-window ()
  (treemacs-select-window)
  (local-unset-key (kbd "SPC"))
  (local-set-key (kbd "SPC") 'jakemacs/body)
  (local-set-key (kbd "J") 'jakemacs/body))

;; Root
(pretty-hydra-define jakemacs
  (:hint nil :color blue :inherit (jakemacs-base/heads) :idle 0.1)
  ("jakemacs"
   (("a" (jakemacs-open-hydra jakemacs-applications/body) "applications")
    ("b" (jakemacs-open-hydra jakemacs-buffers/body) "buffers")
    ("f" (jakemacs-open-hydra jakemacs-files/body) "files")
    ("F" (jakemacs-open-hydra jakemacs-frames/body) "Frames")
    ("j" (jakemacs-open-hydra jakemacs-jump/body) "fump")
    ("o" (jakemacs-open-hydra jakemacs-org/body) "org")
    ("p" (jakemacs-open-hydra jakemacs-projectile/body) "projectile")
    ("q" (jakemacs-open-hydra jakemacs-quit/body) "quit")
    ("s" (jakemacs-open-hydra jakemacs-search/body) "search")
    ("w" (jakemacs-open-hydra jakemacs-windows/body) "windows"))
   "goto window"
   (("1" (goto-window-number 1) "window 1")
    ("2" (goto-window-number 2) "window 2")
    ("3" (goto-window-number 3) "window 3")
    ("4" (goto-window-number 4) "window 4")
    ("5" (goto-window-number 5) "window 5")
    ("6" (goto-window-number 6) "window 6")
    ("7" (goto-window-number 7) "window 7")
    ("8" (goto-window-number 8) "window 8")
    ("9" (goto-window-number 9) "window 9")
    ("0" (goto-treemacs-window) "treemacs window"))
  )
)

;; Applications
(pretty-hydra-define jakemacs-applications
  (:hint nil :color blue :inherit (jakemacs-base/heads))
  ("Browser"
    (("a" helm-M-x "M-x"))
  )
)

;; Buffers
(pretty-hydra-define jakemacs-buffers
  (:hint nil :color blue :inherit (jakemacs-base/heads))
  ("Temp Header"
    (("b" helm-mini "buffers")
     ("d" kill-this-buffer "kill"))
  )
)


;; File
(pretty-hydra-define jakemacs-files
  (:hint nil :color blue :inherit (jakemacs-base/heads))
  ("Basic"
    (("s" save-buffer "save buffer")
     ("w" write-file "write file")
     ("f" helm-find-files "find file"))
  )
)

;; Projectile
(pretty-hydra-define jakemacs-projectile
  (:hint nil :color blue :inherit (jakemacs-base/heads))
  ("Find File"
    (("f" helm-projectile-find-file "file")
     ("w" projectile-find-file-dwim "file dwim")  ; because d is for dir
     ("p" projectile-find-file-in-directory "file curr dir")  ; p for pwd?
     ("r" projectile-recentf "recent file")
     ("d" projectile-find-dir "dir"))
    "Search/Tags"
    (("a" helm-projectile-ag "ag")
     ("g" ggtags-update-tags "update gtags")
     ("m" projectile-multi-occur "multi-occur"))
    "Buffers"
    (("i" projectile-ibuffer "ibuffer")
     ("b" projectile-switch-to-buffer "switch to buffer")
     ("K" projectile-kill-buffers "kill all buffers"))
    "Cache"
    (("c" projectile-invalidate-cache "clear cache")
     ("x" projectile-remove-known-project "remove project")
     ("X" projectile-cleanup-known-projects "cleanup non-existing projects")
     ("z" projectile-cache-current-file "cache current file"))
    "Other Window"
    (("o" (jakemacs-open-hydra jakemacs-projectile-other/body) "Other Window"))
  )
)

; Projectile Other Window
(pretty-hydra-define jakemacs-projectile-other
  (:hint nil :color blue :inherit (jakemacs-base/heads))
  ("Other Window"
    (("f" projectile-find-file-other-window "file")
     ("w" projectile-find-file-dwim-other-window "file dwim")
     ("d" projectile-find-dir-other-window "dir")
     ("b" projectile-switch-to-buffer-other-window "buffer"))
  )
)
   
;; Quit
(pretty-hydra-define jakemacs-quit
  (:hint nil :color blue :inherit (jakemacs-base/heads))
  ("Quit"
    (("q" save-buffers-kill-emacs "save buffers and quit"))
  )
)

;; Search
(pretty-hydra-define jakemacs-search
  (:hint nil :color blue :inherit (jakemacs-base/heads))
  ("Projectile"
   (("a" helm-projectile-ag "projectile ag"))
   "Avy"
   (("c" avy-goto-char "char")
    ("w" avy-goto-word-1 "word")
    ("l" avy-goto-line "line"))
  )
)

;;! Window functions
;! split frame into n windows
(defun split-n-ways (n)
  (delete-other-windows)
  (while (> n 1)
    (split-window-right)
    (setq n (1- n)))
  (balance-windows))

;! switch to minubuffer
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

;; Windows
(pretty-hydra-define jakemacs-windows
  (:hint nil :color blue :inherit (jakemacs-base/heads))
  ("control"
    (("a" ace-window "ace-window")
     ("d" delete-window "delete window")
     ("b" (switch-to-minibuffer) "minibuffer"))
   "sizing"
    (("[" shrink-window-horizontally "shrink horizontally" :color red)
     ("]" enlarge-window-horizontally "enlarge horizontally" :color red)
     ("{" shrink-window "shrink vertically" :color red)
     ("}" enlarge-window "enlarge vertically" :color red)
    )
   "Split"
    (("1" delete-other-windows "delete other windows")
     ("2" (split-n-ways 2) "2 windows")
     ("3" (split-n-ways 3) "3 windows")
     ("4" (split-n-ways 4) "4 windows")
    )
  )
)
