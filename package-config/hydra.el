;; HEAVILY based on scimax hydra config.

(use-package major-mode-hydra
  :ensure t
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
     (jakemacs-hydra-push hydra-curr-body-fn)  ; TODO function okay for pretty-hydra?
     (call-interactively ',hydra)))  ; TODO fctn okay?

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
  ("x" counsel-M-x "M-x")  ; TODO change to HELM-M-x
  ("C-s" save-buffer "Save")
  ("/" undo-tree-undo "undo" :color red)
  ("\\" undo-tree-redo "redo" :color red)
  ("8" (switch-to-buffer "*scratch*") "*scratch*")
  ("?" jakemacs-hydra-help "Menu help")
  ("." jakemacs-dispatch-mode-hydra "Major mode hydras")
  ("u" (hydra--universal-argument current-prefix-arg) "C-u" :color red))

;;; jakemacs hydra

;; Root
(pretty-hydra-define jakemacs (:color blue
                               :body-pre (jakemacs-hydra-reset)
                               :inherit (jakemacs-base/heads)
                               :idle 0.5)
  ("jakemacs"
   (("a" (jakemacs-open-hydra jakemacs-applications/body) "Applications")
    ("b" (jakemacs-open-hydra jakemacs-buffers/body) "Buffers")
    ("f" (jakemacs-open-hydra jakemacs-files/body) "Files")
    ("F" (jakemacs-open-hydra jakemacs-frames/body) "Frames")
    ("j" (jakemacs-open-hydra jakemacs-jump/body) "Jump")
    ("o" (jakemacs-open-hydra jakemacs-org/body) "Org")
    ("p" (jakemacs-open-hydra jakemacs-projectile/body) "Projects")
    ("q" (jakemacs-open-hydra jakemacs-quit/body) "Quit")
    ("s" (jakemacs-open-hydra jakemacs-search/body) "Search")
    ("w" (jakemacs-open-hydra jakemacs-windows/body) "Windows")
   )
  )
)

;; Applications
(pretty-hydra-define jakemacs-applications (:hint nil
                                            :color blue
                                            :inherit (jakemacs-base/heads))
  ("Browser"
    (("a" helm-M-x "M-x"))
  )
)

;; Buffers
(pretty-hydra-define jakemacs-buffers (:hint nil
				       :color blue
				       :inherit (jakemacs-base/heads))
  ("Temp Header"
    (("b" helm-buffers-list "buffers")
     ("d" kill-buffer "kill"))
  )
)


;; File
(pretty-hydra-define jakemacs-files (:hint nil
				     :color blue
                                     :inherit (jakemacs-base/heads))
  ("Basic"
    (("s" save-buffer "save buffer")
     ("w" write-file "write file")
     ("f" helm-find-files "find file"))
  )
)

;; Quit
(pretty-hydra-define jakemacs-quit (:hint nil
                                    :color blue
                                    :inherit (jakemacs-base/heads))
  ("Quit"
    (("q" save-buffers-kill-emacs "save buffers and quit"))
  )
)

;! Window functions
(defun split-3-ways ()
  (delete-other-windows)
  (split-window-right)
  (split-window-right)
  (balance-windows))

;; Windows
(pretty-hydra-define jakemacs-windows (:hint nil
                                       :color blue
                                       :inherit (jakemacs-base/heads))
  ("control"
    (("a" ace-window "ace-window")
     ("d" delete-window "delete window")
    )
   "sizing"
    (("[" shrink-window-horizontally "shrink horizontally" :color red)
     ("]" enlarge-window-horizontally "enlarge horizontally" :color red)
     ("{" shrink-window "shrink vertically" :color red)
     ("}" enlarge-window "enlarge vertically" :color red)
    )
   "Not sure the header here"
    (("2" delete-other-windows "delete other windows")
     ("3" (split-3-ways) "split 3")
    )
  )
)
