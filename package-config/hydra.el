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
  ("u" (hydra--universal-argument current-prefix-arg) "C-u" :color red)
  ("q" nil "quit"))

;;; jakemacs hydra

(pretty-hydra-define jakemacs (:color blue
                               :body-pre (jakemacs-hydra-reset)
                               :inherit (jakemacs-base/heads)
                               :idle 0.5)
  ("jakemacs"
  (("a" (jakemacs-open-hydra jakemacs-applications/body) "Applications"))))

(pretty-hydra-define jakemacs-applications (:hint nil
                                            :color blue
                                            :inherit (jakemacs-base/heads))
  ("Browser"
    (("a" helm-M-x "M-x"))))
