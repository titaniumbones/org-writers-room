(defgroup org-writers-room nil
  "Book & project-writing setup for org-mode"
  :group 'org-writers-room)

(defcustom org-writers-room-column-width 35
  "Width of the side columns"
  :group 'org-writers-room
  :type 'number)

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-window-name (window name)
  (set-window-parameter window 'name name))

(defun window-with-name (name)
  (window-with-parameter 'name name))
(defun display-buffer-in-main (buffer alist)
  "Try displaying BUFFER in a window named main."
  (let ((main-window (window-with-name "main")))
    (when main-window
      (window--display-buffer
       buffer main-window 'reuse alist display-buffer-mark-dedicated))))

(defun writers-room-windows ()
  "Trying to figure out how to get a nice windows config for a writers room mode"
  (interactive "")
  (setq width org-writers-room-column-width)
  (global-linum-mode 0)
  (delete-other-windows)
  (let* ((main (split-window nil width t))
         (metadata (split-window main (- width) t)))
    (set-window-name (selected-window) "guide")
    (set-window-name main "main")
    (set-window-name metadata "metadata"))
  (select-window (window-with-name "main"))
  (setq my-buffer-name-regex (concat (buffer-name) "-") )
  (writers-room-pop-buffer)
)

(defun writers-room-pop-buffer ()
  "Creates a temporary rule that binds buffer names derived from the current 
   buffer name to window 'main'.  Then alls org-tree-to-indirect-buffer"
  (interactive)
  (let ((display-buffer-alist
         (cons
          `( ,(rx-to-string `(and ,(buffer-name) "-" (zero-or-more digit))) (display-buffer-in-main))
          display-buffer-alist)))
;;    (pop-to-buffer (get-buffer-create   (concat (buffer-name) "-" "1"))))
    (org-tree-to-indirect-buffer))
)

;; set the hooks up
;; for some reason this hook seems to be called when the mode istoggled off, too!  
;; not sure why that would be.  
(setq org-writers-room-hooks nil)
(add-hook 'org-writers-room-hooks 'writers-room-windows)



(define-minor-mode org-writers-room 
  "Toggle Writer's Room Mode.  
Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state."
  ;; initial value
  :init-value nil
  ;;
  ;; The indicator for the mode line.
  :lighter "WriRo"
  ;; minor mode keybindings
  :keymap 
  '(([?\C-c ?\C-x ?b] . writers-room-pop-buffer)
    )
  ;; 
  :group "org-writers-room"
  :global nil
(run-hooks 'org-writers-room-hooks 
  )
)
