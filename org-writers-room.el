(defgroup org-writers-room nil
  "Book & project-writing setup for org-mode"
  :group 'org-writers-room)

(defcustom org-writers-room-column-width 35
  "Width of the side columns"
  :group 'org-writers-room
  :type 'number)

(defcustom org-writers-room-properties '(("Synopsis" . "Put a short summary here") ("Role in Book" . "Describe what you want this section to accomplish") ("Characters" . "who is in this section"))
  "alist of properties to be inserted automatically on heading creation"
  :group 'org-writers-room
  :type 'alist)

;; ORG-WR-META Mode.  
;; Trivial minor mode to display a properties drawer in the metadata window of writers-mode
;; minor mode is probably not necessary, actually.  

(define-minor-mode org-wr-meta
  "Toggle Writer's Room Metadata Window Mode.  
Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state."
  ;; initial value
  :init-value nil
  ;;
  ;; The indicator for the mode line.
  :lighter "WR-Meta"
  ;; minor mode keybindings
  ;; :keymap 
  ;; '(([?\C-c ?\C-x ?b] . org-wr-guide-pop-buffer)
  ;;   )
  ;; 
  :group "org-writers-room"
  :global nil
  (run-hooks 'org-wr-meta-hooks 
	     )
  )

;; narrow the buffer automatically to the properties drawer
(add-hook 'org-wr-meta-hooks 'org-wr-meta-narrow)


(defun org-wr-meta-narrow ()
  "really just a hook to narrow the buffer to the properties drawer of the active heading"
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (search-forward-regexp  ":PROPERTIES:")
    (setq beg (point))
    (search-forward-regexp ":END:")
    (setq end (match-beginning 0))
    (narrow-to-region beg end)
    (show-all)
    )
  )



(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))


;; I'm not using this one currently
(defun toggle-current-window-dedication ()
  "this function allows a key binding to set the dedication value of a
window.  could be useful for sustaining concentration, but I'm not sure I want to impose it on everyone just yet."
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

;; (global-set-key [pause] 'toggle-current-window-dedication)




(defun org-writers-room-windows ()
  "Trying to figure out how to get a nice windows config for a writers room mode. Uses the window-naming funtions defined above."
  (interactive "")
  (setq width org-writers-room-column-width)
  (global-linum-mode 0)
  (delete-other-windows)
  (set-window-dedicated-p (selected-window) t)
  (let* ((main (split-window nil width t))
         (metadata (split-window main (- width) t)))
    (set-window-name (selected-window) "guide")
    (set-window-name main "main")
    (set-window-name metadata "metadata"))
  (select-window (window-with-name "main"))
  (setq my-buffer-name-regex (concat (buffer-name) "-") )
  (org-writers-room-tree-to-indirect-buffer)
)


;; set the hooks up
;; for some reason this hook seems to be called when the mode istoggled off, too!  
;; not sure why that would be.  
(setq org-writers-room-hooks nil)
(add-hook 'org-writers-room-hooks 'org-writers-room-windows)

;; -- BEGIN org-wr-main
;; Trivial minor mode to display a an org node in the main window of writers-mode

(define-minor-mode org-wr-main 
  "Toggle Writer's Room Main Window Mode.  
Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state."
  ;; initial value
  :init-value nil
  ;;
  ;; The indicator for the mode line.
  :lighter "WR-Main"
  ;; minor mode keybindings
  ;; :keymap 
  ;; '(([?\C-c ?\C-x ?b] . org-wr-guide-pop-buffer)
  ;;   )
  ;; 
  :group "org-writers-room"
  :global nil
  (run-hooks 'org-wr-main-hooks 
	     )
  )


;; Several functions to populate each new heading with a properties drawer
;; would also be nice to completely hid the properties drawer after it's created
;; cf hide-show-mode solution here: http://stackoverflow.com/questions/17478260/completely-hide-the-properties-drawer-in-org-mode

;; save 
(defun org-wr-main-heading-hook ()
  "Adds a properties drawer & populates it with several properties.  Intended to be used with org-insert-heading-hook, but is also interactive."
  (interactive)
  (save-excursion
    (dolist (this-property org-writers-room-properties)
      (org-set-property (car this-property) (cdr this-property))
    )))

(defun org-wr-main-property-fns ()
  "adds a hook to org-insert-heading-hook that automatically adds property drawers whenever a heading is created"
  (interactive)
  (make-local-variable 'org-insert-heading-hook)
  (add-to-list 'org-insert-heading-hook 'org-wr-main-heading-hook)
  )

(add-hook 'org-wr-main-hooks 'org-wr-main-property-fns)

;; --- END org-wr-main -------------


;; --- BEGIN org-wr-guide ------------
;; Trivial minor mode to display guide of an org buffer  in the side window of writers-mode

(defvar org-wr-last-meta-buffer nil)
(define-minor-mode org-wr-guide
  "Toggle Writer's Room Guide Window Mode.  
Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state."
  ;; initial value
  :init-value nil
  ;;
  ;; The indicator for the mode line.
  :lighter "WR-guide"
  ;; minor mode keybindings
  :keymap 
  '(([?\C-c ?\C-x ?b] . org-writers-room-tree-to-indirect-buffer)
   ((kbd "<return>") . org-writers-room-tree-to-indirect-buffer) 
    )
  :group "org-writers-room"
  :global nil
  (run-hooks 'org-wr-guide-hooks 
	     )
  )

(define-key org-wr-guide-map (kbd "<return>") 'org-writers-room-tree-to-indirect-buffer) 
;; (add-hook 'org-wr-guide-hooks 'org-wr-side-narrow)


;; 3 helper functions to enable us to work with named windows (important)
;; --------------------------------------------
(defun set-window-name (window name)
  (set-window-parameter window 'name name))

(defun window-with-name (name)
  (window-with-parameter 'name name))


(defun display-buffer-in-named-window (buffer &optional name)
  "Try displaying BUFFER in a window with parameter 'name'.  If name is nil, attempts to save to window named 'main'."
  (if name
      nil
    (setq name "main"))
  (let ((named-window (window-with-name name)))
    (when named-window
      (window--display-buffer
       buffer named-window 'reuse nil display-buffer-mark-dedicated))))

;; --------------------------------------------
;; end named windows

;; indirect buffer creation for org-writers-room
;; stolen from org.el, but had to rewrite to remove cloning
(defun org-writers-room-get-indirect-buffer (&optional buffer newname )
  "create an indirect buffer from current bufer, but do not clone"
  (setq buffer (or buffer (current-buffer)))
  (let ((n 1) (base (buffer-name buffer)) bname)
    (while (buffer-live-p
	    (get-buffer (setq bname (concat base "-" (number-to-string n)))))
      (setq n (1+ n)))
    ;; allow buffer name to be set when called
    ;; this gives more meaningful buffer names in org-writers-room
    (if newname
	(setq bname newname)
      )
   
    (condition-case nil
	;; this is the main diference form org-get-indirect-buffer
	;; final option 'clone is missing
        ;; (make-indirect-buffer buffer bname 'clone)
	(make-indirect-buffer buffer bname )
      (error (make-indirect-buffer buffer bname)))))


;; hacked org-tree-to-indiret-buffer
;; removes several options, imposing the window structure defined earlier
;; on the user to create a "writer's room" hopefully free of distractions
(defun org-writers-room-tree-to-indirect-buffer (&optional  newname)
  "Create first indirect buffer, ibuf, and narrow it to current subtree.  Then create a second indirect guffer, metabuf.  Display these two buffers windows named 'main' and 'metadata'.  
Org options to go up and down levels are not available, nor are options to display in a new frame etc. etc.  "
  (interactive "P")
  ;; this is the work of figuring out the right window & buffer
  (let ((cbuf (current-buffer))
	(cwin (selected-window))
	(pos (point))
	beg end level heading ibuf)
    ;; select the apropriate heading
    (save-excursion
      (org-back-to-heading t)
      (setq beg (point)
	    heading (org-get-heading))
      (org-end-of-subtree t t)
      (if (org-at-heading-p) (backward-char 1))
      (setq end (point)))
    ;; kill indirect buffer if it still exists
    ;; problem for the metadata window creation!!
    (if (buffer-live-p org-last-indirect-buffer)
	(kill-buffer org-last-indirect-buffer))
    (if (buffer-live-p org-wr-last-meta-buffer)
	(kill-buffer  org-wr-last-meta-buffer))
    
    ;; create and display the indirect buffers
    (setq ibuf (org-writers-room-get-indirect-buffer cbuf heading)
	  org-last-indirect-buffer ibuf)
    (setq metabuf (org-writers-room-get-indirect-buffer cbuf (concat heading "-meta") )
	  org-wr-last-meta-buffer metabuf)
    (display-buffer-in-named-window metabuf "metadata")
    (display-buffer-in-named-window ibuf "main")
    ;; set modes and narrow for ibuf & metabuf
    (pop-to-buffer ibuf)
    (narrow-to-region beg end)
    (org-mode)
    (org-wr-main)
    (show-all)
    (goto-char pos)
    (run-hook-with-args 'org-cycle-hook 'all)
    (pop-to-buffer metabuf)
    (narrow-to-region beg end)
    (org-mode)
    (org-wr-meta)
    (show-all)
    (and (window-live-p cwin) (select-window cwin))))
;; --- END org-wr-guide --------------------------


;; --- BEGIN org-writers-room mode
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
  '(([?\C-c ?\C-x ?b] . org-writers-room-tree-to-indirect-buffer)
    )
  ;; 
  :group "org-writers-room"
  :global nil
  (run-hooks 'org-writers-room-hooks )
  )

;; make sure that the original buffer gets guide-mode
(add-hook 'org-writers-room-hooks 'org-wr-guide)

(provide 'org-writers-room)
