

(progn						; Tools
  (defmacro defcomp (name &rest funcs)
    "Define NAME as a function composing FUNCS like (f (g (h x)))."
    `(defun ,name (x)
       ,(cl-reduce (lambda (arg fn) `(,fn ,arg))
		   (reverse funcs)
		   :initial-value 'x
		   :from-end nil)))

  ;; (defcomp my/composed-func print buffer-name)
  ;; (my/composed-func (current-buffer))
  ;; (car (map nil #'my/composed-func (buffer-list)))

  )

(progn						; Buffer-Togglers
  (defun my/compare-buffer (buf1 buf2)
    (equal buf1 buf2))

  (cl-defmacro mk/generic--toggler (symbol buf
				    &key
				      (comp-form	'(equal buf (buffer-name)))
				      (hide-form	'(bury-buffer))
				      (find-form	'(get-buffer buf))
				      (make-form	'(switch-to-buffer buf))
				      (show-form	'(pop-to-buffer found-buffer))
				      (show-prms	'((display-buffer-use-some-window)
							  (dedicated . t))))
    (declare (indent defun))
    `(cl-defun ,symbol (&optional (buf ,buf))
       (interactive)
       (if ,comp-form
	   ,hide-form
	 (if-let ((found-buffer ,find-form))
	     (let ((display-buffer-overriding-action ,show-prms))
	       (logd found-buffer)
	       ,show-form)
	   (let ((display-buffer-overriding-action ,show-prms))
	     ,make-form)))))

  )

(progn						; Buffer-Togglers
  (cl-defmacro mk/generic--buffer-toggler (symbol buf
					   &key
					     (comp-func	#'equal)
					     (hide-func	#'bury-buffer)
					     (find-func	#'get-buffer)
					     (make-func	#'switch-to-buffer)
					     (show-func	#'pop-to-buffer)
					     (show-prms	'((display-buffer-use-some-window)
							  (dedicated . t))))
    (declare (indent defun))
    `(mk/generic--toggler ,symbol ,buf
       :comp-form (,comp-func buf (buffer-name))
       :hide-form (,hide-func)
       :find-form (,find-func buf)
       :make-form (,make-func buf)
       :show-form (,show-func buf)
       :show-prms ,show-prms))

  (cl-defmacro mk/buffer-toggler (symbol buf
				  &key
				    (comp-func	#'equal)
				    (hide-func	#'bury-buffer)
				    (find-func	#'get-buffer)
				    (make-func	#'switch-to-buffer)
				    (show-func	#'pop-to-buffer)
				    (show-prms	'((display-buffer-use-some-window)
						  (dedicated . t))))
    (declare (indent defun))
    `(mk/generic--buffer-toggler ,symbol ,buf
       :comp-func ,comp-func
       :hide-func ,hide-func
       :find-func ,find-func
       :make-func ,make-func
       :show-func ,show-func
       :show-prms ',show-prms)))


(progn						; File-Togglers
  (defun my/compare-file (file1 file2)
    (and file1
	 (equal (expand-file-name file1)
		(expand-file-name file2))))

  (cl-defmacro mk/generic--file-toggler (symbol buf
					 &key
					   (comp-func	#'my/compare-file)
					   (hide-func	#'bury-buffer)
					   (find-func	#'get-buffer)
					   (make-func	#'find-file)
					   (show-func	#'pop-to-buffer)
					   (show-prms	'((display-buffer-use-some-window)
							  (dedicated . t))))
    (declare (indent defun))
    `(mk/generic--toggler ,symbol ,buf
       :comp-form (,comp-func ,buf (buffer-file-name))
       :hide-form (,hide-func)
       :find-form (,find-func (expand-file-name ,buf))
       :make-form (,make-func (expand-file-name ,buf))
       :show-form (,show-func ,buf)
       :show-prms ,show-prms))


  (cl-defmacro mk/file-toggler (symbol buf
				&key
				  (comp-func	#'my/compare-file)
				  (hide-func	#'bury-buffer)
				  (find-func	#'get-buffer)
				  (make-func	#'find-file)
				  (show-func	#'pop-to-buffer)
				  (show-prms	'((display-buffer-use-some-window)
						  (dedicated . t))))
    (declare (indent defun))
    `(mk/generic--file-toggler ,symbol ,buf
       :comp-func ,comp-func
       :hide-func ,hide-func
       :find-func ,find-func
       :make-func ,make-func
       :show-func ,show-func
       :show-prms ',show-prms)))

(progn (defun my/compare-dired (buf1 buf2)	; Dired-Togglers
	 (and (derived-mode-p 'dired-mode)
	      (equal
	       (expand-file-name buf1)
	       (expand-file-name buf2))))

       (cl-defmacro mk/generic--dired-toggler (symbol buf
					       &key
						 (comp-func    #'my/compare-dired)
						 (hide-func	#'bury-buffer)
						 (find-func	#'get-buffer)
						 (make-func	#'find-file)
						 (show-func	#'pop-to-buffer)
						 (show-prms	'((display-buffer-use-some-window)
								  (dedicated . t))))
	 (declare (indent defun))
	 `(mk/generic--toggler ,symbol ,buf
	    :comp-form (,comp-func ,buf default-directory)
	    :hide-form (,hide-func)
	    :find-form (,find-func (expand-file-name ,buf))
	    :make-form (,make-func (expand-file-name ,buf))
	    :show-form (,show-func ,buf)
	    :show-prms ,show-prms))



       (cl-defmacro mk/dired-toggler (symbol buf
				      &key
					(comp-func	#'my/compare-dired)
					(hide-func	#'bury-buffer)
					(find-func	#'get-buffer)
					(make-func	#'find-file)
					(show-func	#'pop-to-buffer)
					(show-prms	'((display-buffer-use-some-window)
							  (dedicated . t))))
	 (declare (indent defun))
	 `(mk/generic--dired-toggler ,symbol ,buf
	    :comp-func ,comp-func
	    :hide-func ,hide-func
	    :find-func ,find-func
	    :make-func ,make-func
	    :show-func ,show-func
	    :show-prms ',show-prms)))


(progn						; Term-Togglers
  (defun my/find-term (&optional dir)
    (car (remove-if-not
	  (lambda (bf)
	    (with-current-buffer bf
	      ;; (logd (file-truename default-directory))
	      ;; (logd (file-truename toggle-buffer-name))
	      (and (derived-mode-p 'term-mode) 
		   (equal (file-truename (or dir
					     default-directory))
			  (file-truename default-directory)))))
	  (buffer-list))))

  (defun my/make-term (&optional directory)
    (let ((default-directory (or directory
				 default-directory)))
      (with-current-buffer
	  (if (file-remote-p default-directory)
	      (make-term "tmp/term" "bash" nil "-c"
			 (format "ssh %s@%s -p%s -Y"
				 (tramp-file-name-user (tramp-dissect-file-name default-directory))
				 (tramp-file-name-host (tramp-dissect-file-name default-directory))
				 (or (tramp-file-name-port (tramp-dissect-file-name default-directory)) 22)))
	    (make-term "tmp/term" "bash"))
	(term-char-mode)
	
	(pop-to-buffer (current-buffer))
	;; (keymap-local-set "C-z" #'delete-window-switch-to-prev)
	;; (keymap-local-set "C-x C-y" #'term-paste)
	;; (keymap-local-set "C-S-y" #'term-paste)
	;; (keymap-local-set "C-Y" #'term-paste)
	(add-hook 'after-change-functions
		  (defun my/rename-term-to-path (_ _ _)
		    (rename-buffer (format "*%s*" (if (file-remote-p default-directory)
						      (format "%s@%s -p%s"
							      (tramp-file-name-user (tramp-dissect-file-name default-directory))
							      (tramp-file-name-host (tramp-dissect-file-name default-directory))
							      (or (tramp-file-name-port (tramp-dissect-file-name default-directory)) 22))
						    (file-truename default-directory))) t)) nil t))))

  (defun my/compare-term (dir1 dir2)
    (and (derived-mode-p 'term-mode) 
	 (equal (file-truename dir1)
		(file-truename dir2))))

  (cl-defmacro mk/generic--term-toggler (symbol dir
					 &key
					   (comp-func	#'my/compare-term)
					   (hide-func	#'bury-buffer)
					   (find-func	#'my/find-term)
					   (make-func	#'my/make-term)
					   (show-func	#'pop-to-buffer)
					   (show-prms	'((display-buffer-use-some-window)
							  (dedicated . t))))
    (declare (indent defun))
    `(mk/generic--toggler ,symbol ,dir
       :comp-form (,comp-func (file-truename ,dir) default-directory)
       :hide-form (,hide-func)
       :find-form (,find-func (expand-file-name ,dir))
       :make-form (,make-func ,dir)
       :show-form (,show-func found-buffer)
       :show-prms ,show-prms))

  (cl-defmacro mk/term-toggler (symbol buf
				&key
				  (comp-func	#'my/compare-term)
				  (hide-func	#'bury-buffer)
				  (find-func	#'my/find-term)
				  (make-func	#'my/make-term)
				  (show-func	#'pop-to-buffer)
				  (show-prms	'((display-buffer-use-some-window)
						  (dedicated . t))))
    (declare (indent defun))
    `(mk/generic--term-toggler ,symbol ,buf
       :comp-func ,comp-func
       :hide-func ,hide-func
       :find-func ,find-func
       :make-func ,make-func
       :show-func ,show-func
       :show-prms ',show-prms)))



(progn						; Cyclers
  (cl-defmacro mk/generic--cycler (symbol &key
					    cycle-form
					    (map-form '(buffer-name))
					    (condition-forms nil))
    (declare (indent defun))
    `(cl-defun ,symbol ()
       (interactive)
       (let ((first-item ,map-form)
	     (bread-crumb ,map-form))
	 ,cycle-form
	 (while
	     (and (not (equal ,map-form first-item))
		  (or
		   ,@condition-forms
		   (equal bread-crumb ,map-form)))
	   ,cycle-form))))
  )

(progn						; Future Work
  (cl-defmacro mk/generic--filter (symbol
				   &key
				     (include-form	't)
				     (exclude-form	'nil))
    (declare (indent defun))
    `(cl-defun ,symbol ()
       (interactive)
       (let* ((buffers (buffer-list))
	      (buffers (remove-if-not
			(lambda (buf)
			  ,include-form)
			buffers))
	      (buffers (remove-if
			(lambda (buf)
			  ,exclude-form)
			buffers)))
	 (let ((first-buffer (buffer-name))
	       (bread-crumb (buffer-name)))
	   (next-buffer)
	   (while
               (and (not (equal (buffer-name) first-buffer))
		    (or
		     (popper-display-control-p (get-buffer (buffer-name)))
		     (string-match-p "^ *\\*+ *.* *\\*+ *" (buffer-name))
		     (member (get-buffer-major-mode (buffer-name)) moz/starred-modes)
		     (equal bread-crumb (buffer-name))))
	     (next-buffer)))

	 (map nil #'print buffers))))
  )



(when nil					; Tests
  (global-set-key (kbd "C-z") nil)
  (global-set-key (kbd "C-z C-c")		(mk/generic--cycler my/next-buffer-cycler
						    :cycle-form (switch-to-next-buffer)
						    :map-form (buffer-name)
						    :condition-forms ((popper-display-control-p (get-buffer (buffer-name)))
								      (string-match-p "^ *\\*+ *.* *\\*+ *" (buffer-name))
								      (member (get-buffer-major-mode (buffer-name)) moz/starred-modes))))
  (global-set-key (kbd "C-z C-x")		(mk/generic--cycler my/next-buffer-cycler
						    :cycle-form (switch-to-next-buffer)
						    :map-form (buffer-name)
						    :condition-forms ((popper-display-control-p (get-buffer (buffer-name)))
								      (string-match-p "^ *\\*+ *.* *\\*+ *" (buffer-name))
								      (member (get-buffer-major-mode (buffer-name)) moz/starred-modes))))
  (global-set-key (kbd "C-<tab>")		(mk/generic--cycler my/next-buffer-cycler
						    (switch-to-next-buffer)
						  (popper-display-control-p (get-buffer (buffer-name)))
						  (string-match-p "^ *\\*+ *.* *\\*+ *" (buffer-name))
						  (member (get-buffer-major-mode (buffer-name)) moz/starred-modes)))
  (global-set-key (kbd "C-<iso-lefttab>")	(mk/generic--cycler my/prev-buffer-cycler
						    (switch-to-prev-buffer)
						  (popper-display-control-p (get-buffer (buffer-name)))
						  (string-match-p "^ *\\*+ *.* *\\*+ *" (buffer-name))
						  (member (get-buffer-major-mode (buffer-name)) moz/starred-modes)))
  (global-set-key (kbd "C-M-<tab>")		(mk/generic--cycler my/next-starred-buffer-cycler
						    (switch-to-next-buffer)
						  (popper-display-control-p (get-buffer (buffer-name)))
						  (not (string-match-p "^ *\\*+ *.* *\\*+ *" (buffer-name)))
						  (member (get-buffer-major-mode (buffer-name)) moz/starred-modes)))
  (global-set-key (kbd "C-M-S-<iso-lefttab>")	(mk/generic--cycler my/prev-starred-buffer-cycler
						    (switch-to-prev-buffer)
						  (popper-display-control-p (get-buffer (buffer-name)))
						  (not (string-match-p "^ *\\*+ *.* *\\*+ *" (buffer-name)))
						  (member (get-buffer-major-mode (buffer-name)) moz/starred-modes)))
  (global-set-key (kbd "C-z C-1")		(mk/buffer-toggler my/scratch-buffer-toggler1 "*scratch*"))
  (global-set-key (kbd "C-z C-2")		(mk/buffer-toggler my/scratch-buffer-toggler2 "*scratch*"
						  :show-prms ((display-buffer-at-bottom)
							      (dedicated . t))))
  (global-set-key (kbd "C-z C-3")		(mk/file-toggler my/scratch-file-toggler1 "~/scratch.org"))
  (global-set-key (kbd "C-z C-4")		(mk/file-toggler my/scratch-file-toggler2 "~/scratch.org"
						  :show-prms ((display-buffer-at-bottom)
							      (dedicated . t))))

  (global-set-key (kbd "C-z C-q")		(mk/dired-toggler my/scratch-dir-toggler1 "~/"))
  (global-set-key (kbd "C-z C-w")		(mk/dired-toggler my/scratch-dir-toggler2 "~/"
						  :show-prms ((display-buffer-in-side-window)
							      (dedicated . t))))

  (global-set-key (kbd "C-z C-a")		(mk/term-toggler my/home-term-toggler1 "~/"))
  (global-set-key (kbd "C-z C-s")		(mk/term-toggler my/home-term-toggler2 "~/"
						  :show-prms ((display-buffer-in-side-window)
							      (dedicated . t))))

  (global-set-key (kbd "C-z C-z")		(mk/term-toggler3 my/toggle-ansi-term-buffer default-directory)))
