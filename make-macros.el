


(cl-defmacro mk/generic-toggler (symbol buf
				 &key
				   (comp-form	'(equal buf (buffer-name)))
				   (hide-form	'(bury-buffer))
				   (find-form	'(get-buffer buf))
				   (make-form	'(switch-to-buffer buf))
				   (show-form	'(pop-to-buffer buf))
				   (show-prms	'((dedicated . t))))
  (declare (indent defun))
  `(cl-defun ,symbol (&optional (buf ,buf))
     (interactive)
     (if ,comp-form
	 ,hide-form
       (if-let ((found-buffer ,find-form))
	   (let ((display-buffer-overriding-action ,show-prms))
	     ,show-form)
	 (let ((display-buffer-overriding-action ,show-prms))
	   ,make-form)))))



(cl-defmacro mk/generic-buffer-toggler (symbol buf
					&key
					  (comp-func	#'equal)
					  (hide-func	#'bury-buffer)
					  (find-func	#'get-buffer)
					  (make-func	#'switch-to-buffer)
					  (show-func	#'pop-to-buffer)
					  (show-prms	'((dedicated . t))))
  (declare (indent defun))
  `(mk/generic-toggler ,symbol ,buf
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
				  (show-prms	'((dedicated . t))))
  (declare (indent defun))
  `(mk/generic-buffer-toggler ,symbol ,buf
     :comp-form (,comp-func buf (buffer-name))
     :hide-form (,hide-func)
     :find-form (,find-func buf)
     :make-form (,make-func buf)
     :show-form (,show-func buf)
     :show-prms ,show-prms))

(cl-defmacro mk/generic-file-toggler (symbol buf
				      &key
					(comp-form	'(and (buffer-file-name)
							  (equal (expand-file-name toggle-buffer-name)
							   (expand-file-name (buffer-file-name)))))
					(hide-form	'(bury-buffer))
					(find-form	'(get-buffer buf))
					(make-form	'(find-file buf))
					(show-form	'(pop-to-buffer buf))
					(show-prms	'((dedicated . t))))
  (declare (indent defun))
  `(mk/generic-toggler ,symbol ,buf
     :comp-form ,comp-form
     :hide-form ,hide-func
     :find-form ,find-func buf
     :make-form ,make-func buf
     :show-form ,show-func buf
     :show-prms ,show-prms))


(cl-defmacro mk/generic-file-toggler (symbol buf
				      &key
					(comp-func	(lambda (buf1 buf2)
							  (and buf1
							       (equal (expand-file-name buf1)
								      (expand-file-name buf2)))))
					(hide-func	#'bury-buffer)
					(find-func	#'get-buffer)
					(make-func	#'find-file)
					(show-func	#'pop-to-buffer)
					(show-prms	'((dedicated . t))))
  (declare (indent defun))
  `(mk/generic-buffer-toggler ,symbol ,buf
     :comp-func ,comp-func
     :hide-func ,hide-func
     :find-func ,find-func
     :make-func ,make-func
     :show-func ,show-func
     :show-prms ,show-prms))


(global-set-key (kbd "C-M-z")
		(mk/generic-file-toggler my/scratch-file-toggler1 "~/scratch.org"
		  :show-prms '((display-buffer-in-direction)
			       (direction . left)
			       (window-width . .2)
			       (window-height . .3)
			       (dedicated . t))))

;; (global-set-key (kbd "C-M-z")
;; 		(mk/generic-toggler my/scratch-toggler1 "*nonono*"
;; 		  :show-prms '((display-buffer-in-direction)
;; 			       (direction . left)
;; 			       (window-width . .2)
;; 			       (window-height . .3)
;; 			       (dedicated . t))))

(global-set-key (kbd "C-z")
		(mk/generic-buffer-toggler my/scratch-toggler2 "*nonono*"
		  :show-prms '((display-buffer-below-selected)
			       (side . above)
			       (window-width . .3)
			       (window-height . .3)
			       (dedicated . t))))
