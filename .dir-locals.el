((scheme-mode (eval
			   . (with-eval-after-load 'geiser-guile
				   (let ((root-dir (file-name-directory
									(locate-dominating-file default-directory ".dir-locals.el"))))
					 (require 'cl-lib)
					 (cl-pushnew root-dir geiser-guile-load-path :test #'string-equal))))
			  (eval
			   .
			   (setq-local liate/guile-module-root
						   (file-name-directory
							(locate-dominating-file default-directory ".dir-locals.el"))))
			  (eval
			   . (put 'sxml-match 'scheme-indent-function (get 'match 'scheme-indent-function)))
			  (eval
			   . (put 'set-fields 'scheme-indent-function 'defun))
			  (eval
			   . (put 'call-with-open-mpd-port 'scheme-indent-function 1))
			  (eval
			   . (put 'define-command 'scheme-indent-function 1))))
