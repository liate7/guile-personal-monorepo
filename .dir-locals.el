((scheme-mode (eval
			   . (with-eval-after-load 'geiser-guile
				   (let ((root-dir (file-name-directory
									(locate-dominating-file default-directory ".dir-locals.el"))))
					 (require 'cl-lib)
					 (cl-pushnew root-dir geiser-guile-load-path :test #'string-equal))))
			  (eval
			   . (put 'set-fields 'scheme-indent-function 'defun))
			  (eval
			   . (put 'call-with-open-mpd-port 'scheme-indent-function 1))
			  (eval
			   . (put 'define-command 'scheme-indent-function 1))))
