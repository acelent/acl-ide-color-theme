;;;; Interface

(cl:in-package #:cl-user)

(defpackage #:ide.color-theme
  (:use #:cl)
  (:export #:*foreground-color*
	   #:*background-color*
	   #:all-color-themes
	   #:color-theme-args
	   #:color-theme
	   #:define-color-theme
	   #:remove-color-theme
	   ))

(in-package #:ide.color-theme)


;;;; Configuration

(defvar *foreground-color* nil)

(defvar *background-color* nil)


;;;; Implementation

(defvar *all-color-themes* (make-hash-table :test 'string=))

(defun all-color-themes ()
  (maphash #'(lambda (key value)
	       (declare (ignore value))
	       key)
	   *all-color-themes*))

(defun color-theme-data (theme-name)
  (multiple-value-bind (color-theme-data found?)
      (gethash theme-name *all-color-themes*)
    (if found?
	color-theme-data
	(error "No color theme named ~s found." theme-name))))

(defun color-theme-super-theme-names (theme-name)
  (first (color-theme-data theme-name)))

(defun color-theme-args (theme-name)
  (rest (color-theme-data theme-name)))

(defvar *all-lisp-edit-panes* (make-hash-table :test 'eq
					       :weak-keys t))

(defmacro with-unmodified-pane (pane &body body)
  (let ((pane-v (gensym))
	(modified-v (gensym)))
    `(let* ((,pane-v ,pane)
	    (,modified-v (cg:modified ,pane-v)))
       (unwind-protect
	    (progn ,@body)
	 (setf (cg:modified ,pane-v) ,modified-v)))))

(defun update-editor-pane (pane)
  (when (open-stream-p pane)
    (mp:process-progn (cg:creation-process pane)
      (with-unmodified-pane pane
	(setf (cg:foreground-color pane) *foreground-color*)
	(setf (cg.base:old-forecolor pane) *foreground-color*)
	
	(setf (cg:background-color pane) *background-color*)
	(setf (cg.base:old-backcolor pane) *background-color*)
	
	(unless (typep pane 'ide.base:listener-pane)
	  (let ((recolorize-p (cg:colorize-on-load-file (cg:configuration cg:*system*))))
	    (when recolorize-p
	      (cg:colorize-source-code pane :entire-buffer t :from-position 0)))))))
  (values))

(defun update-editor-panes ()
  (maphash #'(lambda (pane value)
	       (declare (ignore value))
	       (update-editor-pane pane))
	   *all-lisp-edit-panes*)
  (values))

(defun set-color-theme (theme-name)
  (destructuring-bind (&key
		       foreground
		       background
		       characters
		       comments
		       external-allegro-symbols
		       external-cg-symbols
		       external-cl-symbols
		       global-variables
		       strings
		       user-functions
		       parenthesis-matching-color
		       parenthesis-matching-style)
      (color-theme-args theme-name)
    (setf *foreground-color* foreground)
    (setf *background-color* background)
  
    (setf (cg:color-for-characters               (cg:configuration cg:*system*)) characters)
    (setf (cg:color-for-comments                 (cg:configuration cg:*system*)) comments)
    (setf (cg:color-for-external-allegro-symbols (cg:configuration cg:*system*)) external-allegro-symbols)
    (setf (cg:color-for-external-cg-symbols      (cg:configuration cg:*system*)) external-cg-symbols)
    (setf (cg:color-for-external-cl-symbols      (cg:configuration cg:*system*)) external-cl-symbols)
    (setf (cg:color-for-global-variables         (cg:configuration cg:*system*)) global-variables)
    (setf (cg:color-for-strings                  (cg:configuration cg:*system*)) strings)
    (setf (cg:color-for-user-functions           (cg:configuration cg:*system*)) user-functions)
    (setf (cg:parenthesis-matching-color         (cg:configuration cg:*system*)) parenthesis-matching-color)
    (setf (cg:parenthesis-matching-style         (cg:configuration cg:*system*)) parenthesis-matching-style))
  
  theme-name)

(defun color-theme (theme-name)
  (mapc 'set-color-theme (color-theme-super-theme-names theme-name))
  (set-color-theme theme-name)
  
  (update-editor-panes)
  
  theme-name)

(defun define-color-theme (theme-name super-theme-names
			   &rest color-theme-args &key &allow-other-keys)
  (dolist (super-theme-name super-theme-names)
    (multiple-value-bind (color-theme-data found?)
	(gethash super-theme-name *all-color-themes*)
      (declare (ignore color-theme-data))
      (unless found?
	(warn "Inherited color theme ~s not defined." super-theme-name))))
  
  (setf (gethash theme-name *all-color-themes*) (list* super-theme-names color-theme-args))
  
  theme-name)

(defun remove-color-theme (theme-name)
  (remhash theme-name *all-color-themes*))

(excl:without-package-locks
  (defmethod initialize-instance :around ((pane ide.base:ide-lisp-edit-pane) &key &allow-other-keys)
    (multiple-value-prog1
	(call-next-method)
     
      (unless (typep pane 'ide.base::extended-editor-lisp-edit-pane)
	(with-unmodified-pane pane
	  (setf (gethash pane *all-lisp-edit-panes*) pane)
       
	  (when *foreground-color*
	    (setf (cg:foreground-color pane)   *foreground-color*)
	    (setf (cg.base:old-forecolor pane) *foreground-color*))
       
	  (when *background-color*
	    (setf (cg:background-color pane)   *background-color*)
	    (setf (cg.base:old-backcolor pane) *background-color*))))))
 
  (defmethod cg:default-foreground-color ((pane ide.base:ide-lisp-edit-pane))
    (or (and (not (typep pane 'ide.base::extended-editor-lisp-edit-pane))
	     *foreground-color*)
	(call-next-method)))
 
  (defmethod cg:default-background-color ((pane ide.base:ide-lisp-edit-pane))
    (or (and (not (typep pane 'ide.base::extended-editor-lisp-edit-pane))
	     *background-color*)
	(call-next-method)))
 
  (defmethod (setf cg:color) (color (pane ide.base:ide-lisp-edit-pane))
    (call-next-method (or (and (eq color (cg:system-foreground-color))
			       *foreground-color*)
			  color)
		      pane)
    color)
  )


;;;; Initial color themes

(define-color-theme "default" ()
  :foreground nil :background nil
  :characters                 (cg:make-rgb :red 190 :green   0 :blue   0)
  :comments                   cg:dark-green
  :external-allegro-symbols   cg:dark-magenta
  :external-cg-symbols        (cg:make-rgb :red 200 :green 100 :blue   0)
  :external-cl-symbols        (cg:make-rgb :red   0 :green   0 :blue 180)
  :global-variables           (cg:make-rgb :red 240 :green   0 :blue   0)
  :strings                    (cg:make-rgb :red 190 :green   0 :blue   0)
  :user-functions             nil
  :parenthesis-matching-color cg:green
  :parenthesis-matching-style :color-block)

(define-color-theme "plain" ()
  :foreground nil :background nil
  :characters                 nil
  :comments                   nil
  :external-allegro-symbols   nil
  :external-cg-symbols        nil
  :external-cl-symbols        nil
  :global-variables           nil
  :strings                    nil
  :user-functions             nil
  :parenthesis-matching-color cg:green
  :parenthesis-matching-style :off)

(define-color-theme "emacs" ()
  :foreground cg:black :background cg:white
  :characters                 nil
  :comments                   (cg:make-rgb :red 102 :green 102 :blue 102)
  :external-allegro-symbols   cg:dark-red
  :external-cg-symbols        cg:dark-green
  :external-cl-symbols        (cg:make-rgb :red 160 :green  32 :blue 240)
  :global-variables           (cg:make-rgb :red 184 :green 134 :blue  11)
  :strings                    (cg:make-rgb :red 188 :green 143 :blue 143)
  :user-functions             cg:blue
  :parenthesis-matching-color (cg:make-rgb :red  64 :green 224 :blue 255)
  :parenthesis-matching-style :color-block)

(define-color-theme "torte" ()
  :foreground                 (cg:make-rgb :red 204 :green 204 :blue 204)
  :background                 (cg:make-rgb :red   0 :green   0 :blue   0)
  :characters                 (cg:make-rgb :red 255 :green 160 :blue 160)
  :comments                   (cg:make-rgb :red 128 :green 160 :blue 255)
  :external-allegro-symbols   (cg:make-rgb :red 255 :green 128 :blue 255)
  :external-cg-symbols        (cg:make-rgb :red 128 :green 255 :blue 128)
  :external-cl-symbols        (cg:make-rgb :red 255 :green 255 :blue   0)
  :global-variables           (cg:make-rgb :red 255 :green 255 :blue 255)
  :strings                    (cg:make-rgb :red 255 :green 160 :blue 160)
  :user-functions             (cg:make-rgb :red   0 :green 255 :blue 255)
  :parenthesis-matching-color (cg:make-rgb :red 160 :green  32 :blue 240)
  :parenthesis-matching-style :parenthesis-color)


;;; Show presence when loaded
(pushnew :ide-color-theme *features*)

