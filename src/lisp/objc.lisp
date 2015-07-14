(in-package :metal)

(defun new (class)
  (objc:autorelease (objc:alloc-init-object class)))

(defun release-var (variable)
  (when (boundp variable)
    (objc:release (symbol-value variable))
    (makunbound variable)))

(defun ns-error (err)
  (and (objc:objc-object-from-pointer err)
       (error (objc:invoke-into 'string (fli:dereference err)
                                "localizedDescription"))))

(defmacro with-objc-reference (symbols &body body)
  `(fli:with-dynamic-foreign-objects
       (,@(mapcar (lambda (symbol)
                    (list symbol 'objc:objc-object-pointer))
                  symbols))
     ,@body))

(defmacro define-objc-property ((property class-name) &key (getter property)
                                                           setter
                                                           accessor
                                                           (reader accessor)
                                                           (writer accessor))
  (let ((setter (or setter
                    (string-append "set"
                                   (string-capitalize property :end 1)
                                   ":"))))
    (cond
      (accessor
       `(progn
          (defmethod ,reader ((self capi:cocoa-view-pane))
            (objc:invoke (capi:cocoa-view-pane-view self) ,getter))
          (defmethod ,reader ((self ,class-name))
            (objc:invoke (objc:objc-object-pointer self) ,getter))
          (defmethod (setf ,writer) (new-value (self capi:cocoa-view-pane))
            (objc:invoke (capi:cocoa-view-pane-view self) ,setter new-value))
          (defmethod (setf ,writer) (new-value (self ,class-name))
            (objc:invoke (objc:objc-object-pointer self) ,setter new-value))))
      (reader
       `(progn
          (defmethod ,reader ((self capi:cocoa-view-pane))
            (objc:invoke (capi:cocoa-view-pane-view self) ,getter))
          (defmethod ,reader ((self ,class-name))
            (objc:invoke (objc:objc-object-pointer self) ,getter))))
      (writer
       `(progn
          (defmethod (setf ,writer) (new-value (self capi:cocoa-view-pane))
            (objc:invoke (capi:cocoa-view-pane-view self) ,setter new-value))
          (defmethod (setf ,writer) (new-value (self ,class-name))
            (objc:invoke (objc:objc-object-pointer self) ,setter new-value))))
      (t nil))))
