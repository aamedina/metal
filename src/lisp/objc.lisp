(in-package :metal)

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
