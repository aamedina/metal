(in-package :metal)

(objc:define-objc-class metal-kit-view ()
  ()
  (:objc-class-name "CLMTKView")
  (:objc-superclass-name "MTKView"))

(objc:define-objc-method ("initWithFrame:" objc:objc-object-pointer)
    ((self metal-kit-view)
     (frame cocoa:ns-rect))
  (when-let (self (objc:invoke (objc:current-super) "initWithFrame:" frame))
    (objc:invoke self "setDevice:" (create-system-default-device))
    self))

(objc:define-objc-method ("drawRect:" :void)
    ((self metal-kit-view)
     (rect cocoa:ns-rect))
  (declare (ignore self rect)))

(objc:define-objc-method ("setFrameSize:" :void)
    ((self metal-kit-view)
     (size cocoa:ns-size))
  (objc:invoke (objc:current-super) "setFrameSize:" size))
