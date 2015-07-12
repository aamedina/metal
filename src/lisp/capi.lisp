(in-package :metal)

(objc:define-objc-class metal-kit-view ()
  ()
  (:objc-class-name "CLMTKView")
  (:objc-superclass-name "MTKView"))

(objc:define-objc-method ("initWithFrame:" objc:objc-object-pointer)
    ((self metal-kit-view)
     (frame cocoa:ns-rect))
  (declare (ignore self))
  (when-let (self (objc:invoke (objc:current-super) "initWithFrame:" frame))
    (objc:invoke self "setDevice:" (create-system-default-device))
    ;; MTLPixelFormatBGRA8Unorm_sRGB
    (objc:invoke self "setColorPixelFormat:" 81) 
    (objc:invoke self "setPreferredFramesPerSecond:" 60)
    self))

(objc:define-objc-method ("drawRect:" :void)
    ((self metal-kit-view)
     (rect cocoa:ns-rect))
  (declare (ignore self rect)))

(objc:define-objc-method ("setFrameSize:" :void)
    ((self metal-kit-view)
     (size cocoa:ns-size))
  (objc:invoke (objc:current-super) "setFrameSize:" size))

(defun metal-kit-view-initializer (frame)
  (lambda (pane view)
    (declare (ignore pane))
    (objc:invoke view "initWithFrame:" frame)))

(defun make-metal-kit-view-pane (&key (frame #(0 0 1280 800)))
  (make-instance 'capi:cocoa-view-pane
                 :view-class "CLMTKView"
                 :init-function (metal-kit-view-initializer frame)))
