(in-package :metal)

(defvar *library-pathname*
  (asdf:system-relative-pathname :metal "src/metal/Shaders.metal"))

(defvar *view*)
(defvar *device*)
(defvar *library*)
(defvar *descriptor*)
(defvar *command-queue*)
(defvar *command-buffer*)
(defvar *command-encoder*)
(defvar *texture-loader*)

(defun compile-metal-library (&optional (lib-pathname *library-pathname*)
                                        (device *device*))
  (when-let (src (file-string lib-pathname))
    (when *library*
      (objc:release *library*)
      (setf *library* nil))
    (fli:with-dynamic-foreign-objects ((err objc:objc-object-pointer))
      (let ((lib (objc:invoke device "newLibraryWithSource:options:error:"
                              src nil err)))
        (or (and (objc:objc-object-from-pointer err)
                 (error (objc:invoke-into 'string (fli:dereference err)
                                          "localizedDescription")))
            (setf *library* lib))))))

(defvar *timers* (make-hash-table))

(defun wrap-file-watcher (pathname callback)
  (let ((last-modified-time (file-write-date pathname)))
    (lambda ()
      (let ((file-modified-time (file-write-date pathname)))
        (when (not (= last-modified-time file-modified-time))
          (setf last-modified-time file-modified-time)
          (funcall callback))))))

(defun watch-file (pathname callback &key (timeout-ms 50))
  (when (null (gethash pathname *timers*))
    (let* ((watcher (wrap-file-watcher pathname callback))
           (timer (mp:make-timer watcher)))
      (setf (gethash pathname *timers*) timer)
      (mp:schedule-timer-milliseconds timer timeout-ms timeout-ms))))

(defun unwatch-file (pathname)
  (when-let (timer (gethash pathname *timers*))
    (mp:unschedule-timer timer)
    (remhash pathname *timers*)))

(defun auto-compile-metal-library (&optional (lib-pathname *library-pathname*)
                                             (device *device*))
  (compile-metal-library lib-pathname device)
  (watch-file lib-pathname
              (lambda () (compile-metal-library lib-pathname device))))

(defun make-texture-loader (&optional (device *device*))
  (let ((self (objc:invoke "MTKTextureLoader" "alloc")))
    (objc:invoke self "initWithDevice:" device)))

(defun initialize-metal ()
  (when (not (boundp '*device*))
    (setf *device* (create-system-default-device))
    (setf *command-queue* (objc:invoke *device* "newCommandQueue"))
    (setf *texture-loader* (make-texture-loader))
    (auto-compile-metal-library)))

(objc:define-objc-class metal-kit-view ()
  ((draw-callback :accessor draw-callback :type function))
  (:objc-class-name "CLMTKView")
  (:objc-superclass-name "MTKView"))

(objc:define-objc-method ("initWithFrame:" objc:objc-object-pointer)
    ((self metal-kit-view)
     (frame cocoa:ns-rect))
  (declare (ignore self))
  (when-let (self (objc:invoke (objc:current-super) "initWithFrame:" frame))
    (initialize-metal)
    (objc:invoke self "setDevice:" *device*)
    ;; MTLPixelFormatBGRA8Unorm_sRGB
    (objc:invoke self "setColorPixelFormat:" 81)
    (objc:invoke self "setPreferredFramesPerSecond:" 60)
    self))

(objc:define-objc-method ("drawRect:" :void)
    ((self metal-kit-view)
     (rect cocoa:ns-rect))
  (declare (ignore rect))
  (when-let (*view* (objc:objc-object-pointer self))
    (objc:with-autorelease-pool ()
      (funcall (draw-callback self)))))

(objc:define-objc-method ("setFrameSize:" :void)
    ((self metal-kit-view)
     (size cocoa:ns-size))
  (objc:invoke (objc:current-super) "setFrameSize:" size))

(objc:define-objc-method ("acceptsFirstResponder" objc:objc-bool)
    ((self metal-kit-view))
  t)

(defun metal-view-initializer (draw-callback frame)
  (lambda (pane view)
    (declare (ignore pane))
    (setf (draw-callback (objc:objc-object-from-pointer view)) draw-callback)
    (objc:invoke view "initWithFrame:" frame)))

(defun make-metal-pane (&key (draw-callback #'default-draw-callback)
                             (frame #(0 0 1280 800)))
  (make-instance 'capi:cocoa-view-pane
                 :view-class "CLMTKView"
                 :init-function (metal-view-initializer draw-callback frame)))

(defun default-draw-callback ()
  (let* ((*descriptor* (objc:invoke *view* "currentRenderPassDescriptor")))
    *descriptor*))
