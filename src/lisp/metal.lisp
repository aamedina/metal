(in-package :metal)

(defvar *library-pathname*
  (asdf:system-relative-pathname :metal "src/metal/Shaders.metal"))

(defvar *view*)
(defvar *device*)
(defvar *library*)
(defvar *descriptor*)
(defvar *drawable*)
(defvar *texture-loader*)

(defvar *command-queue*)
(defvar *command-buffer*)
(defvar *command-encoder*)

(defvar *buffers* (make-hash-table))
(defvar *textures* (make-hash-table))

(defun compile-metal-library (&optional (lib-pathname *library-pathname*)
                                        (device *device*))
  (when-let (src (file-string lib-pathname))
    (when (boundp '*library*)
      (objc:release *library*)
      (makunbound '*library*))
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

(defun make-render-encoder (&optional (command-buffer *command-buffer*)
                                      (descriptor *descriptor*))
  (objc:invoke command-buffer "renderCommandEncoderWithDescriptor:" descriptor))

(defun make-compute-encoder (&optional (command-buffer *command-buffer*))
  (objc:invoke command-buffer "computeCommandEncoder"))

(defun make-blit-encoder (&optional (command-buffer *command-buffer*))
  (objc:invoke command-buffer "blitCommandEncoder"))

(defun make-parallel-render-encoder (&optional (command-buffer *command-buffer*)
                                               (descriptor *descriptor*))
  (objc:invoke command-buffer "parallelRenderCommandEncoderWithDescriptor:"
               descriptor))

(defmacro with-command-encoder (encoder &body body)
  `(let* ((*descriptor* (objc:invoke *view* "currentRenderPassDescriptor"))
          (*command-buffer* (objc:invoke *command-queue* "commandBuffer"))
          (*command-encoder* ,encoder))
     (unwind-protect
          (progn
            (objc:invoke *command-buffer* "enqueue")
            ,@body)
       (objc:invoke *command-buffer* "commit"))))

(defmacro rendering (&body body)
  `(with-command-encoder (make-render-encoder)
     (when-let (*drawable* (objc:invoke *view* "currentDrawable"))
       (unwind-protect
            (progn
              ,@body)
         (objc:invoke *command-buffer* "presentDrawable:" *drawable*)))))

(defmacro computing (&body body)
  `(with-command-encoder (make-compute-encoder)
     ,@body))

(defmacro blitting (&body body)
  `(with-command-encoder (make-blit-encoder)
     ,@body))

(defmacro parallel-rendering (&body body)
  `(with-command-encoder (make-parallel-render-encoder)
     ,@body))

(objc:define-objc-class metal-kit-view ()
  ((draw-callback :accessor draw-callback)
   (offscreen-draw-callback :accessor offscreen-draw-callback))
  (:objc-class-name "CLMTKView")
  (:objc-superclass-name "MTKView"))

(objc:define-objc-method ("initWithFrame:" objc:objc-object-pointer)
    ((self metal-kit-view)
     (frame cocoa:ns-rect))
  (declare (ignore self))
  (when-let (self (objc:invoke (objc:current-super) "initWithFrame:" frame))    
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
      (funcall (offscreen-draw-callback self))
      (rendering
        (funcall (draw-callback self))))))

(objc:define-objc-method ("setFrameSize:" :void)
    ((self metal-kit-view)
     (size cocoa:ns-size))
  (objc:invoke (objc:current-super) "setFrameSize:" size))

(objc:define-objc-method ("acceptsFirstResponder" objc:objc-bool)
    ((self metal-kit-view))
  t)

(defun metal-view-initializer (draw-callback offscreen-draw-callback frame)
  (lambda (pane view)
    (declare (ignore pane))
    (let ((view (objc:invoke view "initWithFrame:" frame))
          (instance (objc:objc-object-from-pointer view)))
      (setf (draw-callback instance) draw-callback)
      (setf (offscreen-draw-callback instance) offscreen-draw-callback)
      (print instance)
      view)))

(defun make-metal-pane (&key (draw-callback #'default-draw-onscreen)
                             (offscreen-draw-callback #'default-draw-offscreen)
                             (frame #(0 0 1280 800)))
  (initialize-metal)
  (make-instance 'capi:cocoa-view-pane
                 :view-class "CLMTKView"
                 :init-function (metal-view-initializer draw-callback
                                                        offscreen-draw-callback
                                                        frame)))

(defun default-draw-onscreen ())

(defun default-draw-offscreen ())

(defvar *pane*)

(defvar *interface*)

(defun test-metal-pane ()
  (setf *interface* (capi:contain (setf *pane* (make-metal-pane))
                                  :title "Metal"
                                  :best-width 1280
                                  :best-height 800)))
