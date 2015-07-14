(in-package :metal)

(defvar *library-pathname*
  (asdf:system-relative-pathname :metal "src/metal/Shaders.metal"))

(defvar *view*)
(defvar *library*)
(defvar *descriptor*)
(defvar *drawable*)
(defvar *texture-loader*)

(defvar *command-queue*)
(defvar *command-encoder*)

(defvar *buffers* (make-hash-table))
(defvar *textures* (make-hash-table))

(defvar *render-pipeline* (make-hash-table))
(defvar *compute-pipeline* (make-hash-table))
(defvar *blit-pipeline* (make-hash-table))

(defun make-render-pipeline (name &key vertex-function
                                       fragment-function
                                       vertex-descriptor
                                       color-attachments
                                       depth-attachment-pixel-format
                                       stencil-attachment-pixel-format)
  (let ((descriptor (new "MTLRenderPipelineDescriptor")))
    (objc:invoke descriptor "setVertexFunction:" vertex-function)
    (objc:invoke descriptor "setFragmentFunction:" fragment-function)
    (when vertex-descriptor
      (objc:invoke descriptor "setVertexDescriptor:" vertex-descriptor))
    (when color-attachments
      (let ((arr (objc:invoke-into '(array (:unsigned :long)) descriptor
                                   "colorAttachments")))
        (dotimes (i (length arr))
          (setf (aref arr i) (aref color-attachments i)))))
    (when depth-attachment-pixel-format)
    (when stencil-attachment-pixel-format)
    (fli:with-dynamic-foreign-objects ((err objc:objc-object-pointer))
      (let ((pipeline (objc:invoke *device*
                                   "newRenderPipelineStateWithDescriptor:error:"
                                   descriptor err)))
        (or (ns-error err)
            (setf (gethash name *render-pipeline*) pipeline))))))

(defun compile-metal-library (&optional (lib-pathname *library-pathname*)
                                        (device *device*))
  (when-let (src (file-string lib-pathname))
    (release-var '*library*)
    (fli:with-dynamic-foreign-objects ((err objc:objc-object-pointer))
      (let ((lib (objc:invoke device "newLibraryWithSource:options:error:"
                              src nil err)))
        (or (ns-error err)
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
    (setf *texture-loader* (make-texture-loader))))

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
  `(let* ((*command-buffer* (objc:invoke *command-queue* "commandBuffer"))
          (*command-encoder* ,encoder))
     (unwind-protect
          (progn
            (objc:invoke *command-buffer* "enqueue")
            ,@body)
       (objc:invoke *command-buffer* "commit"))))

(defmacro rendering (&body body)
  `(let ((*descriptor* (objc:invoke *view* "currentRenderPassDescriptor")))
     (with-command-encoder (make-render-encoder)
       (when-let (*drawable* (objc:invoke *view* "currentDrawable"))
         (unwind-protect
              (progn
                (loop
                  for pipeline being the hash-values in *render-pipeline*
                  do (objc:invoke *command-encoder* "setRenderPipelineState:"
                                  pipeline))
                ,@body)
           (objc:invoke *command-encoder* "endEncoding")
           (objc:invoke *command-buffer* "presentDrawable:" *drawable*))))))

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

(define-objc-property ("clearColor" metal-kit-view)
  :accessor clear-color)

(define-objc-property ("clearDepth" metal-kit-view)
  :accessor clear-depth)

(define-objc-property ("clearStencil" metal-kit-view)
  :accessor clear-stencil)

(define-objc-property ("colorPixelFormat" metal-kit-view)
  :accessor color-pixel-format)

(define-objc-property ("depthStencilPixelFormat" metal-kit-view)
  :accessor depth-stencil-pixel-format)

(define-objc-property ("sampleCount" metal-kit-view)
  :accessor sample-count)

(define-objc-property ("currentRenderPassDescriptor" metal-kit-view)
  :reader current-render-pass-descriptor)

(define-objc-property ("depthStencilTexture" metal-kit-view)
  :reader depth-stencil-texture)

(define-objc-property ("multisampleColorTexture" metal-kit-view)
  :reader multisample-color-texture)

(define-objc-property ("preferredFramesPerSecond" metal-kit-view)
  :accessor preferred-frames-per-second)

(define-objc-property ("nominalFramesPerSecond" metal-kit-view)
  :accessor nominal-frames-per-second)

(define-objc-property ("paused" metal-kit-view)
  :accessor paused
  :getter "isPaused")

(define-objc-property ("enableSetNeedsDisplay" metal-kit-view)
  :accessor enable-set-needs-display)

(define-objc-property ("autoResizeDrawable" metal-kit-view)
  :accessor auto-resize-drawable)

(define-objc-property ("currentDrawable" metal-kit-view)
  :reader current-drawable)

(define-objc-property ("drawableSize" metal-kit-view)
  :accessor drawable-size)

(define-objc-property ("framebufferOnly" metal-kit-view)
  :accessor framebuffer-only)

(define-objc-property ("presentsWithTransaction" metal-kit-view)
  :accessor presents-with-transaction)

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
      (funcall (offscreen-draw-callback self))
      (rendering
        (funcall (draw-callback self))))))

(objc:define-objc-method ("setFrameSize:" :void)
    ((self metal-kit-view)
     (size cocoa:ns-size))
  (objc:invoke (objc:current-super) "setFrameSize:" size))

(defun dealloc-metal (&optional interface)
  (declare (ignore interface))
  (release-var '*device*)
  (release-var '*command-queue*)
  (release-var '*texture-loader*)
  (loop
    for pipeline being the hash-values in *render-pipeline*
    do (objc:release pipeline))
  (clrhash *render-pipeline*))

(objc:define-objc-method ("dealloc" :void)
    ((self metal-kit-view))
  (declare (ignore self))
  (objc:invoke (objc:current-super) "dealloc"))

(defun metal-view-initializer (draw-callback offscreen-draw-callback frame)
  (lambda (pane view)
    (declare (ignore pane))
    (let ((view (objc:invoke view "initWithFrame:" frame))
          (instance (objc:objc-object-from-pointer view)))
      (setf (draw-callback instance) draw-callback)
      (setf (offscreen-draw-callback instance) offscreen-draw-callback)
      view)))

(defun make-metal-pane (&key draw-callback
                             offscreen-draw-callback
                             frame)
  (initialize-metal)
  (make-instance 'capi:cocoa-view-pane
                 :view-class "CLMTKView"
                 :init-function (metal-view-initializer draw-callback
                                                        offscreen-draw-callback
                                                        frame)))

(capi:define-interface metal-application ()
  ((draw-callback :initarg :draw-callback)
   (offscreen-draw-callback :initarg :offscreen-draw-callback)
   (frame :initarg :frame))
  (:panes (metal-pane capi:cocoa-view-pane
                      :view-class "CLMTKView"
                      :accessor metal-pane
                      :min-width 640
                      :min-height 400)
          (toolbar capi:push-button-panel)
          (progress-bar capi:progress-bar
                        :start 0
                        :end 100)
          (clear-color-red capi:slider
                           :title "Clear Color Red"
                           :start 0
                           :end 255
                           :tick-frequency 0
                           :slug-start 0
                           :callback 'change-clear-color-red)
          (clear-color-green capi:slider
                             :title "Clear Color Green"
                             :start 0
                             :end 255
                             :tick-frequency 0
                             :slug-start 0
                             :callback 'change-clear-color-green)
          (clear-color-blue capi:slider
                            :title "Clear Color Blue"
                            :start 0
                            :end 255
                            :tick-frequency 0
                            :slug-start 0
                            :callback 'change-clear-color-blue)
          (clear-color-alpha capi:slider
                             :title "Clear Color Alpha"
                             :start 0
                             :end 255
                             :tick-frequency 0
                             :slug-start 255
                             :callback 'change-clear-color-alpha))
  (:layouts (main capi:column-layout
                  '(metal-pane
                    clear-color-red
                    clear-color-green
                    clear-color-blue
                    clear-color-alpha)))
  (:default-initargs
   :title ""
   :window-styles '(:internal-borderless)))

(defmethod initialize-instance :after ((self metal-application) &key)
  (with-slots (draw-callback offscreen-draw-callback frame)
      self
    (initialize-metal)
    (auto-compile-metal-library)
    (setf (capi:cocoa-view-pane-init-function (metal-pane self))
          (metal-view-initializer draw-callback
                                  offscreen-draw-callback
                                  frame))))

(defvar *application* nil)

(defun test-metal-application ()
  (setf *application* (make-instance
                       'metal-application
                       :draw-callback 'test-draw-callback
                       :offscreen-draw-callback 'test-offscreen-draw-callback
                       :destroy-callback 'dealloc-metal
                       :frame #(0 0 640 400)
                       :best-x 570
                       :best-y 0
                       :visible-min-width 640
                       :visible-min-height 600))
  (capi:display *application*))

(defun test-draw-callback ())

(defun test-offscreen-draw-callback ())

(defun change-clear-color (value index)
  (let ((color (clear-color (metal-pane *application*))))
    (setf (elt color index) (coerce (/ value 255) 'double-float))
    (setf (clear-color (metal-pane *application*)) color)))

(defun change-clear-color-red (interface value event)
  (declare (ignore interface event))
  (change-clear-color value 0))

(defun change-clear-color-green (interface value event)
  (declare (ignore interface event))
  (change-clear-color value 1))

(defun change-clear-color-blue (interface value event)
  (declare (ignore interface event))
  (change-clear-color value 2))

(defun change-clear-color-alpha (interface value event)
  (declare (ignore interface event))
  (change-clear-color value 3))


