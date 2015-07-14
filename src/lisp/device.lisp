(in-package :metal)

(defvar *device* (create-system-default-device))

(defun make-library-with-source (source options)
  (with-objc-reference (err)
    (let ((lib (objc:invoke *device* "newLibraryWithSource:options:error:"
                            source options err)))
      (or (ns-error err) lib))))

(defun make-command-queue (&key max-command-buffer-count)
  (if max-command-buffer-count
      (objc:invoke *device* "newCommandQueueWithMaxCommandBufferCount:"
                   max-command-buffer-count)
      (objc:invoke *device* "newCommandQueue")))

(defun make-buffer (&key length pointer no-copy-p (options 0) deallocator)
  (cond
    (no-copy-p
     (objc:invoke *device*
                  "newBufferWithBytesNoCopy:length:options:deallocator:"
                  pointer length options deallocator))
    (pointer (objc:invoke *device* "newBufferWithBytes:length:options:"
                          pointer length options))
    (length (objc:invoke *device* "newBufferWithLength:options:"
                         length options))))

(defun make-texture (&key descriptor io-surface plane)
  (cond
    ((and io-surface plane)
     (objc:invoke *device* "newTextureWithDescriptor:iosurface:plane:"
                  descriptor io-surface plane))
    
    (descriptor (objc:invoke *device* "newTextureWithDescriptor:" descriptor))))

(defun make-sampler-state (&key descriptor)
  (when descriptor
    (objc:invoke *device* "newSamplerStateWithDescriptor:" descriptor)))

(defun make-depth-stencil-state (&key descriptor)
  (when descriptor
    (objc:invoke *device* "newDepthStencilStateWithDescriptor:" descriptor)))

(fli:define-foreign-block-callable-type render-pipeline-state-handler
    :void
  (objc:objc-object-pointer objc:objc-object-pointer))

(defun pipeline-state-options (options)
  (case options
    (:none 0)
    (:argument-info 1)
    (:buffer-type-info 2)))

(defun make-render-pipeline-state-with-reflection (descriptor
                                                   options
                                                   reflection
                                                   error)
  (objc:invoke *device*
               "newRenderPipelineStateWithDescriptor:options:reflection:error:"
               descriptor (pipeline-state-options options) reflection error))

(defun make-render-pipeline-state (&key descriptor
                                        completion-handler
                                        reflection
                                        (options :none))
  (cond
    (reflection
     (with-objc-reference (err)
       (let ((o (make-render-pipeline-state-with-reflection descriptor
                                                            options
                                                            reflection
                                                            err)))
         (or (ns-error err) o))))
    (completion-handler
     (objc:invoke
      *device*
      "newRenderPipelineStateWithDescriptor:options:completionHandler:"
      descriptor (pipeline-state-options options) completion-handler))
    (descriptor
     (with-objc-reference (err)
       (let ((o (objc:invoke *device*
                             "newRenderPipelineStateWithDescriptor:error:"
                             descriptor err)))
         (or (ns-error err) o))))))

(fli:define-foreign-block-callable-type compute-pipeline-state-handler
    :void
  (objc:objc-object-pointer objc:objc-object-pointer))

(defun make-compute-pipeline-state (&key function
                                         completion-handler
                                         reflection
                                         (options :none))
  (cond
    (reflection
     (with-objc-reference (err)
       (let ((o (objc:invoke
                 *device*
                 "newComputePipelineStateWithFunction:options:reflection:error:"
                 function (pipeline-state-options options) reflection err)))
         (or (ns-error err) o))))
    (completion-handler
     (objc:invoke
      *device*
      "newComputePipelineStateWithFunction:options:completionHandler:"
      function (pipeline-state-options options) completion-handler))
    (function
     (with-objc-reference (err)
       (let ((o (objc:invoke *device*
                             "newComputePipelineStateWithFunction:error:"
                             function err)))
         (or (ns-error err) o))))))


