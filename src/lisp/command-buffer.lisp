(in-package :metal)

(defvar *command-buffer*)

(defun make-render-command-encoder (&key descriptor parallel-p)
  (if parallel-p
      (objc:invoke *command-buffer*
                   "parallelRenderCommandEncoderWithDescriptor:"
                   descriptor)
      (objc:invoke *command-buffer*
                   "renderCommandEncoderWithDescriptor:"
                   descriptor)))

(defun make-compute-command-encoder ()
  (objc:invoke *command-buffer* "computeCommandEncoder"))

(defun make-blit-command-encoder ()
  (objc:invoke *command-buffer* "blitCommandEncoder"))

(defun enqueue ()
  (objc:invoke *command-buffer* "enqueue"))

(defun commit ()
  (objc:invoke *command-buffer* "commit"))

(fli:define-foreign-block-callable-type command-buffer-handler
    :void
  (objc:objc-object-pointer))

(defun add-scheduled-handler (handler)
  (fli:with-foreign-block (handler 'command-buffer-handler handler)
    (objc:invoke *command-buffer* "addScheduledHandler:" handler)))

(defun add-completed-handler (handler)
  (fli:with-foreign-block (handler 'command-buffer-handler handler)
    (objc:invoke *command-buffer* "addCompletedHandler:" handler)))

(defun present-drawable (drawable &key at-time)
  (if at-time
      (objc:invoke *command-buffer* "presentDrawable:atTime:" drawable at-time)
      (objc:invoke *command-buffer* "presentDrawable:" drawable)))

(defun wait-until-scheduled ()
  (objc:invoke *command-buffer* "waitUntilScheduled"))

(defun wait-until-completed ()
  (objc:invoke *command-buffer* "waitUntilCompleted"))

(defun command-buffer-status ()
  (case (objc:invoke *command-buffer* "status")
    (0 :not-enqueued)
    (1 :enqueued)
    (2 :committed)
    (3 :scheduled)
    (4 :completed)
    (5 :error)))

(defun command-buffer-error ()
  (objc:invoke *command-buffer* "error"))

(defun command-buffer-has-retained-references-p ()
  (objc:invoke-bool *command-buffer* "retainedReferences"))

