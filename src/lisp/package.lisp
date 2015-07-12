(defpackage :metal
  (:use :common-lisp))

(defvar *frameworks*
  '("/System/Library/Frameworks/Metal.framework/Metal"
    "/System/Library/Frameworks/MetalKit.framework/MetalKit"
    "/System/Library/Frameworks/ModelIO.framework/ModelIO"))

(objc:ensure-objc-initialized :modules *frameworks*)
