(defpackage :metal
  (:use :common-lisp
        :harlequin-common-lisp
        :lispworks))

(defvar *library-pathname*
  (asdf:system-relative-pathname :metal "src/metal/Shaders.metal"))

(defvar *frameworks*
  '("/System/Library/Frameworks/Metal.framework/Metal"
    "/System/Library/Frameworks/MetalKit.framework/MetalKit"
    "/System/Library/Frameworks/ModelIO.framework/ModelIO"))

(objc:ensure-objc-initialized :modules *frameworks*)
