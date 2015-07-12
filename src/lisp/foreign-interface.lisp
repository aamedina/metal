(in-package :metal)

(objc:define-objc-struct (clear-color (:foreign-name "MTLClearColor"))
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(objc:define-objc-struct (origin (:foreign-name "MTLOrigin"))
  (x (:unsigned :long))
  (y (:unsigned :long))
  (z (:unsigned :long)))

(objc:define-objc-struct (size (:foreign-name "MTLSize"))
  (width (:unsigned :long))
  (height (:unsigned :long))
  (depth (:unsigned :long)))

(objc:define-objc-struct (region (:foreign-name "MTLRegion"))
  (origin (:struct origin))
  (region (:struct size)))

(objc:define-objc-struct (scissor-rect (:foreign-name "MTLScissorRect"))
  (x (:unsigned :long))
  (y (:unsigned :long))
  (width (:unsigned :long))
  (height (:unsigned :long)))

(objc:define-objc-struct (viewport (:foreign-name "MTLViewport"))
  (origin-x :double)
  (origin-y :double)
  (width :double)
  (height :double)
  (z-near :double)
  (z-far :double))

(objc:define-objc-struct (draw-primitives-indirect-arguments
                          (:foreign-name "MTLDrawPrimitivesIndirectArguments"))
  (vertex-count :uint32)
  (instance-count :uint32)
  (vertex-start :uint32)
  (base-instance :uint32))

(objc:define-objc-struct
    (draw-indexed-primitives-indirect-arguments
     (:foreign-name "MTLDrawIndexedPrimitivesIndirectArguments"))
  (index-count :uint32)
  (instance-count :uint32)
  (index-start :uint32)
  (base-vertex :uint32)
  (base-instance :uint32))

(objc:define-objc-struct
    (dispatch-threadgroups-indirect-arguments
     (:foreign-name "MTLDispatchThreadgroupsIndirectArguments"))
  (threadgroups-per-grid (:c-array :uint32 3)))
