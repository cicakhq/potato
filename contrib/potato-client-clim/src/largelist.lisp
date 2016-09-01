(in-package :potato-client-clim)

(defclass dynlist-pane (clim:application-pane)
  ((content :type t
            :initarg :content
            :accessor dynlist-pane/content))
  (:default-initargs :output-record (make-instance 'dynlist-output-history)))

(defclass dynlist-output-history (clim:output-record clim:stream-output-history-mixin)
  ((parent        :initarg :parent
                  :reader clim:output-record-parent)
   (prefix-end    :initform 0
                  :accessor prefix-end)
   (prefix-height :initform 0
                  :accessor prefix-height)
   (width         :initform 0
                  :accessor width)
   (height        :initform 0
                  :accessor height)))

(defgeneric content (obj)
  (:method ((obj dynlist-output-history))
    (dynlist-pane/content (clim:output-record-parent obj))))

(defmethod initialize-instance :after ((obj dynlist-pane) &key)
  (setf (clim:stream-recording-p obj) nil)
  (setf (clim:stream-end-of-line-action obj) nil)
  (let ((history (clim:stream-output-history obj)))
    (setf (slot-value history 'parent) obj)))

(defgeneric dynlist-updated (dylist)
  (:documentation "Notifies a dynlist that its content has been updated"))

(defmethod dynlist-updated ((pane dynlist-pane))
  (let ((history (clim:stream-output-history pane)))
    (recompute-output-history pane)
    (change-space-requirements history)
    (clim:replay history pane)))

(defgeneric dynlist-size (list)
  (:documentation "Returns the number of elements in LIST"))

(defgeneric dynlist-get (list index)
  (:documentation "Returns the element in LIST at INDEX"))

(defun forward (history)
  (incf (prefix-height history)
	(clim:bounding-rectangle-height
	 (dynlist-get-output-record-for-object (clim:output-record-parent history) (prefix-end history))))
  (incf (prefix-end history)))

(defun backward (history)
  (decf (prefix-end history))
  (decf (prefix-height history)
	(clim:bounding-rectangle-height
	 (dynlist-get-output-record-for-object (clim:output-record-parent history) (prefix-end history)))))

(defun adjust-prefix (history viewport-top)
  ;; If there are lines in the suffix that are entirely above the
  ;; viewport, then move them to the prefix.
  (loop
    with lines = (content history)
    with length = (dynlist-size lines)
    until (= (prefix-end history) length)
    while (<= (+ (prefix-height history)
                 (clim:bounding-rectangle-height
                  (dynlist-get-output-record-for-object (clim:output-record-parent history) (prefix-end history))))
              viewport-top)
    do (forward history))
  ;; If there are lines in the prefix that are not entirely above
  ;; the viewport, then move them to the suffix.
  (loop
    until (zerop (prefix-end history))
    while (> (prefix-height history) viewport-top)
    do (backward history)))

(defmethod clim:replay-output-record ((record dynlist-output-history) stream
                                      &optional
                                        region x-offset y-offset)
  (declare (ignore x-offset y-offset))
  (alexandria:when-let ((viewport (clim:pane-viewport-region stream)))
    (multiple-value-bind (left top right bottom)
        (clim:bounding-rectangle* viewport)
      (clim:medium-clear-area (clim:sheet-medium stream) left top right bottom)
      (adjust-prefix record top)
      (loop
        with lines = (content record)
        with length = (dynlist-size lines)
        for i from (prefix-end record) below length
        for line = (dynlist-get-output-record-for-object (clim:output-record-parent record) i)
        for y = (prefix-height record) then (+ y height)
        for height = (clim:bounding-rectangle-height line)
        while (< y bottom)
        do (setf (clim:output-record-position line) (values 0 y))
           (clim:replay-output-record line stream region)))))

(defmethod clim:bounding-rectangle* ((history dynlist-output-history))
  (values 0 0 (width history) (height history)))

(defun recompute-width (history)
  (check-type history dynlist-output-history)
  (setf (width history)
	(loop
          with lines = (content history)
          with length = (dynlist-size lines)
          for i from 0 below length
          for record = (dynlist-get-output-record-for-object (clim:output-record-parent history) i)
          maximize (clim:bounding-rectangle-width record))))

(defun recompute-output-history (pane)
  (check-type pane dynlist-pane)
  (let* ((history (clim:stream-output-history pane))
         (content (dynlist-pane/content pane))
         (length (dynlist-size content)))
    (setf (prefix-end history) length)
    (setf (height history)
          (loop
            for i from 0 below length
            summing (clim:bounding-rectangle-height (dynlist-get-output-record-for-object pane i))))
    (recompute-width history)))

#+nil(defun insert (history record index)
  (when (> (prefix-end history) index)
    (incf (prefix-end history))
    (incf (prefix-height history)
	  (clim:bounding-rectangle-height record)))
  (incf (height history)
	(clim:bounding-rectangle-height record))
  (let ((width (clim:bounding-rectangle-width record)))
    (when (> width (width history))
      (setf (width history) width)))
  (flexichain:insert* (lines history) index record))

#+nil(defun delete (history index)
  (let ((existing (flexichain:element* (lines history) index)))
    (when (> (prefix-end history) index)
      (decf (prefix-height history)
	    (clim:bounding-rectangle-height existing))
      (decf (prefix-end history)))
    (decf (height history)
	  (clim:bounding-rectangle-height existing))
    (flexichain:delete* (lines history) index)
    (when (= (clim:bounding-rectangle-width existing) (width history))
      (recompute-width history))))

#+nil(defun replace (history record index)
  (let ((existing (flexichain:element* (lines history) index)))
    (when (> (prefix-end history) index)
      (incf (prefix-height history)
	    (- (clim:bounding-rectangle-height record)
	       (clim:bounding-rectangle-height existing))))
    (incf (height history)
	  (- (clim:bounding-rectangle-height record)
	     (clim:bounding-rectangle-height existing)))
    (setf (flexichain:element* (lines history) index) record)
    (if (> (clim:bounding-rectangle-width record)
	   (clim:bounding-rectangle-width existing))
	(when (> (clim:bounding-rectangle-width record) (width history))
	  (setf (width history)
		(clim:bounding-rectangle-width record)))
	(when (= (clim:bounding-rectangle-width existing) (width history))
	  (recompute-width history)))))

(defmethod clim:clear-output-record ((history dynlist-output-history))
  (log:warn "We should probably recompute the content here")
  #+nil(let ((chain (content history)))
    (flexichain:delete-elements* chain 0 (flexichain:nb-elements chain))))

(defmethod clim:add-output-record ((record clim:standard-updating-output-record)
                                   (history dynlist-output-history))
  (error "When is this called?")
  #+nil(flexichain:push-end (lines history) record))

(defmethod clim:map-over-output-records-containing-position (function (history dynlist-output-history) x y
                                                             &optional
                                                               x-offset y-offset
                                                             &rest function-args)
  (declare (ignore x-offset y-offset))
  (log:info "Mapping over records")
  (loop
    with lines = (content history)
    with length = (dynlist-size lines)
    for index from 0 below length
    for record = (dynlist-get-output-record-for-object (clim:output-record-parent history) index)
    when (clim:region-contains-position-p record x y)
      do (apply function record function-args)
         (log:info "printing region for ~s" index)))


(defun change-space-requirements (output-history)
  (clim:change-space-requirements (clim:output-record-parent output-history)
                                  :width (width output-history)
                                  :height (height output-history)))

;;;
;;;  Implementation of content functions for plain arrays
;;;

(defmethod dynlist-size ((obj array))
  (array-dimension obj 0))

(defmethod dynlist-get ((obj array) index)
  (aref obj index))

(defgeneric output-record-for-object (pane object))

(defmethod output-record-for-object ((pane dynlist-pane) object)
  (clim:with-output-to-output-record (pane 'clim:standard-presentation record
                                           :object object)
    (present-to-stream object pane)))

(defun dynlist-get-output-record-for-object (pane index)
  (output-record-for-object pane (dynlist-get (dynlist-pane/content pane) index)))
