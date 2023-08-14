(defpackage #:flamegraph
  (:use #:cl)
  (:import-from #:alexandria)
  (:import-from #:sb-sprof)
  (:nicknames #:flamegraph/core)
  (:export
   #:save-flame-graph))
(in-package flamegraph)


(defparameter *frame-where-profiling-was-started* nil)


(defclass node ()
  ((func :initarg :func
         :initform nil
         :type (or string
                   sb-di::compiled-debug-fun
                   null)
         :accessor get-func)
   (counter :initform 0
            :type fixnum
            :initarg :counter
            :accessor get-counter)
   (calls :initform nil
          :type list
          :initarg :calls
          :documentation "A list of other nodes, called by current one"
          :accessor get-calls)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "~A :calls ~A"
            (or (get-func node)
                "<root>")
            (get-counter node))))

(defun search-or-add-child (node func)
  ;; Not all frames contain an info for some reason.
  ;; We only want to show  meaningfull nodes
  (when func
    (let* ((children (get-calls node))
           (child (find func children
                        :test #'equal
                        :key #'get-func)))
      (unless child
        (setf child (make-instance 'node :func func))
        (push child (get-calls node)))
      child)))

(defgeneric get-name (obj))

(defmethod get-name ((obj node))
  (get-name (get-func obj)))

(defmethod get-name ((obj string))
  obj)

(defmethod get-name ((obj sb-di::compiled-debug-fun))
  (get-name (slot-value obj 'SB-DI::COMPILER-DEBUG-FUN)))

(defmethod get-name ((obj SB-C::COMPILED-DEBUG-FUN))
  (get-name (slot-value obj 'SB-C::NAME)))

(defmethod get-name ((obj cons))
  (let ((*print-pretty* nil))
    (format nil "~S" obj)))

(defmethod get-name ((obj symbol))
  (symbol-name obj))

(defmethod get-name ((obj sb-kernel:code-component))
  "Some binary code")

(defun aggregate-raw-data ()
  ;; We need to actually run a report once to make the call graph
  ;; available to map.
  (sb-sprof:report :stream (make-broadcast-stream)))

(defun make-graph ()
  (aggregate-raw-data)
  (let ((root (make-instance 'node)))
    (sb-sprof:map-traces
     (lambda (thread trace)
       (declare (ignorable thread))
       (let ((current-node root))
         (sb-sprof::map-trace-pc-locs
          (lambda (info pc-or-offset)
            (declare (ignorable pc-or-offset))
            (let ((node (search-or-add-child current-node
                                             info)))
              (when node
                (incf (get-counter node))
                (setf current-node
                      node))))
          trace)))
     sb-sprof::*samples*)
    root))


(defun remove-nodes-up-to-frame (nodes frame)
  (let ((func (slot-value frame 'sb-di::debug-fun)))
    (loop for rest on nodes
          for node = (car rest)
          when (eql (get-func node)
                    func)
            do (return (cdr rest)))))


(defun print-graph (root &key (stream t) (max-depth most-positive-fixnum))
  (let* ((roots (get-calls root)))
    (labels ((print-path (path count)
               (let* ((nodes (reverse path))
                      (rest-nodes (remove-nodes-up-to-frame nodes
                                                            *frame-where-profiling-was-started*))
                      (names (mapcar #'get-name rest-nodes)))
                 (when names
                   (format stream "~{~A~^;~} ~A~%"
                           names
                           count))))
             (print-node (node &optional path (depth 0))
               (when (< depth max-depth)
                 (let* ((count (get-counter node))
                        (path (list* node path))
                        (children (get-calls node)))
                   (when (> count 0)
                     (print-path path count))
                   (loop for child in children
                         do (print-node child path (1+ depth)))))))
      (mapcar #'print-node
              roots)
      (values))))


(defmacro save-flame-graph ((filename &rest sb-sprof-opts) &body body)
  (alexandria:with-gensyms (result-var)
    `(let ((*frame-where-profiling-was-started*
             (sb-di:top-frame))
           (,result-var nil))
       (with-simple-restart (abort "Stop profiling and save graph")
         (sb-sprof:with-profiling (,@sb-sprof-opts)
           (setf ,result-var
                 (multiple-value-list
                  (progn ,@body)))))
       (alexandria:with-output-to-file (s ,filename :if-exists :supersede)
         (print-graph (make-graph)
                      :stream s))
       (values-list ,result-var))))
