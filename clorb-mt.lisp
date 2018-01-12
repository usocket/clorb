;;;; clorb-mt.lisp -- System independent multi-threading and synchronization

(in-package :clorb)


(defgeneric lock (obj)
  (:documentation
   "Return the synchronization lock for object"))

(defgeneric waitqueue (obj)
  (:documentation
   "Return the waitqueue/semaphore/condition etc. for the object."))


(defclass synchronized ()
  ((lock       :initform (make-lock "syn")   :reader lock)
   (waitqueue  :initform (make-waitqueue)    :reader waitqueue)))

(defclass synchronized-lazy (synchronized)
  ((waitqueue  :initform nil  )))

(defmethod waitqueue ((obj synchronized-lazy))
  (or (slot-value obj 'waitqueue)
      (setf (slot-value obj 'waitqueue)
            (make-waitqueue))))


(defmacro with-synchronization (obj &body body)
"Execute body with the synchronized object's lock held."
  `(with-lock (lock ,obj)
     ,@body))


(defun synch-locked-wait (obj)
  "Synchronize wait on locked object. 
Object should have (lock _) and (waitqueue _)"
  (wq-locked-wait (waitqueue obj) (lock obj)))


(defun synch-notify (obj)
  "Notify object's waitqueue, wake at least one process waiting on the waitqueue.
Should me called with object lock held."
  (wq-notify (waitqueue obj)))


(defun synch-wait-on-condition (obj wait-func &rest wait-args)
  (with-synchronization obj
    (unless (apply wait-func wait-args)
      (apply #'wq-locked-wait-on-condition
             (waitqueue obj) (lock obj)
             wait-func wait-args))))



;;;; Shared Queue


(defclass shared-queue (synchronized)
  ((queue  :initform nil  :accessor queue)))


(defmethod enqueue ((q shared-queue) obj)
  (with-synchronization q
    (enqf (queue q) obj)
    (wq-notify (waitqueue q)))
  t)


(defmethod dequeue ((q shared-queue) &optional non-blocking)
  (with-synchronization q
    (if non-blocking
        (deqf (queue q))
        (loop
           (if (queue-empty-p (queue q))
               (synch-locked-wait q)
               (return (deqf (queue q))))))))



;;;; Execution Queue


(defclass execution-queue (synchronized)
  ((executor      :initarg :executor  :initform 'eq-exec :accessor executor)
   (name-template :initarg :name-template :initform "eq" :accessor name-template)  
   (max-processes :initarg :max-processes :initform 10   :accessor max-processes)
   (max-handler   :initarg :max-handler   :initform nil  :accessor max-handler)
   (max-idle      :initarg :max-idle      :initform nil  :accessor max-idle)
   (queue                               :initform nil :accessor eq-queue)
   (idle-count                          :initform 0   :accessor idle-count)
   (process-count                       :initform 0   :accessor process-count)
   (process-table                       :initform nil :accessor process-table)
   (name-count                          :initform 0   :accessor name-count)))


(defmacro with-execution-queue (q &body body)
  `(with-accessors ((max-processes max-processes)
                    (max-handler max-handler)
                    (max-idle max-idle)
                    (queue eq-queue) (idle-count idle-count)
                    (process-table process-table) (process-count process-count)
                    (executor executor) (name-template name-template)
                    (name-count name-count))
                   ,q
     ,@body))


(defmethod next-process-name ((q execution-queue))
  (with-execution-queue q
    (format nil "~A ~D" name-template (incf name-count))))


(defun eq-main (q obj entry)
  (with-execution-queue q
    (let* ((process (current-process)))
      (setf (car entry) process)
      (loop
        (labels 
          ((exit ()
             (decf process-count)
             (deletef entry process-table)
             (return))
           (set-status (status &optional obj)
             (let ((x (cdr entry)))
               (setf (car x) status)
               (setf (cdr x) obj)))
           (idle ()
             (incf idle-count)
             (set-status :idle)
             (do () ((not (queue-empty-p queue)))
               (synch-locked-wait q))
             (decf idle-count))
           (broken (condition)
             (warn "Thread fails: ~A thr ~A" condition (current-process))
             (with-synchronization q (exit))))
          (set-status :working obj)
          (handler-case (funcall executor obj)
            (serious-condition (condition) (broken condition)))
          (with-synchronization q
            (when (and (queue-empty-p queue)
                       (or (null max-idle) (< idle-count max-idle)))
              (idle))
            (if (queue-empty-p queue) 
              (exit)
              (setq obj (deqf queue)))))))))



(defun eq-exec (obj)
  (etypecase obj
    (function (funcall obj))
    (cons (apply (car obj) (cdr obj)))))


(defmethod enqueue ((q execution-queue) obj)
  (with-execution-queue q
    (with-synchronization q
      (flet ((ok-make-more-processes ()
               (or (< process-count max-processes)
                   (if max-handler
                     (funcall max-handler q obj)))))
        (cond ((and (zerop idle-count) (ok-make-more-processes))
               (incf process-count)
               (let ((entry (list nil nil)))
                 (push entry process-table)
                 (start-process (next-process-name q) #'eq-main q obj entry)))
              (t 
               (enqf queue obj)
               (synch-notify q)
               nil))))))


(defun garb-threads (q)
  (with-execution-queue q
    (with-synchronization q
      (setq process-table
            (remove-if (lambda (entry)
                         (let ((p (car entry)))
                           (unless (process-running-p p)
                             (decf process-count)
                             (when (eql (cadr entry) :idle)
                               (decf idle-count))
                             t)))
                       process-table)))))
