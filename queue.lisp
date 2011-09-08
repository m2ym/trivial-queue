(in-package :cl-user)

(defpackage :trivial-queue
  (:use :cl)
  (:export #:make-queue
           #:queue-size
           #:queue-empty-p
           #:enqueue
           #:dequeue
           #:queue-peek
           #:queue-clear))
(in-package :trivial-queue)

(declaim (inline make-queue queue-size queue-empty-p
                 enqueue dequeue queue-peek queue-clear))

(locally (declare (optimize (speed 3) (space 0)))
  (defstruct (queue (:conc-name)
                    (:constructor %make-queue))
    (%head () :type cons)
    (%tail () :type cons)
    (%size 0  :type fixnum)))

(defun make-queue (&key initial-contents)
  (declare (optimize (speed 3) (space 0)))
  (let* ((cell (cons nil nil))
         (queue (%make-queue :%head cell :%tail cell)))
    (when initial-contents
      (dolist (item initial-contents)
        (enqueue queue item)))
    queue))

(defun queue-size (queue)
  (declare (optimize (speed 3) (space 0)))
  (%size queue))

(defun queue-empty-p (queue)
  (declare (optimize (speed 3) (space 0)))
  (eq (%head queue) (%tail queue)))

(defun enqueue (queue item)
  (declare (optimize (speed 3) (space 0)))
  (let ((tail (%tail queue)))
    (locally (declare (optimize (safety 0)))
      (incf (%size queue)))
    (setf (cdr tail) (setf (%tail queue) (cons nil nil))
          (car tail) item)))

(defun dequeue (queue)
  (declare (optimize (speed 3) (space 0)))
  (unless (queue-empty-p queue)
    (locally (declare (optimize (safety 0)))
      (decf (%size queue)))
    (pop (%head queue))))

(defun queue-peek (queue)
  (declare (optimize (speed 3) (space 0)))
  (car (%head queue)))

(defun queue-clear (queue)
  (declare (optimize (speed 3) (space 0)))
  (setf (%head queue) (%tail queue))
  (values))
