(in-package :cl-user)

(defpackage :trivial-queue
  (:use :cl)
  (:export #:make-queue
           #:queue-size
           #:queue-empty-p
           #:enqueue
           #:dequeue
           #:queue-peek
           #:queue-clear
           #:queue-to-list))
(in-package :trivial-queue)

(declaim (inline make-queue queue-size queue-empty-p
                 enqueue dequeue queue-peek queue-clear
                 queue-to-list))

(locally (declare (optimize (speed 3) (space 0)))
  (defstruct (queue (:conc-name)
                    (:constructor %make-queue))
    (%head () :type cons)
    (%last () :type cons)
    (%size 0  :type fixnum)))

(defun make-queue (&key initial-contents)
  (declare (optimize (speed 3) (space 0)))
  (let ((cell (cons nil nil)))
    (if initial-contents
        (let ((head initial-contents)
              (last (last initial-contents))
              (size (length initial-contents)))
          (setq last (setf (cdr last) (cons nil nil)))
          (%make-queue :%head head :%last last :%size size))
        (%make-queue :%head cell :%last cell))))

(defun queue-size (queue)
  (declare (optimize (speed 3) (space 0)))
  (%size queue))

(defun queue-empty-p (queue)
  (declare (optimize (speed 3) (space 0)))
  (eq (%head queue) (%last queue)))

(defun enqueue (queue item)
  (declare (optimize (speed 3) (space 0)))
  (let ((last (%last queue)))
    (locally (declare (optimize (safety 0)))
      (incf (%size queue)))
    (setf (cdr last) (setf (%last queue) (cons nil nil))
          (car last) item)))

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
  (setf (%head queue) (%last queue))
  (values))

(defun queue-to-list (queue)
  (declare (optimize (speed 3) (space 0)))
  (loop for cell on (%head queue)
        while (cdr cell)
        collect (car cell)))
