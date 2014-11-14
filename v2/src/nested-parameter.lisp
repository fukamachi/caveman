(in-package :cl-user)
(defpackage caveman2.nested-parameter
  (:use :cl)
  (:export :parse-parameters))
(in-package :caveman2.nested-parameter)

(defun parse-parameters (params-alist)
  (let ((params (make-hash-table :test 'equal)))
    (labels ((process (key val)
               (declare (optimize speed))
               (let ((keys (if (char= (aref key (1- (length key))) #\])
                               (parse-key key)
                               (list key))))
                 (if (cdr keys)
                     (build-val keys val params)
                     (setf (gethash (car keys) params) val)))))
      (loop for (key . val) in params-alist
            do (process key val)))
    (expand-to-alist params)))

(defun build-val (nested-keys val current)
  (etypecase current
    (hash-table (%build-val-for-hash nested-keys val current))
    (array (%build-val-for-array nested-keys val current))))

(declaim (inline %build-val-for-hash))
(defun %build-val-for-hash (nested-keys val current)
  (let ((key (pop nested-keys)))
    (cond
      ((null nested-keys)
       (setf (gethash key current) val))
      (T (unless (nth-value 1 (gethash key current))
           (setf (gethash key current)
                 (if (string= (car nested-keys) "")
                     (make-array 0 :adjustable t :fill-pointer t)
                     (make-hash-table :test 'equal))))
         (build-val nested-keys val
                    (gethash key current))))))

(declaim (inline %build-val-for-array))
(defun %build-val-for-array (nested-keys val current)
  (pop nested-keys)
  (cond
    ((null nested-keys)
     (vector-push-extend val current))
    (T
     (if (string= (car nested-keys) "")
         (let ((next (make-array 0 :adjustable t :fill-pointer t)))
           (vector-push-extend next current)
           (build-val nested-keys val next))
         (progn
           (when (= (length current) 0)
             (vector-push-extend (make-hash-table :test 'equal)
                                 current))
           (let ((next (aref current (1- (length current)))))
             (when (nth-value 1 (gethash (car nested-keys) next))
               (setf next (make-hash-table :test 'equal))
               (vector-push-extend next current))
             (build-val nested-keys val next)))))))

(defun parse-key (key)
  (loop with pos = 0
        for (nested-key new-pos) = (multiple-value-list (peek-key key pos))
        while nested-key
        do (setf pos new-pos)
        collect nested-key))

(declaim (inline peek-key))
(declaim (ftype (function (string integer) t) peek-key))
(defun peek-key (string start)
  (declare (optimize speed))
  (if (= start (length string))
      nil
      (let ((begin (position #\[ string :start start)))
        (if begin
            (if (= begin start)
                (let ((end (position #\] string :start (1+ begin))))
                  (if end
                      (values (subseq string (1+ begin) end)
                              (1+ end))
                      nil))
                (let ((end (position #\] string :start (1+ begin))))
                  (if end
                      (values (subseq string start begin)
                              begin)
                      (values string
                              (length string)))))
            (values (subseq string start)
                    (length string))))))

(defun expand-to-alist (obj)
  (typecase obj
    (hash-table (loop for k being the hash-keys in obj using (hash-value v)
                      collect (cons k (expand-to-alist v))))
    ((and array
          (not string))
     (loop for a across obj
           collect (expand-to-alist a)))
    (T obj)))
