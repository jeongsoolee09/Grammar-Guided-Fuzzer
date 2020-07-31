;; QuickLisp is needed!
(ql:quickload "str")

(defparameter *raw-grammar-lines*
  (with-open-file (in-stream "hw3.ml")
    (loop for line = (read-line in-stream nil) while line collect line)))

(defparameter *refined-grammar-lines*
  (remove-if-not (lambda (x) (/= (length x) 0)) *raw-grammar-lines*))

;; type와 and로 시작하는 것들을 추리고, 그 사이에 있는 constructor들을 챙긴다.

(defvar *hashtbl* (make-hash-table :test #'equal))

(defun extract-type-name (string)
  "type <type-name1> = <type-name2> 꼴의 스트링에서 <type-name1>을 추출한다."
  (let ((lhs (nth 1 (str:split " " string)))
        (rhs (nth 3 (str:split " " string))))
    (cons lhs rhs)))


(defun equal-sign-at-end (string)
  (= (search "=" string) (- (length string) 1)))


(defun collect-simple-types (line)
  (when (and (or (search "type" line)
                 (search "and" line))
             (not (equal-sign-at-end line)))
    (let ((lhs (car (extract-type-name line)))
          (rhs (cdr (extract-type-name line))))
      (setf (gethash lhs *hashtbl*) rhs))))

;; (collect-simple-types "type program = exp")


(defun collect-complex-types (lst acc)
  (if (null lst)
      acc
      (if (and (or (search "type" line)
                   (search "and" line))
               (equal-sign-at-end line))
          (collect-constructors (cdr lst))
          ())))

;; this function is broken!
(defun collect-constructors (lst)
  (loop for line in lst
        when (or (search "type" line)
                 (search "and" line))
          do (loop-finish)
        collect line))


;; for debugging purposes
(defun print-hash-table-entry (key value)
  (format t "~a => ~a~%" key value))

(defun print-hash-table (hashtbl)
  (maphash #'print-hash-table-entry hashtbl))
