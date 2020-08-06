(defpackage :grammar-guided-fuzzer
  (:use
   :common-lisp)
  (:export
   :open-db
   :save
   :store))

;; QuickLisp is needed!
(ql:quickload "str")

(defparameter *raw-grammar-lines*
  (with-open-file (in-stream "hw3.ml")
    (loop for line = (read-line in-stream nil) while line collect line)))

(defparameter *refined-grammar-lines*
  (remove-if-not (lambda (x) (/= (length x) 0)) *raw-grammar-lines*)
  "읽은 파일 중 empty string을 제외한 나머지")

;; type와 and로 시작하는 것들을 추리고, 그 사이에 있는 constructor들을 챙긴다.

(defvar *type-constructor-table* (make-hash-table :test #'equal))

(defun extract-type-name (string)
  "type <type-name1> = <type-name2> 혹은
   type <type-name1> =
   꼴의 스트링에서 <type-name1>을 추출한다."
  (let ((splitted-on-equal (str:split " " string)))
    (if (= (length splitted-on-equal) 4)
        (let ((lhs (nth 1 splitted-on-equal))
              (rhs (nth 3 splitted-on-equal)))
          (cons lhs rhs))
        (nth 1 splitted-on-equal))))


(defun equal-sign-at-end-p (string)
  "맨 끝의 whitespace를 무시하고, 주어진 string이 =로 끝나는지를 테스트"
  (let ((string_ (str:trim string)))
    (str:ends-with? "=" string_)))


(defun collect-simple-types-inner (lst acc)
  (if (null lst)
      acc
      (let ((line (car lst)))
        (if (and (or (search "type" line)
                     (search "and" line))
                 (not (equal-sign-at-end-p line)))
            (let* ((lhs (car (extract-type-name line)))
                   (rhs (cdr (extract-type-name line)))
                   (type-and-def (cons lhs rhs)))
              (collect-simple-types-inner (cdr lst) (cons type-and-def acc)))
            (collect-simple-types-inner (cdr lst) acc)))))


(defun collect-simple-types (lst)
  "각 type alias 정의에 대해서, type 이름과 alias의 쌍을 모은다."
  (collect-simple-types-inner lst ()))


(defun collect-complex-types-inner (lst acc)
  (if (null lst)
      acc
      (let ((line (car lst)))
        (if (and (or (search "type" line)
                     (search "and" line))
                 (equal-sign-at-end-p line))
            (let ((constructors (collect-constructors (cdr lst)))
                  (type-name (if (consp (extract-type-name line))
                                  (car (extract-type-name line))
                                  (extract-type-name line))))
              (let ((type-and-constructors (cons type-name constructors)))
                (collect-complex-types-inner (cdr lst) (cons type-and-constructors acc))))
            (collect-complex-types-inner (cdr lst) acc)))))


(defun collect-complex-types (lst)
  "여러 개의 constructor를 가진 type들에 대해, type 이름과 constructor list의 쌍을 모은다."
  (collect-complex-types-inner lst ()))


(defun collect-constructors-inner (lst acc)
  (if (null lst)
      acc
      (let ((line (car lst)))
        (if (str:starts-with? "|" (str:trim line))
            (collect-constructors-inner (cdr lst) (cons (str:trim line) acc))
            (collect-constructors-inner (cdr lst) acc)))))


(defun collect-constructors (lst)
  "하나의 type definition에 포함된 constructor list들을 모은다."
  (collect-constructors-inner lst ()))


(defvar *simple-types-and-aliases* (collect-simple-types *refined-grammar-lines*))


(defvar *complex-types-and-constructors* (collect-complex-types *refined-grammar-lines*))


(defun register-simple-types ()
  (loop for cons-cell in *simple-types-and-aliases* do
    (setf (gethash (car cons-cell) *type-constructor-table*) (cdr cons-cell))))


(defun register-complex-types ()
  (loop for list in *complex-types-and-constructors* do
    (setf (gethash (car list) *type-constructor-table*) (cdr list))))


;; for debugging purposes
(defun print-hash-table-entry (key value)
  (format t "~a => ~a~%" key value))


;; for debugging purposes
(defun print-hash-table (hashtbl)
  (maphash #'print-hash-table-entry hashtbl))


(defun main ()
  (register-simple-types)
  (register-complex-types))
