;;;-*- Mode: Lisp; Package: NET.CDDR.REDPAS -*-

(in-package "NET.CDDR.REDPAS")

;;; ===========================================================================

(defvar *lexer*)

(defun wrap-match (x)
  (cond ((atom x) `(match-token *lexer* ',x))
        ((and (consp x) (eq 'function (car x)))
         `(match-pred *lexer* ',x ,x))
        (t x)))

(defmacro seq (&rest forms)
  `(and ,@(mapcar #'wrap-match forms)))


(defmacro mv-or (&rest forms)
  (cond ((null forms) nil)
        ((null (cdr forms)) (car forms))
        (t 
         (let ((var '#:X))
         `(let ((,var (multiple-value-list ,(car forms))))
            (if (car ,var) (values-list ,var)
                (mv-or ,@(cdr forms))))))))

(defmacro mv2-or (&rest forms)
  (cond ((null forms) nil)
        ((null (cdr forms)) (car forms))
        (t 
         (let ((x '#:X)
               (y '#:y))
           `(multiple-value-bind (,x ,y) ,(car forms)
              (if ,x (values ,x ,y)
                  (mv2-or ,@(cdr forms))))))))

#|
 (defun testr (n)
  (mv2-or (values (= n 1) :a)
          (values (= n 2) :b)
          (values (= n 3) :c)))
|#


(defmacro alt (&rest forms)
  #+(or)
  (let ((fn (gensym "ALT")))
    `(flet ((,fn () (mv2-or ,@(mapcar #'wrap-match forms))))
       (if *failable*
         (,fn)
         (let ((*failable* t)
               (*failed-tokens* nil))
           (mv2-or (,fn) (error 'alt-error :lexer *lexer* :alt-tokens *failed-tokens*))))))
  `(let ((#1=#:outer-failable *failable*))
     (progv 
       (if *failable* '() '(*failable* *failed-tokens*))
       (if *failable* '() '(t nil))
       (mv2-or ,@(mapcar #'wrap-match forms)
               (unless #1#
                 (error 'alt-error :lexer *lexer* :alt-tokens *failed-tokens*))))))


(defmacro opt (&rest forms)
  `(let ((*failable* t) (*failed-tokens* nil))
    (seq ,@forms)
    (values t)))


(defmacro repeat ((&optional (min 0)) &body forms)
  `(seq ,@(loop repeat min append forms)
        (action (loop while (let ((*failable* t)
                                  (*failed-tokens* nil))
                              (seq ,@forms))))))


(defmacro seq+ (&rest syntax-seq)
  `(repeat (1) ,@syntax-seq))

(defmacro seq* (&rest syntax-seq)
  `(repeat (0) ,@syntax-seq))


(defmacro action (&body forms)
  `(values t (progn ,@forms)))

(defmacro -> (form &rest vars)
  (let ((result (gensym)))
    `(let (,result)
       (multiple-value-setq (,result ,@vars) ,(wrap-match form))
       ,result)))

