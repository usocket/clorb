;;; clorb-idllexer.lisp -- A simple lexer for IDL
;;
;; The lexer uses clorb-idlcpp to preprocess using the system cpp and
;; the net.cddr.redpas macros to do the lexical analysis.
;;

(in-package :clorb)


(defclass IDLLEX (lexer)
  ((lexer
    :initarg :lexer
    :accessor idllex-lexer)))

(defclass cppchar-lexer (lexer)
  ((stream
    :initarg :cpp
    :accessor lexer-cpp)
   (line :initform "")
   (pos  :initform 1)))


(defmethod next-token ((lexer cppchar-lexer))
  (with-slots (stream line pos) lexer
    (do ((token nil))
        (token (setf (slot-value lexer 'token) token))
      (cond ((= pos (length line))
             (incf pos)
             (setf token #\Newline))
            ((< pos (length line))
             (setf token (char line pos))
             (incf pos))
            (t
             (setf line (read-cpp-line stream))
             (setf pos 0)
             (if (null line)
               (setf token :eof)))))))

(defvar *current-cpp*)

(defun using-idllex (file thunk include-directories &optional defines)
  (check-type file (or string pathname))
  (using-cpp-stream
   file
   (lambda (cpp)
     (let ((*current-cpp* cpp)
           (lexer (make-instance 'cppchar-lexer :cpp cpp)))
       (next-token lexer)
       (let ((*lexer* (make-instance 'idllex :lexer lexer)))
         (next-token *lexer*)
         (funcall thunk))))
   :include-directories include-directories
   :defines defines))


(defmethod next-token ((lexer idllex))
  (let ((*lexer* (idllex-lexer lexer)))
    (loop until
      (multiple-value-bind (flag token) (idl-token)
        (assert flag)
        (setf (slot-value lexer 'token) token)))))

(defun idl-token ()
  (whitespace)
  (alt (seq :eof (action :eof))
       (idl-comment)
       (identifier-token)
       (string-literal)
       (number-token)
       (char-literal)
       (char-token)))

(defun whitespace ()
  (loop
    with newline = nil
    and skip-line = nil
    for token = (token *lexer*)
    do (cond ((or (eql token #\Newline)
                  (eql token #\Linefeed))
              (setq newline t
                    skip-line nil))
             (skip-line)
             ((and newline (eql token #\#))
              (setq skip-line t))
             ((eql token #\Space))
             ((eql token #\Tab))
             (t (loop-finish)))
    (next-token *lexer*)))


(defun idl-comment ()
  (flet ((not-* (ch) (char/= ch #\*))
         (not-/ (ch) (char/= ch #\/))
         (not-newline (ch) (char/= ch #\Newline)))
    (seq #\/
         (alt (seq #\*
                   (seq* #'not-*)
                   #\*
                   (seq* #'not-/ (seq* #'not-*) #\*)
                   #\/
                   (action nil))
              (seq #\/
                   (seq* #'not-newline)
                   (action nil))
              (seq (action "/"))))))


(defun ident-start-char-p (c)
  (or (alpha-char-p c)
      (char= c #\_)))

(defun ident-char-p (c)
  (or (alphanumericp c)
      (char= c #\_)))

(defun identifier-token ()
  (let (ch name)
    (seq (-> #'ident-start-char-p ch)
         (action (push ch name))
         (seq*
           (-> #'ident-char-p ch)
           (action (push ch name)))
         (action (coerce (nreverse name) 'string)))))


(defun hex-char-p (c)
  (and (characterp c)
       (digit-char-p c 16)))

(defun hex-char ()
  (let ((ch (token *lexer*)))
    (let ((n (hex-char-p ch)))
      (if n (progn (match-token *lexer* ch) (action n))))))

(defun hex-escape ()
  (let ((result 0) n)
    (loop repeat 2 while (-> (hex-char) n)
          do (setq result (+ (* result 16) n)))
    (action (code-char result))))

(defun oct-char ()
  (let ((ch (token *lexer*)))
    (let ((n (and (characterp ch)
                  (digit-char-p ch 8))))
      (if n (progn (match-token *lexer* ch) (action n))))))

(defun oct-escape (first-token)
  (let ((result (digit-char-p first-token)) n)
    (loop repeat 2 while (-> (oct-char) n)
          do (setq result (+ (* result 8) n)))
    (action (code-char result))))


(defun escaped-char (terminator-char)
  (block nil
    (let ((token (token *lexer*)))
      (when (or (eql token terminator-char)
                (not (characterp token)))
        (return nil))
      (match-token *lexer* token)
      (unless (eql token #\\) (return (values t token)))
      (seq (-> #'characterp token)
           (case token
             ((#\n) (action #\newline))
             ((#\t) (action #\tab))
             ((#\v) (action (code-char 11)))
             ((#\b) (action #\backspace))
             ((#\r) (action #\return))
             ((#\f) (action #\page))
             ((#\a) (action #\bell))
             ((#\x) (hex-escape))
             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) (oct-escape token))
             (otherwise (action token)))))))


(defun string-literal-1 ()
  (let (ch chs)
    (seq #\"
         (seq* (-> (escaped-char #\") ch)
               (push ch chs))
         #\"
         (action (coerce (nreverse chs) 'string)))))

(defun string-literal ()
  (let ((result nil) s)
    (seq (-> (string-literal-1) result)
         (seq* (action (whitespace))
               (-> (string-literal-1) s)
               (action (setq result (concatenate 'string result s))))
         (action (cons 'string result)))))


(defun char-literal ()
  (let (ch)
    (seq #\' (-> (escaped-char #\') ch) #\'
         (action ch))))


(defun decimal-char-p (c)
  (and (characterp c)
       (digit-char-p c)))


(defun idl-fixed-p (n)
  (and (consp n)
       (eql 'fixed (car n))))

(defun idl-fixed-values (value)
  "Values: digits scale number"
  (values-list (cdr value)))

(defun make-idl-fixed (digits scale number)
  (when (> digits 31)
    (psetq digits 31
           scale (+ (- 31 digits) scale))
    (let ((multiplier (expt 10 scale)))
      (setq number (/ (truncate (* number multiplier)) multiplier))))
  `(fixed ,digits ,scale ,number))


(defun parse-fixed (string)
  (multiple-value-bind (n pos) 
                       (if (eql (char string 0) #\.)
                         (values 0 0) 
                         (parse-integer string :junk-allowed t))
    (if (> (length string) (1+ pos))
      (let ((decimals (parse-integer string :start (1+ pos)))
            (scale    (- (length string) pos 1))) 
        (let ((digits   (+ pos scale))
              (multiplier (expt 10 scale)))
          (make-idl-fixed digits scale 
                          (/ (+ (* multiplier n) decimals) multiplier))))
      (make-idl-fixed pos 0 n))))


(defun number-token ()
  (let ((chars '())
        (base nil))
    (macrolet ((c (&rest seq)
                 (let ((f '#:f) (c '#:c))
                   `(multiple-value-bind (,f ,c) (seq ,@seq)
                      (when ,f
                        (push ,c chars)
                        t)))))
      (seq (opt (seq (c #\0)
                     (action (setq base 8))
                     (opt (seq (alt #\x #\X)) 
                          (action (setq base 16)))))
           (if (eql base 16)
             (seq+ (c #'hex-char-p))
             (seq (seq* (c #'decimal-char-p))
                  (opt (seq (c #\.) (seq* (c #'decimal-char-p))
                            (action (setq base nil))))
                  (alt (seq (alt #\e #\E)
                            (action (push #\D chars)
                                    (setq base nil))
                            (opt (alt (c #\+) (c #\-)))
                            (seq+ (c #'decimal-char-p)))
                       (seq (alt #\d #\D)
                            (action (setq base 'fixed)))
                       (seq (and chars)))))
           (action 
             (let ((string (coerce (nreverse chars) 'string)))
               (cond ((eql base 'fixed)
                      (parse-fixed string))
                     (base
                      (parse-integer string :radix base))
                     (t
                      (let ((*read-default-float-format* 'double-float))
                        (read-from-string string))))))))))


(defun char-token ()
  (alt (seq #\: (alt (seq #\: (action "::"))
                     (seq     (action ":"))))
       (seq #\< (alt (seq #\< (action "<<"))
                     (seq     (action "<"))))
       (seq #\> (alt (seq #\> (action ">>"))
                     (seq     (action ">"))))
       (let (c)
         (seq (-> #'identity c)
              (action (string c))))))



#|
 (with-input-from-string (s "123.44 foo, ")
   (let ((*lexer* (make-instance 'net.cddr.redpas:streamchar-lexer
                                 :stream s)))
     (next-token *lexer*)
     (number-token)
     (idl-token)
     (idl-token)))
|#
