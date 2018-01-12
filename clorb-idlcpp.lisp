;;;; clorb-idlcpp -- preprocessor interface

(in-package :clorb)

(defgeneric idl-prefix (preprocessed-stream)
  (:documentation "Return the current prefix as specified by #pragma prefix in the preprocessed files."))

(defgeneric package-prefix (preprocessed-stream)
  (:documentation 
   "Return the current package-prefix as specified by #pragma package-prefix in the preprocessed files."))

(defgeneric idl-source-position (preprocessed-stream)
  (:documentation "Return (FILE LINE# ..) for last returned line from the stream"))

(defgeneric idl-repositoryid-pragmas (preprocessed-stream))


(defclass PREPROCESSED-STREAM ()
  ((cpp-stream :initarg :cpp-stream :accessor cpp-stream)
   (file-stack
    :initform (list (list "" 0 "" ""))
    :accessor cpp-file-stack
    :documentation "Keep tracks of nested include files. The stack has records containing: (file line# prefix package-prefix)")
   (pragma-list 
    :initform nil
    :accessor cpp-pragma-list)))

(defmethod idl-repositoryid-pragmas ((self preprocessed-stream))
  (prog1 (cpp-pragma-list self)
    (setf (cpp-pragma-list self) nil)))

(defmethod idl-source-position ((self preprocessed-stream))
  (car (cpp-file-stack self)))

(defmethod idl-prefix ((self preprocessed-stream))
  (caddar (cpp-file-stack self)))

(defmethod (setf idl-prefix) (value (self preprocessed-stream))
  (setf (caddar (cpp-file-stack self)) value))

(defmethod package-prefix ((self preprocessed-stream))
  (fourth (car (cpp-file-stack self))))

(defmethod (setf package-prefix) (value (self preprocessed-stream))
  (setf (fourth (car (cpp-file-stack self))) value))


(defmethod current-file ((self preprocessed-stream))
  (caar (cpp-file-stack self)))

(defmethod (setf current-file) (value (self preprocessed-stream))
  (setf (caar (cpp-file-stack self)) value))

(defmethod current-line ((self preprocessed-stream))
  (cadar (cpp-file-stack self)))

(defmethod (setf current-line) (value (self preprocessed-stream))
  (setf (cadar (cpp-file-stack self)) value))


(defmethod read-raw-line ((self preprocessed-stream))
  (incf (current-line self))
  (read-line (cpp-stream self) nil nil))

(defmethod new-file-info ((self preprocessed-stream) file line why)
  (case why
    (:initial (setf (current-file self) file))
    (:push (push (list file line "" "")
                 (cpp-file-stack self)))
    (:pop (assert (cdr (cpp-file-stack self)))
          (pop (cpp-file-stack self))
          (unless (equal file (current-file self))
            (warn "Incosistent line markers in cpp stream"))))
  (setf (current-line self) (1- line)))


(defun read-cpp-line (cpp)
  (let ((line (read-raw-line cpp)))
    (cond ((null line) nil)
          ((cpp-line-p line)
           (parse-cpp-line cpp line)
           "")
          (t
           line))))



;;;; Parse helpers

(defun parse-string-literal (string delimiter &key (start 0))
  (unless (eql delimiter (char string start))
    (error "No string delimiter (~S): ~S" delimiter (subseq string start)))
  (incf start)
  (let ((result
         (with-output-to-string (out)
           (loop for char = (char string start)
                 ;;do (break "char=~S" char)
                 do (cond ((eql char #\\)        ; escape
                           (multiple-value-setq (char start) (parse-escape-sequence string (1+ start)))
                           (princ char out))
                          ((eql char delimiter)
                           (incf start)
                           (loop-finish))
                          (t
                           (princ char out)
                           (incf start)))))))
    (values result start)))


(defun parse-numeric-character (string start radix)
  (loop with code = 0
        for char = (if (< start (length string))
                     (char string start)
                     #\Space)
        for weight = (digit-char-p char radix)
        while weight
        do (setq code (+ (* code radix) weight))
        ;;(break "code=~S" code)
        (incf start)
        finally (return (values (code-char code) start))))

(defun parse-escape-sequence (string &optional (start 0))
  (let ((char (char string start)))
    (cond ((digit-char-p char 8)
           (parse-numeric-character string start 8))
          ((char-equal char #\x)
           (incf start)
           (parse-numeric-character string start 16))
          (t
           ;; singel char sequence
           (values (case char
                     (#\n #\newline)
                     (#\r #\return)
                     (#\t #\tab)
                     (otherwise char))
                   (1+ start))))))


;;;; Parse preprocessing line

(defun cpp-line-p (line)
  (loop for char across line
        when (char= char #\#) return t
        unless (whitespacep char) return nil))

(defun whitespacep (char)
  (or (char= char #\Space)
      (char= char #\Tab)))


(defvar *warn-unknown-pragma* t)

(defun parse-cpp-line (cpp line)
  (setq line (read-cpp-continuation-lines cpp line))
  (let ((pos (position #\# line))
        char )
    (labels ((set-pos (newpos) 
               (setq pos newpos)
               (setq char (if (< pos (length line))
                            (char line pos) #\Null)))
             (next () (set-pos (1+ pos)))
             (whitespacep () (or (eql char #\Space) (eql char #\Tab)))
             (skip-ws () (loop while (whitespacep) do (next)))
             (match (str)
               (and (>= (length line) (+ pos (length str)))
                    (string= str line :start2 pos :end2 (+ pos (length str)))
                    (set-pos (+ pos (length str)))))
             (string-literal ()
               (skip-ws)
               (multiple-value-bind (string end)
                                    (parse-string-literal line #\" :start pos)
                 (set-pos end)
                 string))
             (ident ()
               (skip-ws)
               (let ((start pos))
                 (loop until (or (whitespacep) (eql char #\Null)) do (next))
                 (subseq line start pos))))
      (next) (skip-ws)
      (cond ((digit-char-p char)
             ;; EX: # 1 "working.idl"
             (multiple-value-bind (n newpos) (parse-integer line :start pos :junk-allowed t)
               (set-pos newpos)
               (let ((file (string-literal)))
                 (skip-ws) 
                 (new-file-info cpp file n
                                (cond ((eql char #\1) :push)
                                      ((eql char #\2) :pop)
                                      ((eql char #\Null) :initial))))))
            ((match "pragma")
             (skip-ws)
             (cond ((match "prefix")
                    (setf (idl-prefix cpp) (string-literal)))
                   ((match "package-prefix")
                    (setf (package-prefix cpp) (string-literal)))
                   ((match "ID")
                    (let* ((name (ident))
                           (id (string-literal)))
                      (push (list :id name id)
                            (cpp-pragma-list cpp))))
                   ((match "version")
                    (let* ((name (ident))
                           (version (ident)))
                      (push (list :version name version)
                            (cpp-pragma-list cpp))))
                   ((match "GCC"))      ; known system specific pragma
                   (*warn-unknown-pragma*
                    (warn "Unknown pragma: ~S" line))))
            (t
             (warn "Unknown preprocessor directive: ~S" line))))))



(defun read-cpp-continuation-lines (cpp line)
  (loop for last-char-index = (1- (length line))
        while (char= #\\ (char line last-char-index))
        do (setq line (concatenate 'string 
                                   (subseq line 0 last-char-index)
                                   (read-raw-line cpp))))
  line)


;;;; Run the system cpp command


(defun using-cpp-stream (file function &key include-directories defines)
  (let ((s (shell-to-string (cpp-command-string file include-directories defines))))
    (with-input-from-string (in s)
      (let ((cpp (make-instance 'preprocessed-stream
                   :cpp-stream in)))
        (funcall function cpp)))))


