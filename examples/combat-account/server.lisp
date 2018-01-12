(in-package :cl-user)

(unless (boundp '*orb*)
  (defvar *orb* (CORBA:ORB_init)))

(defclass my-bank (omg.org/root:bank-servant)
  ((accounts :initform nil
             :accessor accounts)))

(corba:define-method create ((bank my-bank) name password)
  (let ((account (assoc name (accounts bank)
                        :test #'string=)))
    (cond (account
           (unless (equal password (second account))
             (error 'omg.org/root:bank/notauthorized))
           (third account))
          (t
           (let ((acc-obj (op:_this (make-instance 'my-account))))
             (push (list name password acc-obj)
                   (accounts bank))
             acc-obj)))))


(defclass my-account (omg.org/root:account-servant)
  ((balance :initform 0
            :accessor balance)))

(corba:define-method balance ((account my-account))
  (balance account))

(corba:define-method deposit ((account my-account) amount)
  (incf (balance account) amount))

(corba:define-method withdraw ((account my-account) amount)
  (when (> amount (balance account))
    (error 'omg.org/root:account/bankrupt
           :balance (balance account)
           :amount amount))
  (incf (balance account) (- amount)))

(defvar *bank-servant* (make-instance 'my-bank))

(defun setup-bank (&key (file (merge-pathnames "server.ior" *account-folder*)))
  (with-open-file (ior-file file
                            :direction :output
                            :if-exists :supersede)
    (write-string (op:object_to_string *orb* (op:_this *bank-servant*))
                  ior-file)))


