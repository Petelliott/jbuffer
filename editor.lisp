
(defpackage :editor
  (:use :cl)
  (:export
    #:open-buff
    #:write-buff
    #:insert
    #:del-from
    #:undo
    #:redo))


(in-package :editor)


(defstruct buffer
  stack  ; the stack of buffer snapshots (terminated when car is not list)
  redo   ; the stack of undone snapshots
  dirty  ; whether or not the buffer has been updated
  fname) ; the file the buffer is associated with


(defun buffer-head (buf)
  (let ((stack (buffer-stack buf)))
    (if (listp stack)
      (car stack)
      stack)))


(defun fname-to-istring (fname)
  (with-open-file (strm fname)
    (let ((contents (make-string (file-length strm))))
      (read-sequence contents strm)
      (istring:make-istring contents))))


(defun open-buff (fname)
  (make-buffer
    :stack (fname-to-istring fname)
    :redo nil
    :dirty nil
    :fname fname))


(defun write-buff (buff &optional fname)
  (if (null fname) (setf fname (buffer-fname buff)))

  (with-open-file (f fname
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (mapcar
      (lambda (chunk)
        (princ chunk f))
      (rope:chunks (buffer-head buff))))

  (make-buffer
    :stack (buffer-stack buff)
    :redo  (buffer-redo buff)
    :dirty nil
    :fname (buffer-fname buff)))


(defun insert (buff str i)
  (make-buffer
    :stack (cons
             (rope:insert (buffer-head buff) str i)
             (buffer-stack buff))
    :redo nil
    :dirty t
    :fname (buffer-fname buff)))


(defun del-from (buff start end)
  (make-buffer
    :stack (cons
             (rope:del-from (buffer-head buff) start end)
             (buffer-stack buff))
    :redo nil
    :dirty t
    :fname (buffer-fname buff)))


(defun undo (buff)
  (if (listp (buffer-stack buff))
    (make-buffer
      :stack (cdr (buffer-stack buff))
      :redo  (cons
               (car (buffer-stack buff))
               (buffer-redo buff))
      :dirty t  ;TODO: attach dirtyness to snapshot
      :fname (buffer-fname buff))
    buff))


(defun redo (buff)
  (if (buffer-redo buff)
    (make-buffer
      :stack (cons
               (car (buffer-redo buff))
               (buffer-stack buff))
      :redo  (cdr (buffer-redo buff))
      :dirty t ;TODO: attatch dirtyness to snapshot
      :fname (buffer-fname buff))
    buff))
