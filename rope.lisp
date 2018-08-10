;;;; The definition of the immutable rope data structure
;;;; author: Peter Elliott
;;;; licence: MIT

(defpackage :rope
  (:use :cl)
  (:export
    #:print-rope
    #:rope-ref
    #:split
    #:rope-len
    #:concat
    #:insert
    #:del-from
    #:chunks))

(in-package :rope)


(defstruct rope
  nl   ; length of left branch
  nnl  ; lines in left branch (currently unused)
  l    ; left branch
  r)   ; right branch


;; TODO: change sequence checks to ropep checks


(defun print-rope (rope)
  "prints a rope via princ"
  (if (typep rope 'sequence)
    (princ rope)
    (progn
      (print-rope (rope-l rope))
      (print-rope (rope-r rope)))))


(defun rope-ref (rope i)
  "gets the char in the rope at index i"
  (cond
    ((typep rope 'sequence) (aref rope i))
    ((< i (rope-nl rope)) (rope-ref (rope-l rope) i))
    (t (rope-ref (rope-r rope) (- i (rope-nl rope))))))


(defun split (rope i)
  "splits a rope at i into a list of two ropes"
  (cond
    ((typep rope 'sequence) (split-str rope i))
    ((= i (rope-nl rope)) (list (rope-l rope) (rope-r rope)))
    ((< i (rope-nl rope))
     (let ((left (split (rope-l rope) i)))
       (list (first left) (concat (second left) (rope-r rope)))))
    ((> i (rope-nl rope))
     (let ((right (split (rope-r rope) (- i (rope-nl rope)))))
       (list (concat (rope-l rope) (first right)) (second right))))))


(defun split-str (str i)
  "splits a string in to two strings at i"
  (list (subseq str 0 i) (subseq str i)))


(defun rope-len (rope)
  "gets the length of the rope"
  (if (typep rope 'sequence)
    (length rope)
    (+
      (rope-nl rope)
      (rope-len (rope-r rope)))))


(defun concat (rope1 rope2)
  "concatenates two ropes"
  (cond
    ((equal rope1 "") rope2)
    ((equal rope2 "") rope1)
    (t (make-rope :nl (rope-len rope1)
                  :l rope1
                  :r rope2))))


(defun insert (rope str i)
  "returns a rope with str inserted into rope at i"
  (let ((ropes (split rope i)))
      (concat (first ropes)
                 (concat str (second ropes)))))


(defun del-from (rope start end)
  "delete a subsequence from a rope"
  (concat
    (first (split rope start))
    (second (split rope end))))


(defun chunks (rope)
  "linearizes the rope into a list"
  (if (rope-p rope)
    (append
      (chunks (rope-l rope))
      (chunks (rope-r rope)))
    (list rope)))
