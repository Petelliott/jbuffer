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
    #:del-from))

(in-package :rope)


(defun print-rope (rope)
  "prints a rope via princ"
  (if (stringp rope)
    (princ rope)
    (progn
      (print-rope (second rope))
      (print-rope (third rope)))))


(defun rope-ref (rope i)
  "gets the char in the rope at index i"
  (cond
    ((stringp rope) (aref rope i))
    ((< i (first rope)) (rope-ref (second rope) i))
    (t (rope-ref (third rope) (- i (first rope))))))


(defun split (rope i)
  "splits a rope at i into a list of two ropes"
  (cond
    ((stringp rope) (split-str rope i))
    ((= i (first rope)) (list (second rope) (third rope)))
    ((< i (first rope))
     (let ((left (split (second rope) i)))
       (list (first left) (concat (second left) (third rope)))))
    ((> i (first rope))
     (let ((right (split (third rope) (- i (first rope)))))
       (list (concat (second rope) (first right)) (second right))))))


(defun split-str (str i)
  "splits a string in to two strings at i"
  (list (subseq str 0 i) (subseq str i)))


(defun rope-len (rope)
  "gets the length of the rope"
  (if (stringp rope)
    (length rope)
    (+
      (first rope)
      (rope-len (third rope)))))


(defun concat (rope1 rope2)
  "concatenates two ropes"
  (cond
    ((equal rope1 "") rope2)
    ((equal rope2 "") rope1)
    (t (list (rope-len rope1) rope1 rope2))))


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
