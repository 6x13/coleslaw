(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'plump)
  (ql:quickload 'drakma))

(defpackage :coleslaw-shouts
  (:use :cl)
  (:export #:shout #:enable)
  (:import-from :coleslaw
                #:*indexed-content-types*
                #:*config*
                #:content
                #:title
                #:author
                #:card
                #:format
                #:url
                #:text
                #:by-date
                #:make-keyword
                #:slugify
                #:find-all
                #:compute-url
                #:render
                #:publish
                #:theme-fn
                #:render-text
                #:write-document))

(in-package :coleslaw-shouts)

(defvar *link* "http://blender.graphics/posts/A-Brand-New-Source-of-Blender-Knowledge.html")

(defun get-property (node)
  (or (plump-dom:get-attribute node "property")
      (plump-dom:get-attribute node "name")))

(defun get-content (property property-list)
  (cdr (assoc property property-list :test #'string-equal)))

(defun make-keyword-property (property)
  "Crops property namespaces. Sets nil properties and properties without a namespace to nil so they can be removed later. All social metadata has a property/name attribute with a namespace. Then turns property into a keyword."
  (let ((namespace-tail (position #\: property)))
     (when namespace-tail
         (intern (string-upcase
                  (substitute #\- #\_
                              (subseq property
                                      (+ namespace-tail 1))))
                 :keyword))))

(defun pair-metadata (node)
  (cons
   (make-keyword-property (get-property node))
   (plump-dom:get-attribute node "content")))

(defun get-link-data (link)
  (let* ((response (drakma:http-request link))
         (dom (plump:parse response))
         (metadata (plump-dom::get-elements-by-tag-name dom "meta")))
    (remove-if-not #'car (mapcar #'pair-metadata metadata))))

(defclass shout (content)
  ((title  :initarg :title  :reader title-of)
   (author :initarg :author :reader author-of)
   (format :initarg :format :reader post-format)
   (description :initarg :description :reader description-of)
   (image :initarg :image :reader image-of)
   (card :initarg :card :reader card-format)
   (creator :initarg :creator :reader creator-of))
  (:default-initargs
   :author nil
   :description nil
   :image nil
   :card nil
   :creator nil))

(push 'shout *indexed-content-types*)

(defmethod initialize-instance :after ((object shout) &key)
  (with-slots (url title author format card text) object
    (setf url "www.google.com"
          format (make-keyword (string-upcase format))
          text (render-text text format)
          author (or author (author *config*))
          card (if card (make-keyword (string-upcase card))))))

(defmethod render ((object shout) &key prev next))

(defmethod publish ((doc-type (eql (find-class 'shout)))))

(defun enable ())
