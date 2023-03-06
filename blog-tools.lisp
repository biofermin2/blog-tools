# -*- coding: utf-8 -*-

(defstruct blog title author since license) ; =>BLOG 
(defvar blog (make-blog :title "(peek-char t *sexp-life*)"
                        :author "biofermin2"
                        :since "2022"
                        :license "©biofermin2")) ; =>BLOG 
(defstruct menu first lisp agri others)              ; =>MENU 
(defvar menu1 (make-menu))                           ; =>MENU1 
(defvar menu-lisp (make-menu))                       ; =>MENU-LISP 
(defvar menu-agri (make-menu))                       ; =>MENU-AGRI 
(defvar menu-others (make-menu))                     ; =>MENU-OTHERS 
(defstruct article title date text link)             ; =>ARTICLE 
(defun make-blog-menu ()
  (let ((a1 (make-article :title "first"
                          :link "./first.html"))
        (a2 (make-article :title "lisp"
                          :link "./lisp-menu.lisp.html"))
        (a3 (make-article :title "agriculture"
                          :link "./agri-menu.lisp.html"))
        (a4 (make-article :title "others"
                          :link "./others-menu.lisp.html")))
    (setf (menu-first menu1) a1
          (menu-lisp menu1) a2
          (menu-agri menu1) a3
          (menu-others menu1) a4)
    (format t "(:menu ~{[[~a][~a]]~^ ~})"
            (list (article-link a1) (article-title a1)
                  (article-link a2) (article-title a2)
                  (article-link a3) (article-title a3)
                  (article-link a4) (article-title a4))))) ; =>MAKE-BLOG-MENU 
(make-blog-menu)                                           ; =>(:menu [[./first.html][first]] [[./lisp-menu.lisp.html][lisp]] [[./agri-menu.lisp.html][agriculture]] [[./others-menu.lisp.html][others]])NIL 

(defparameter blog-home (merge-pathnames "biofermin2.github.io/docs/" (user-homedir-pathname))) ; =>BLOG-HOME
(defun get-titles (org-file)
  (with-open-file (in org-file :direction :input)
    (loop :for line = (read-line in nil)
          :while line
          :with titles
          :do (cond ((and (> (length line) 2) (string-equal (subseq line 0 2) "* "))
                     (push (subseq line 2) titles))
                    ((and (> (length line) 3) (string-equal (subseq line 0 3) "** "))
                     (push (subseq line 3) titles)))
          :finally (return (reverse titles))))) ; =>GET-TITLES 

(defun scan (key obj)
  (let ((key-len (length key))
        (start (search key obj)))
    (when start
      (values start (+ start key-len))))) ; =>SCAN 

(defun %s (old new obj)
  (multiple-value-bind (start end)
      (scan old obj)
    (if start
        (let* ((rest (subseq obj end))
               (result (%s old new rest)))
          (concatenate 'string (subseq obj 0 start) new result))
        obj)))                          ; =>%S 

(defun make-link (html-file)
  (let* ((html-path (merge-pathnames html-file blog-home))
         (html->org (make-pathname :type "org" :defaults html-path))
         (org-file (if (probe-file html->org)
                       html->org
                       (make-pathname :name (%s ".org" "" (pathname-name html-file))
                                      :type "org" :defaults html-path)))
         (titles (get-titles org-file)))
    (with-output-to-string (s)
      (loop :initially (format s "** [[./~a][~a]]~%" html-file (car titles))
            :for title :in (cdr titles)
            :do (format s " - ~a~%" title))))) ; =>MAKE-LINK 

(defun menu (html-file)
  (cd blog-home) 
  (format t "~{~a~}" (mapcar #'(lambda (x) (make-link x)) (reverse (ll html-file))))) ; =>MENU 

(menu "202?/*.html")                    ; =>
(defun main ()


  )
