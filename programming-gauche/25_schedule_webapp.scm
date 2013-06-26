#!/usr/bin/env gosh

(use util.list)
(use text.html-lite)
(use srfi-1)
(use srfi-13)
(use srfi-19)
(use www.cgi)
(use gauche.sequence)
(use dbm.fsdbm)
(use gauche.charconv)
(use gauche.parameter)

(define db (make-parameter #f))
(define *db-name* "/Users/hash/gauche_schedule.data")
(define *style* "
  span.planned {
    background-color: #ffcccc;
  }
")

(define (make-month m y)
  (make-date 0 0 0 0 1 m y (date-zone-offset (current-date))))

(define (first-day-of-month date)
  (make-month (date-month date) (date-year date)))

(define (next-month date)
  (if (= (date-month date) 12)
    (make-month 1 (+ (date-year date) 1))
    (make-month (+ (date-month date) 1) (date-year date))))

(define (prev-month date)
  (if (= (date-month date) 1)
    (make-month 12 (- (date-year date) 1))
    (make-month (- (date-month date) 1) (date-year date))))

(define (days-of-month date)
  (- (date->modified-julian-day (next-month date))
     (date->modified-julian-day (first-day-of-month date))))

(define (date-slices-of-month date)
  (slices (append (make-list (date-week-day (first-day-of-month date)) #f)
                  (iota (days-of-month date) 1))
          7 #t #f))

(define (month->link date content)
  (html:a :href #`"?y=,(date-year date)&m=,(date-month date)" content))

(define (date-cell year month date)
  (if date
    (html:a :href #`"?y=,|year|&m=,|month|&d=,|date|"
            (if (dbm-exists? (db) (dbm-key year month date))
              (html:span :class "planned" date)
              date))
    ""))

(define (calendar date)
  (html:table
    (html:tr (html:td (month->link (prev-month date) "←"))
             (html:td :colspan 5 :align "center"
                      #`",(date-year date)/,(date-month date)")
             (html:td (month->link (next-month date) "→")))
    (html:tr (map html:td "日月火水木金土"))
    (map (lambda (w) (html:tr
                       (map (lambda (d) (html:td (date-cell (date-year date) (date-month date) d)))
                            w)))
         (date-slices-of-month date))))

(define (page . content)
  `(,(cdi-header
       :content-type #`"text/html; char-set=utf-8")
       ,(html-doctype :type :xhtml-1.0-transitional)
       ,(html:html
          (html:head (html:title "簡易スケジュール表")
                     (html:style :type "text/css" *style*))
          (apply html:body content))))

(define (cmd-show-calendar y m)
  (page
    (if (and y m (<= 1 m 12) (<= 1753 y))
      (calendar (make-month m y))
      (calendar (current-date)))))

(define (cmd-show-plan y m d)
  (let ((plan (dbm-get (db) (dbm-key y m d) "")))
    (page
      (calendar (make-month m y))
      (html:form
        (html:p #`",|y|年,|m|月,|d|日の予定")
        (html:pre (html-escape-string plan))
        (html:a :href #`"?y=,|y|&m=,|m|&d=,|d|&c=e" "[予定の編集]")))))

(define (cmd-edit-plan y m d)
  (let ((plan (dbm-get (db) (dbm-key y m d) "")))
    (page
      (html:form
        (html:p #`",|y|年,|m|月,|d|日の予定")
        (html:input :type "hidden" :name "c" :value "c")
        (html:input :type "hidden" :name "y" :value (x->string y))
        (html:input :type "hidden" :name "m" :value (x->string m))
        (html:input :type "hidden" :name "d" :value (x->string d))
        (html:p (html:textarea :rows 8 :cols 40 :name "p"
                               (html-escape-string plan)))
        (html:p (html:input :type "submit" :name "submit" :value "変更"))))))

(define (cmd-change-plan y m d plan)
  (if (string-null? plan)
    (dbm-delete! (db) (dbm-key y m d))
    (dbm-put! (db) (dbm-key y m d) plan))
  (cdi-header :status "302 Moved"
              :location #`"?y=,|y|&m=,|m|&d=,|d|"))


(define-syntax with-db
  (syntax-rules ()
    ((with-db (db-path) . body)
     (parameterize
       ((db (dbm-open <fsdbm> :path path :rw-mode :write)))
       (guard
         (e (else (dbm-close (db)) (raise e)))
         (begin0
           (begin . body)
           (dbm-close (db))))))))

(define (dbm-key y m d) (#`",|y|-,|m|-,|d|"))

(define (main args)
  (cgi-main
    (lambda (params) (let ((y (cgi-get-parameter "y" params :convert x->integer))
                           (m (cgi-get-parameter "m" params :convert x->integer))
                           (d (cgi-get-parameter "d" params :convert x->integer))
                           (cmd (cgi-get-parameter "c" params))
                           (plan (cgi-get-parameter "p" params
                                                    :convert (cut ces-convert <> "*JP"))))
                       (cgi-output-character-encoding 'utf-8)
                       (with-db (db *db-name*)
                                (if (and y m d)
                                  (cond
                                    ((equal? cmd "e")
                                     (cmd-edit-plan y m d))
                                    ((equal? cmd "c")
                                     (cmd-change-plan y m d plan))
                                    (else
                                      (cmd-show-plan y m d)))
                                  (cmd-show-calendar y m)))))))


;; 文字埋め込みの書式
;;     #`",(date-year date)/,(date-month date)")

