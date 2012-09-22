;; -*- coding: utf-8; mode: kahua -*-
;;
;; symlinked from
;;   schedule/schedule/schedule.kahua
;
;
; /Users/hash/work/lang/gauche/schedule
; ├── AUTHORS
; ├── COPYING
; ├── ChangeLog
; ├── DIST
; ├── DIST_EXCLUDE
; ├── INSTALL
; ├── MESSAGES
; ├── Makefile
; ├── Makefile.in
; ├── README
; ├── VERSION
; ├── app-servers
; ├── autom4te.cache
; ├── checkout -> .
; ├── config.log
; ├── config.status
; ├── configure
; ├── configure.ac
; ├── install-sh
; ├── plugins
; ├── schedule
; │   ├── schedule.css
; │   ├── schedule.kahua -> ../../27_kahua.scm
; │   ├── version.kahua
; │   └── version.kahua.in
; ├── templates
; └── test

(load "schedule/version.kahua")

(use srfi-1)
(use srfi-13)
(use srfi-19)
(use util.list)

;; copy-and-paste from 25_schedule_webapp.scm >>>
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
;; <<< copy-and-paste from 25_schedule_webapp.scm


(define page-template
  (kahua:make-xml-template
   (kahua-template-path "schedule/page.xml")))

(define (calendar-head/ date entry)
  (define (month/cont/ date entry content)
    (a/cont/ (@@/ (cont entry (date-year date) (date-month date) 1)) content))
  (define (today/cont/ entry content)
    (let1 today (current-date)
          (a/cont/ (@@/ (cont entry (date-year today) (date-month today) (date-day today)))
                   content)))
  (thead/
    (tr/ (th/ (month/cont/ (prev-month date) entry "<="))
         (th/ (@/ (colspan "5"))
              (today/cont/ entry (date->string date "~Y年~m月")))
         (th/ (month/cont/ (next-month date) entry "=>")))))

(define (calendar-body/ date entry)
  (define (planned? date day)
    (and day (find-kahua-instance <schedule> 'date (date->dbkey date day)) "planned"))
  (define (date-cell/ date day entry)
    (td/ (@/ (class (planned? date day)))
         (and day (a/cont/ (@@/ (cont entry (date-year date) (date-month date) day)) day))))
  (tbody/ (@/ (class "calendar-body"))
          (map/ (lambda (w) (tr/ (map/ (cut date-cell/ date <> entry) w)))
                (date-slices-of-month date))))

(define (calendar/ date)
  (table/
    (calendar-head/ date entry)
    (calendar-body/ date entry)))

(define (ymd->date y m d)
  (let1 now (current-date)
        (cond (y (let ((m (if m (x->integer m) 1))
                       (d (if d (x->integer d) 1)))
                   (make-date 0 0 0 0
                              (if (<= 1 d 31) d 1)
                              (if (<= 1 m 12) m 1)
                              (x->integer y) (date-zone-offset now))))
              (else now)))

(define-entry (show y m d)
  (let1 date (ymd->date y m d)
        (kahua:xml-template->sxml
          page-template
          :title (title/ (date->string date "~Y年~m月~d日"))
          :body  (div/
                   (calendar/ date show)
                   (schedule-show/ date)))))

(define-entry (edit y m d)
  (let1 date (ymd->date y m d)
        (kahua:xml-template->sxml
          page-template
          :title (title/ (date->string date "~Y年~m月~d日 [編集中]"))
          :body  (div/
                   (calendar/ date edit)
                   (schedule-edit/ date)))))

(define-class <schedule> (<kahua-persistent-base>)
  ((date :init-keyword :date :allocation :persistent :index :unique)
   (memo :init-keyword :memo :allocation :persistent)))

(define (date->dbkey date . maybe-day)
  (let1 day (get-optional maybe-day (date-day date))
        (format "~d-~2,'0d-~2,'0d" (date-year date) (date-month date) day)))

(define (schedule-commit sch date memo)
  (cond [sch (if (or (not memo) (string-null? memo))
               (remove-kahua-instance sch)
               (set! (ref sch 'memo) memo))]
        [else (make <schedule>
                    :date (date->dbkey date)
                    :memo memo)]))

(define (schedule-edit/ date)
  (let ((y (date-year date))
        (m (date-month date))
        (d (dateday date))
        (sch (find-kahua-instance <schedule> 'date (date->dbkey date))))
    (node-set/
      (p/ (format "~d年~d月~d日" y m d) "の予定を編集"
          (a/cont/ (@@/ (cont show y m d)) "[戻る]"))
      (form/cont/ (@@/ (cont (entry-lambda (:keyword memo)
                                           (schedule-commit sch date memo)
                                           (redirect/cont (cont show y m d)))))
                  (textarea/ (@/ (name "memo")) (and sch (ref sch 'memo)))
                  (input/ (@/ (value "保存") (type "submit")))))))

(define (schedule-show/ date)
  (let ((y (date-year date))
        (m (date-month date))
        (d (dateday date))
        (sch (find-kahua-instance <schedule> 'date (date->dbkey date))))
    (node-set/
      (p/ (format "~d年~d月~d日の予定" y m d)
          (a/cont/ (@@/ (cont edit y m d)) "[編集]"))
      (and sch (pre/ (ref sch 'memo))))))


(initialize-main-proc show)

