;; 26.4 継続渡しスタイルによる状態管理
;;  "本来は処理が続いているけど、clientからの入力を待つ時のみ処理が中断している"と、サーバ側の視点で考えてみる。

(use gauche.net)
(use util.match)
(use rfc.822)
(use rfc.uri)
(use text.tree)
(use text.html-lite)
(use www.cgi)
(use util.list)
(use srfi-27)

(random-source-randomize! default-random-source)

;; run-server, get-request, handle-request は簡易httpdの定義と同じなので省略
;; また、mapの定義 *dungeon* とget-directionも同じなので省略
(load "./26_webapp_with_sessions")

(define *conts*  (make-hash-table 'eqv?))
(define *max-id*  (expt 2 64))
(define (push-cont! cont)
  (let1 cid (random-integer *max-id*)
        (cond ((hash-table-get *conts* cid #f) (push-cont! cont))
              (else (hash-table-put! *conts* cid cont) cid))))

(define (get-cont params)
  (hash-table-get *conts*
                  (cgi-get-parameter "c" params :convert string->number)
                  #f))

;; content
(define (render-content path params)
  (cond ((get-cont params) => (cut <> params))
        (else (run-application params))))

(define (run-application params)
  (let loop ((location (list-ref *dungeon* 0))
             (history '()))
    (define (render-selector selector)
      (let1 cid (push-cont! (lambda (params)
                              (loop (list-ref *dungeon* (cdr selector))
                                    (cons location history))))
            (html:li (html:a :href #`"?c=,cid"
                             (get-direction (car selector)) "へ進む"))))
    (tree->string
      (html:html
        (html:head (html:title "simple httpd"))
        (html:body (html:p (html-escape-string (car location)))
                   (html:ul (map render-selector (cdr location)))
                   (html:hr)
                   (map (lambda (p) (html:p (html-escape-string (car p))))
                        history))))))


(run-server)

