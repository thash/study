#!/usr/bin/env gosh

(use gauche.net)
(use util.match)
(use rfc.822)
(use rfc.uri)
(use text.tree)
(use text.html-lite)
(use www.cgi)
(use util.list)
(use srfi-27)

;; initialize rand seed
(random-source-randomize! default-random-source)

;; gauche.netのネットワーク機能を使ってserverのmail loopを記述
;;   internal procs: get-request, handle-request
(define (run-server)
  (let1 server-sock (make-server-socket 'inet 8080 :reuse-addr? #t)
        (guard (e (else (socket-close server-sock) (raise e)))
               (let loop ((client (socket-accept server-sock)))
                 (guard (e (else (socket-close client) (raise e)))
                        (handle-request (get-request (socket-input-port client))
                                        (socket-output-port client))
                        (socket-close client))
                 (loop (socket-accept server-sock))))))

;; (let1 <var> <expr> <body>)
;;   ==> syntax sugar of  (let ((<var> <expr>)) <body>)

(define (get-request iport)
  (rxmatch-case (read-line iport)
                (test eof-object? 'bad-request)
                (#/^(GET|HEAD)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
                 (list* meth abs-path (rfc822-read-headers iport)))
                (#/^[A-Z]+/ () 'not-implemented)
                (else 'bad-request)))

;; rxmatch-case ... case式的な構文で正規表現のマッチを行うマクロ。

;; handle-request
;;   internal procs: render-content
(define (handle-request request oport)
  (match request
         ('bad-request     (display "HTTP/1.1 400 Bad Request\r\n\r\n" oport))
         ('not-implemented (display "HTTP/1.1 501 Not Implemented\r\n\r\n" oport))
         ((meth abs-path . headers)
          (receive (auth path q frag) (uri-decompose-hierarchical abs-path)
                   (let1 content
                         (render-content path (cgi-parse-parameters :query-string (or q "")))
                         (display "HTTP/1.1 200 OK\r\n" oport)
                         (display "Content-Type: text/html; charset=utf-8\r\n" oport)
                         (display #`"Content-Length: ,(string-size content)\r\n" oport)
                         (display "\r\n" oport)
                         (when (equal? meth "GET") (display content oport)))))))

(define (render-content path params)
  (tree->string
    (html:html
      (html:head (html:title "simple httpd"))
      (html:body (html:h1 "Welcome to simple httpd")
                 (html:p "Path : " (html-escape-string path))
                 (map (lambda (p) (html:p (html-escape-string (car p)) " : "
                                          (html-escape-string (cdr p))))
                      params)))))

;; (define (main args)
;;   (run-server) 0)

;; map data from sec9
(define *dungeon*
  '(["あなたは森の北端にいる。道は南に続いている。"
     (s . 1)]
    ["あなた鬱蒼とした森の中にいる。道は南北に伸びている。東に降りて行く小径がある。"
     (n . 0)
     (s . 2)
     (e . 3)]
    ["足下がぬかるんでいる。道は直角に折れ、北と西に延びている。西に続く道の先が明るくなっている。"
     (n . 1)
     (w . 4)]
    ["あなたは沼のほとりにいる。空気の動きが止まり、暑さを感じる。西に昇ってゆく小径がある。"
     (w . 1)]
    ["突然目の前が開けた。あなたは森の中の広場にいる。丈の短い、やわらかそうな草が一面に広場を覆っている。道が東に延びている。"
     (e . 2)]))

;; 'と"を区別すること. Rubyの文字列リテラルではない.
(define (get-direction dir)
  (assoc-ref '((n . "北") (e . "東") (w . "西") (s . "南")) dir))


;; sec17, "総称関数とオブジェクト"で利用したdefine-classを使いセッションオブジェクトを生成
;; classにて定義される":hoge" はGaucheのオブジェクトシステムでは"スロット"と呼ばれる.
(define-class <session> ()
             ((sid      :init-keyword :sid)
              (location :init-value (list-ref *dungeon* 0))
              (history  :init-value '())))

(define *sessions* (make-hash-table 'eqv?))

;; session-idをclientと共有するには
;;   1. URL中のpathかqueryに入れる
;;   2. cookie利用
;;   3. 1,2併用

;; orをRubyのnilガード的に使い, hash-tableにsessionがなかったときmake-sessionする
(define (get-session params)
  (or (hash-table-get *sessions*
                      (cgi-get-parameter "s" params :convert string->number)
                      #f)
      (make-session)))

(define *max-id* (expt 2 64))

(define (make-session)
  ;; random-integerは0から与えられたmax-1の間のランダムな整数を返す
  (let1 sid (random-integer *max-id*)
        (cond ((hash-table-get *sessions* sid #f) (make-session))
              (else (let1 sess (make <session> :sid sid)
                          (hash-table-put! *sessions* sid sess)
                          sess)))))

(define (render-content path params)
  (let ((session (get-session params))
        (dir (cgi-get-parameter "d" params :convert string->symbol)))
    (and-let* ((index (assoc-ref (cdr (ref session 'location)) dir)))
              (push! (ref session 'history) (ref session 'location))
              (set! (ref session 'location) (list-ref *dungeon* index)))
    (let1 location (ref session 'location)
          (define (render-selector selector)
            (html:li (html:a :href #`"?s=,(ref session 'sid)&d=,(car selector)"
                             (get-direction (car selector)) "へ進む")))
          (tree->string
            (html:html
              (html:head (html:title "simple httpd"))
              (html:body (html:p (html-escape-string (car location)))
                         (html:ul (map render-selector (cdr location)))
                         (html:hr)
                         (map (lambda (p) (html:p (html-escape-string (car p))))
                              (ref session 'history))))))))



;; なかなかいい感じ.
;; (run-server)


;; 26.3.2 Backボタン問題
;;   "現在の状態"をuniqueに固定するtoken slotを追加。

(define-class <session> ()
             ((sid      :init-keyword :sid)
              (token    :init-value (random-integer *max-id*))
              (location :init-value (list-ref *dungeon* 0))
              (history  :init-value '())))

;; "t" paramでtokenを送信. sessionが存在しtokenも一致した場合, 新たなtokenを割り当てる
(define (get-session params)
  (or (and-let* ((s (hash-table-get
                      *sessions*
                      (cgi-parse-parameters "s" params :convert string->number)
                      #f))
                 ((env? (ref s 'token)
                  (cgi-parse-parameters "t" params :convert string->number))))
      (set! (ref s 'token) (random-integer *max-id*))
      s)
  (make-session)))

(define (render-selector selector)
  (html:li
    (html:a
      :href #`"?s=,(ref session 'sid)&t=,(ref session 'token)&d=,(car selector)"
      (get-direction (car selector)) "へ進む")))


;; session + tokenで現在の状態を保持することができるようになり、back buttonにも対応。
;; だが、この方針だと複数タブ開いて注文追加しつつbrowsing, という用途では不具合が発生する可能性もある.

