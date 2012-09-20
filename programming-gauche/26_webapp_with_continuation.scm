#!/usr/bin/env gosh

(use gauche.net)
(use util.match)
(use rfc.822)
(use rfc.uri)
(use text.tree)
(use text.html-lite)
(use www.cgi)

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

(define (main args)
  (run-server) 0)
