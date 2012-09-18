;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  継続 (continuation)                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 再帰関数(例: 階乗)を継続渡しで書いてみる
;; まずふつうの再帰 -- usage: (fact 4)
(define (fact n)
  (if (= n 1)
    1
    (* n (fact (- n 1)))))

;; これを継続にすると, contを手続きとして次のように書ける。
;; usage: -- (fact/cps 4 (lambda (x) (<自分自身?> (* 4 x))))?
(define (fact/cps n cont)
  (if (= n 1)
    (cont 1)
    (fact/cps (- n 1) (lambda (x) (cont (* n x))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 『プログラミングGauche』 p.277-

;; lisのcarがpred?に相当する時seedをいじる。それ以外は普通の再帰
(define (find-fold pred? proc seed lis)
  (cond ((null? lis) seed)
        ((pred? (car lis))
         (let ((seed2 (proc (car lis) seed)))
           (find-fold pred? proc seed2 (cdr lis))))
        (else
          (find-fold pred? proc seed (cdr lis)))))

;; 引数として渡すprcはこんな感じ
(define (process elt seed)
  (print "found: " elt)
  (cons elt seed))

;; (find-fold odd? process '() '(1 2 3 4 5 6 7 8 9 10))
;; => found: 1
;;    found: 3
;;    found: 5
;;    found: 7
;;    found: 9


;; 継続タイプのfind-fold.
;; 先ほどのfind-fold内部, この部分は
;     (let ((seed2 (proc (car lis) seed)))
;       (find-fold pred? proc seed2 (cdr lis)))
;; 以下のlambdaと同義
;     ((lambda () (seed2)
;        (find-fold pred? proc seed2 (cdr lis)))
;      (proc (car lis) seed))

;; "継続渡し方式では、procの戻り値を使うのではなく、procの追加の引数として成果物を受け取る処理を渡してやります"

(define (find-fold2 pred? proc/cont seed lis)
  (cond ((null? lis) seed)
        ((pred? (car lis))
         (proc/cont (car lis)
                    seed
                    (lambda (seed2)
                      (find-fold2 pred? proc/cont seed2 (cdr lis)))))
        (else
          (find-fold2 pred? proc/cont seed (cdr lis)))))


;; proc/contに渡される継続手続きは
(define (process/cont elt seed cont)
  (print "found: " elt)
  (cont (cons elt seed)))

;; 同じ結果を得る。
;; (find-fold2 odd? process/cont '() '(1 2 3 4 5 6 7 8 9 10))

;; 継続の利点を示す。
(define next #f)
(define (break val) val)
(define (process/break elt seed cont)
  (set! next
    (lambda ()
      (print "found: " elt)
      (cont (cons elt seed))))
  (break #f))


(find-fold2 odd? process/break '() '(1 2 3 4 5 6 7 8 9 10))
; => #f
;
; gosh> (next)
; found: 1
; #f
; gosh> (next)
; found: 3
; #f
; gosh> (next)
; found: 5
; #f
; gosh> (next)
; found: 7
; #f
; gosh> (next)
; found: 9
; (9 7 5 3 1)
; gosh> (next)
; found: 9
; (9 7 5 3 1)

;; break機能を汎用的に使ってみる
(define (breaker proc)
  (lambda (elt seed cont)
    (set! next (lambda () (cont (proc elt seed))))
    (break #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 19.3 さらに継続を渡して
;;   lisの要素が亡くなった後、contに成果物を渡す
(define (find-fold/cont pred? proc/cont seed lis cont) ;; contがふえた
  (cond ((null? lis) (cont seed))
        ((pred? (car lis))
         (proc/cont (car lis)
                    seed
                    (lambda (seed2)
                      (find-fold/cont pred? proc/cont seed2 (cdr lis) cont))))))


;; CPS変換
;;   上記のfind-fold/contを"完全な"継続渡し方式にする。pred?やnull?もcall-return方式からCPS形式に書き換える。
(define (find-fold/cps pred?/cps proc/cps seed lis cont)
  (null?/cps lis
             (lambda (result)
               (if result
                 (cont seed)
                 (pred?/cps (car lis)
                            (if result
                              (proc/cps (car lis)
                                        seed ...)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 19.5 大域脱出
;;   大域脱出ってmemqとかで使われてる？

;; ref: [http://www.shido.info/lisp/scheme_cc.html] 4.1. call/ccを使った大域脱出




;; 内部でcall/ccを呼び出す"block"を定義する
(define-syntax block
  (syntax-rules ()
                ((_ escape body ...)
                 (call/cc (lambda (escape) body ...)))))

;; blockの後に「内部で出てきたらここまで戻るキーワード」を置き、処理のbodyを続ける

(block escape-top
       (block escape-1st
              (block escape-2nd
                     (escape-1st 1)
                     (print 'NG!!!))
              (print 'NGGGG!!))
       (print 'OK))

;; => OK
;;    #<undef>

(block escape-top
       (block escape-1st
              (block escape-2nd
                     (escape-top 1)
                     (print 'NG!!!))
              (print 'NGGGG!!))
       (print 'OK))
;; => 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 19.6 break/next 名前付き for-each
;;   Schemeのfor-eachにもbreakやnext(continue)が欲しいよ。
(define-syntax for-each-ext
  (syntax-rules ()
                ((_ break next lambda-expr arg-list ...)
                 (let ((arg1 (list arg-list ...)))
                   (call/cc (lambda (break)
                              (apply for-each
                                     (lambda arg
                                       (call/cc (lambda (next)
                                                  (apply lambda-expr arg))))
                                     arg1)))))))

;; how to use
(for-each-ext break1 next1
              (lambda (x)
                (for-each-ext break2 next2
                              (lambda (y)
                                (format #t "~2d " (* x y)))
                              '(1 2 3 4 5 6 7 8 9))
                (newline))
              '(1 2 3 4 5 6 7 8 9))

; =>  1  2  3  4  5  6  7  8  9
;     2  4  6  8 10 12 14 16 18
;     3  6  9 12 15 18 21 24 27
;     4  8 12 16 20 24 28 32 36
;     5 10 15 20 25 30 35 40 45
;     6 12 18 24 30 36 42 48 54
;     7 14 21 28 35 42 49 56 63
;     8 16 24 32 40 48 56 64 72
;     9 18 27 36 45 54 63 72 81
;    #<undef>


(for-each-ext break1 next1
              (lambda (x)
                (for-each-ext break2 next2
                              (lambda(y)
                                (cond ((not (number? x)) (next1 x))
                                      ((not (number? y)) (next2 y))
                                      ((< x y) (break2 x))
                                      ((>= x 100) (break1 'done))
                                      (else (format #t "~2d " (* x y)))))
                              '(1 2 3 a 4 5 6 b 7 8 #\a 9 '() 10))
                (newline))
              '(#\x 1 2 3 x 4 5 6 y 7 8 9 10 x 100))

; =>  1
;     2  4
;     3  6  9
;     4  8 12 16
;     5 10 15 20 25
;     6 12 18 24 30 36
;     7 14 21 28 35 42 49
;     8 16 24 32 40 48 56 64
;     9 18 27 36 45 54 63 72 81
;    10 20 30 40 50 60 70 80 90 100
;    done


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 19.7 簡易な例外機構

;; マクロについて復習
;; (define-syntax <name>
;;   (syntax-rules (<literal> ...)
;;     (pattern1 template1)
;;     (pattern2 template2) ...))
;;
;; (syntax-rules (here) <= に入るのは literal. 特殊なシンボルにマッチさせたい場合。

(define *signals* '())

;; finally付きパターンにマッチさせ、なければ後者.
(define-syntax catch
  (syntax-rules (finally)
                ((_ (sig body ...) (finally follow ...))
                 (let* ((signals-backup *signals*)
                        (val (call/cc (lambda (k)
                                        (set! *signals* (cons (cons 'sig k) *signals*))
                                        body ...))))
                   (set! *signals* signals-backup)
                   follow ...
                   val))
                ((_ (sig body ...))
                 (catch (sig body ...) (finally)))))

(define-syntax throw
  (syntax-rules ()
                (())))

;; usage
(define (div n d)
  (if (= d 0)
    (throw)))



