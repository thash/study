;;; 『プログラミングGauche』 p.277
;;; 継続 (continuation)

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



