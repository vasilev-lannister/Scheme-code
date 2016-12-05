(define (take n lst)       ;ВРЪЩА ПЪРВИТЕ N
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) 
            (take (- n 1) 
                  (cdr lst))
      )
  )
)

(define (drop n lst)        ;ВРЪЩА ПОСЛЕДНИТЕ N
  (cond ((and (null? lst) (> n 0)) '())
        ((= n 0) lst)
        (else (drop (- n 1) (cdr lst))))
)

(define (merge a b)   ;СЛИВА 2 СОРТИРАНИ СПИСЪКА В 1 СОРТИРАН СПИСЪК
  (cond ((null? a) b)
        ((null? b) a)
        ((> (car a) (car b)) (cons (car b)
                                   (merge a (cdr b))))
        (else (cons (car a)
                    (merge (cdr a) b)))
  )
)

(define (merge-sort lst)  ; СОРТИРА ДАДЕН СПИСЪК ЧРЕЗ СЛИВАНЕ 
    (if (or (null? lst) (null? (cdr lst)))
        lst
        (merge (merge-sort (take (div (length lst) 2) lst))
               (merge-sort (drop (div (length lst) 2) lst))
        )
    )
)

(define (at n lst)  ; ДАВА N-ТИЯ ЕЛЕМЕНТ НА СПИСЪК
  (if (= n 0) (car lst) (at (- n 1) (cdr lst)))
)

(define (sort-two lst) ; СОРТИРА ТОЧНО 2 ЕЛЕМЕНТА
    (if (< (car lst) (cadr lst))
        (list (cadr lst) (car lst))
        lst
    )
)
(define (quick-sort lst)               ; ВСИЧКИ ЕЛЕМЕНТИ ПО-МАЛКИ ОТ ИЗБРАНИЯ СЕ ОТДЕЛЯТ В 1 СПИСЪК
    (cond ((or (null? lst) (null? (cdr lst))) lst)
          ((= 2 (length lst)) (sort-two lst)) ; АНАЛОГИЧНО ЗА ВСИЧКИ ПО-ГОЛЕМИ И СЕ СОРТИРАТ 2ТА СП
          (else (append (quick-sort (filter (lambda (x) ;НАКРАЯ СЛИВАНЕ НА СП1 ЕЛЕМЕНТА И СП2
                                                (> x (at (div (length lst) 2)
                                                          lst)))
                                             lst))
                        (list (at (div (length lst) 2) lst))
                        (quick-sort (filter (lambda (x) 
                                                (< x (at (div (length lst) 2)
                                                          lst)))
                                             lst)))
          )
    )
)

(define (check-sublist lst sub) ; ДАЛИ ОТ 1 СПИСЪК ЧРЕЗ ПРЕМАХВАНЕ- ДР СПИСЪК
    (cond ((null? sub) #t)
          ((null? lst) #f)
          ((= (car lst) (car sub)) (check-sublist (cdr lst) (cdr sub)))
          (else (check-sublist (cdr lst) sub))
    )
)

(define (check-sublist-adj-Helper lst sub subfull) 
    (cond ((null? sub) #t)
          ((null? lst) #f)
          ((= (car lst) (car sub)) (check-sublist-adj-Helper (cdr lst) (cdr sub) subfull))
          (else (check-sublist-adj-Helper (cdr lst) subfull subfull))
    )
)
                  ; ДАЛИ ДАДЕН СПИСЪК СЪДЪРЖА ДРУГ СПИСЪК
(define (check-sublist-adj lst sub) (check-sublist-adj-Helper lst sub sub))


(define (get-left root)
    (car (cdr root))
)

(define (get-right root)
    (car (cdr (cdr root)))
)

(define (tree-map f tree)
    (if (null? tree) ;ПРИЛАГА ДАДЕНА Ф-ЦИЯ НА ВС ЕЛ НА ДЪРВОТО
        '()
        (list   (f (car tree))
                (tree-map f (get-left tree))
                (tree-map f (get-right tree))
        )
    )
)

(define (tree-filter pred tree) ;ВРЪЩА ТЕЗИ ЕЛ НА ДЪРВОТО ЗА КОИТО ДАДЕНА
    (cond ((null? tree) '())    ; ФУНКЦИЯ ВРЪЩА TRUE
          ((pred (car tree)) (append (list (car tree))
                                     (tree-filter pred (get-left tree))
                                     (tree-filter pred (get-right tree))))
          (else (append (tree-filter pred (get-left tree))
                        (tree-filter pred (get-right tree)))
          )
    )
)

(define (check-same treeA treeB) ; ПРОВЕРЯВА ДАЛИ 2 ДЪРВЕТА СА ЕКВИВАЛЕНТНИ
    (if (and (null? treeA) (null? treeB))
        #t
        (and (= (car treeA) (car treeB))
             (check-same (get-left treeA) (get-left treeB))
             (check-same (get-right treeA) (get-right treeB)))
    )
)
