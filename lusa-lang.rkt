#lang racket

(require syntax/strip-context)
(require syntax/readerr)

(provide (rename-out [lusa-read read]
                     [lusa-read-syntax read-syntax]))

(define (lusa-read in)
  (parameterize ([current-readtable (make-lusa-readtable)])
    (read in)))

(define (lusa-read-syntax src in)
  (parameterize ([current-readtable (make-lusa-readtable)])
    (read-syntax src in)))

(define lusa-parser
  (case-lambda
    [(ch in)
     (check-lusa-after (read-lusa in) in (object-name in))]
    [(ch in src line col pos)
     (check-lusa-after (read-lusa-syntax src in) in src)]))

(define (check-lusa-after val in src)
  (regexp-match #px"^\\s*" in)
  (match (regexp-try-match #px"^~" in)
    [(list _) val]
    [else (bad-ending (peek-char in) src in)]))

(define (bad-ending ch src in)
  (let-values ([(line col pos) (port-next-location in)])
    ((if (eof-object? ch)
         raise-read-error
         raise-read-eof-error)
     "expected a closing `~'"
     src line col pos
     (if (eof-object? ch) 0 1))))

(define (make-lusa-readtable)
  (make-readtable (current-readtable)
                  #\~ 'terminating-macro lusa-parser))

(define (read-lusa in)
  (read-lusa-syntax #f in))

;; read expression list
(define (read-lusa-expr-list pre src in)
  (regexp-match #px"^\\s*" in)
  (match (regexp-try-match #px"^\\s*\\}" in)
    [(list _) pre]
    [else (with-syntax ([p pre]
                        [c (read-syntax src in)])
            (read-lusa-expr-list #'(begin p c) src in))]))

;; read parameter list for functio call
(define (read-lusa-params-list pre src in)
  (regexp-match #px"^\\s*" in)
  (match (or (regexp-try-match #px"^\\s*\\)\\s*(\\{)\\s*\\|([^|]*)\\|" in)
             (regexp-try-match #px"^\\s*\\)" in))
    [(list _) pre]
    [(list _ #"{" params) (with-syntax ([p pre]
                                        [c (read-lusa-closure params src in)])
                            #'(cons c p))]
    [else (with-syntax ([p pre]
                        [c (read-syntax src in)])
            (read-lusa-params-list #'(cons c p) src in))]))

;; read closure
(define (read-lusa-closure params src in)
  (with-syntax ([ps (map (lambda (i)
                           (string->symbol (bytes->string/locale i)))
                         (remove #"" (regexp-split #px"\\s+" params)))]
                [b (read-lusa-expr-list #'(void) src in)])
    #'(lambda ps b)))

;; read reference
(define (read-lusa-ref id src in)
  (let ([ids (remove "" (regexp-split #px"\\." (bytes->string/locale id)))])
    (with-syntax ([o (match (length ids)
                       [0 #'null]
                       [1 (string->symbol (car ids))]
                       [else (foldl
                              (lambda (p h)
                                (match (or (regexp-match #px"^\\[([\\d\\\"][^\\]]*)\\]" p)
                                           (regexp-match #px"^(\\[)([^\\]]+)\\]" p))
                                  [(list _ pp) (with-syntax ([ho h]
                                                             [po (string->symbol pp)])
                                                 #'(hash-ref ho 'po))]
                                  [(list _ "[" pp) (with-syntax ([ho h]
                                                                  [po (string->symbol pp)])
                                                      #'(hash-ref ho po))]
                                  [else (with-syntax ([ho h]
                                                      [po (string->symbol p)])
                                          #'(hash-ref ho 'po))]))
                              (with-syntax ([io (string->symbol (car ids))])
                                #'io)
                              (cdr ids))])])
      #'o)))

;; read define

;; get parent
(define (get-parent id)
  (let ([ids (remove "" (regexp-split #px"\\." (bytes->string/locale id)))])
    (foldl
     (lambda (p h)
       (with-syntax ([ho h]
                     [po (string->symbol p)])
         #'(hash-ref ho 'po)))
     (with-syntax ([io (string->symbol (car ids))])
       #'io)
     (cdr (reverse (cdr (reverse ids)))))))

;; get property
(define (get-property id)
  (let ([ids (remove "" (regexp-split #px"\\." (bytes->string/locale id)))])
    (string->symbol (car (reverse ids)))))

;; for normal define
(define (read-lusa-def id src in)
  (let ([ids (remove "" (regexp-split #px"\\." (bytes->string/locale id)))])
    (match (length ids)
      [0 #'(void)]
      [1 (with-syntax ([o (string->symbol (car ids))]
                       [v (read-syntax src in)])
           #'(define o v))]
      [else (with-syntax ([o (get-parent id)]
                          [k (get-property id)]
                          [v (read-syntax src in)])
              #'(hash-set! o 'k v))])))

;; define hash table
(define (read-lusa-hash-def id src in)
  (let ([ids (remove "" (regexp-split #px"\\." (bytes->string/locale id)))])
    (match (length ids)
      [0 #'(void)]
      [1 (with-syntax ([o (string->symbol (car ids))]
                      [v (read-lusa-expr-list #'(void) src in)])
          #'(define o (let ([self (make-hash)])
                        v
                        self)))]
      [else (with-syntax ([o (get-parent id)]
                          [k (get-property id)]
                          [v (read-lusa-expr-list #'(void) src in)])
              #'(hash-set! o 'k (let ([self (make-hash)])
                                  v
                                  self)))])))

;; set one property of a hash table with a closure
;; a parameter 'self' will be inserted automatically
(define (read-lusa-hash-closure-def id params src in)
  (let ([ids (remove "" (regexp-split #px"\\." (bytes->string/locale id)))])
    (match (length ids)
      [0 #'(void)]
      [1 (with-syntax ([o (string->symbol (car ids))]
                       [v (read-lusa-closure (bytes-append #"self " params) src in)])
           #'(define o v))]
      [else (with-syntax ([o (get-parent id)]
                          [k (get-property id)]
                          [v (read-lusa-closure (bytes-append #"self " params) src in)])
              #'(hash-set! o 'k v))])))

;; read function call
;; parent object will be passed to self parameter
(define (read-lusa-call id src in)
  (define parent #"")
  (match (regexp-match #px"^(.*)\\.[\\w\\_\\d\\[\\]\\\"]+$" id)
    [(list _ p) (set! parent p)]
    [else (set! parent #"")])
  (with-syntax ([f (read-lusa-ref id src in)]
                [self (read-lusa-ref parent src in)]
                [vs (read-lusa-params-list #'`() src in)])
    #'(apply f (cons self (reverse vs)))))

;; syntax dispatcher
(define (read-lusa-syntax src in)
  (regexp-match #px"^\\s*" in)
  (match (or (regexp-try-match #px"^(\\{)\\s*\\|\\s*([^|]*)\\s*\\|" in)
             (regexp-try-match #px"^(\\{)" in)
             (regexp-try-match #px"^([\\w\\_\\d\\.\\[\\]\\\"]+)\\s*(=)\\s*(\\{)\\s*\\|\\s*([^|]*)\\s*\\|" in)
             (regexp-try-match #px"^([\\w\\_\\d\\.\\[\\]\\\"]+)\\s*(=)\\s*(\\{)" in)
             (regexp-try-match #px"^([\\w\\_\\d\\.\\[\\]\\\"]+)\\s*(=)" in)
             (regexp-try-match #px"^([\\w\\_\\d\\.\\[\\]\\\"]+)\\s*(\\()" in)
             (regexp-try-match #px"^([\\w\\_\\d\\.\\[\\]\\\"]+)" in))
    [(list _ #"{" params) (read-lusa-closure params src in)]
    [(list _ #"{") (read-lusa-expr-list #'(void) src in)]
    [(list _ id) (read-lusa-ref id src in)]
    [(list _ id #"=") (read-lusa-def id src in)]
    [(list _ id #"(") (read-lusa-call id src in)]
    [(list _ id #"=" #"{") (read-lusa-hash-def id src in)]
    [(list _ id #"=" #"{" params) (read-lusa-hash-closure-def id params src in)]
    [else (read-syntax src in)]))
