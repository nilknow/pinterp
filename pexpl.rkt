#lang racket
(define env0 '())

(define ext-env
  (lambda (k v env)
    (cons `(,k . ,v) env)))

(define lookup
  (lambda (k env)
    (let ([p (assq k env)])
      (cond
        [(not p) #f]
        [else (cdr p)]))))

(struct Closure (f env))

(define pinterp
  (lambda (exp env)
    (match exp
      [(? number? x) x]
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
           [(not v)
            (error "undefined variable" x)]
           [else v]))]
      [`(lambda (,x) ,e)
       (Closure exp env)] ;;exp meams `(lambda (,x) ,e)
      [`(let ([,x ,e1]) ,e2)
       (let ([v1 (pinterp e1 env)])
         (pinterp e2 (ext-env x v1 env)))]
      [`(,op ,e1 ,e2)
       (let ([v1 (pinterp e1 env)]
             [v2 (pinterp e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))]
      [`(,e1 ,e2)
       (let ([v1 (pinterp e1 env)]
             [v2 (pinterp e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env-save)
            (pinterp e (ext-env x v2 env-save))]))])))

(define pc
  (lambda (exp)
    (pinterp exp env0)))
