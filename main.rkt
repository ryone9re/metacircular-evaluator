#lang sicp

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and (and-predicates exp) env))
        ((or? exp) (eval-or (or-predicates exp) env))
        ((let? exp) (let->combination (let-bindings exp)
                                      (let-body exp)
                                      env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoeted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable? exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (sumbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (null? (cdddr exp))
      'false
      (cadddr exp)))

(define (make-if predicate consequent alternative)
  (cons 'if (cons predicate (cons consequent (cons alternative '())))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (fisrt-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (recipient clause) (caddr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (if (arrow-clause? first)
                (make-if (cond-predicate first)
                         (cons (recipient first) (cons (cond-predicate first) '()))
                         (expand-clauses rest)))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest))))))

(define (arrow-clause? clause)
  (if (null? clause)
      false
      (let ((rest (cdr clause)))
        (if (null? rest)
            false
            (arrow? (car rest))))))

(define (arrow? exp) (eq? exp '=>))
                  

(define (and? exp) (tagged-list? exp 'and))

(define (and-predicates exp) (cdr exp))

(define (eval-and exp env)
  (if (null? exp)
      'true
      (let ((first (car exp))
            (rest (cdr exp)))
        (if (first))
            'false
            (eval-and rest env))))

(define (or? exp) (tagged-list? exp 'or))

(define (or-predicates exp) (cdr exp))

(define (eval-or exp env)
  (if (null? exp)
      'false
      (let ((first (car exp))
            (rest (cdr exp)))
        (if (first))
            'true
            (eval-or rest env))))

(define (let? exp) (tagged-list? exp 'let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let->combination bindings body env)
  (let ((vars (map car bindings))
        (exps (map cadr bindings)))
    (cons (make-lambda vars body) exps)))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first (list-of-values (rest-operands exps) env)))))

;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (let ((last (list-of-values (rest-operands exps) env)))
;        (cons (eval (first-operand exps) env) last))))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (operands exp) (cdr exp))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
            (procedure-body procedure)
            (extend-environment
               (procedure-parameters procedure)
               arguments
               (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))
