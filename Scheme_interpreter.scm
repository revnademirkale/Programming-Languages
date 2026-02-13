;;; -------------------------------------------------------------------
;;; File: revna.demirkale-hw5.scm
;;; CS305 HW5: s7 Subset Interpreter
;;; -------------------------------------------------------------------

;;; -------------------------------------------------------------------
;;; 1. Global Environment Management
;;; -------------------------------------------------------------------

;; We store the global environment as a list of pairs (alist).
(define *global-env* '())

;; Lookup a variable in an environment
;; env: can be a list of frames (for local scope) or just the global alist
(define (lookup var env)
  (cond
    ((null? env) 'err-var-not-found)
    ((list? (car env)) ;; It's a local frame (let/let*)
     (let ((binding (assoc var (car env))))
       (if binding
           (cdr binding)
           (lookup var (cdr env))))) ;; Look in parent scope
    (else ;; We hit the global environment list structure
     (let ((binding (assoc var env)))
       (if binding
           (cdr binding)
           'err-var-not-found)))))

;; Add a definition to the global environment
(define (add-global-def var val)
  (let ((binding (assoc var *global-env*)))
    (if binding
        (set-cdr! binding val)
        (set! *global-env* (cons (cons var val) *global-env*))))
  var) 

;;; -------------------------------------------------------------------
;;; 2. Error Handling
;;; -------------------------------------------------------------------

(define (s7-error)
  (display "cs305: ERROR")
  (newline)
  (cs305)) ;; Restart the REPL loop

;;; -------------------------------------------------------------------
;;; 3. Evaluation Helpers
;;; -------------------------------------------------------------------

(define (check-duplicates bindings)
  (let ((vars (map car bindings)))
    (define (has-dup? lst)
      (cond ((null? lst) #f)
            ((memq (car lst) (cdr lst)) #t)
            (else (has-dup? (cdr lst)))))
    (has-dup? vars)))

(define (is-operator? x)
  (member x '(+ - * /)))

(define (apply-op op operands)
  (cond
    ((eq? op '+) (apply + operands))
    ((eq? op '-) (apply - operands))
    ((eq? op '*) (apply * operands))
    ((eq? op '/) (apply / operands))
    (else (s7-error))))

;; Helper for 'every' (standard in MIT Scheme, added here for compatibility)
(define (every pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst)) (every pred (cdr lst)))
        (else #f)))

;;; -------------------------------------------------------------------
;;; 4. The Core Evaluator
;;; -------------------------------------------------------------------

(define (s7-interpret expr env)
  (cond
    ;; Case 1: Number
    ((number? expr) expr)

    ;; Case 2: Variable (Identifier)
    ((symbol? expr)
     (let ((val (lookup expr env)))
       (if (eq? val 'err-var-not-found)
           (if (assoc expr *global-env*) 
               (cdr (assoc expr *global-env*))
               (s7-error))
           val)))

    ;; Case 3: Compound Expression (List)
    ((list? expr)
     (let ((head (car expr)))
       (cond
         ;; --- DEFINE ---
         ((eq? head 'define)
          (if (not (= (length expr) 3)) (s7-error)
              (let ((var (cadr expr))
                    (val-expr (caddr expr)))
                (if (not (symbol? var)) 
                    (s7-error)
                    (add-global-def var (s7-interpret val-expr env))))))

         ;; --- LET ---
         ((eq? head 'let)
          (if (< (length expr) 3) (s7-error)
              (let ((bindings (cadr expr))
                    (body (caddr expr)))
                (if (not (list? bindings)) (s7-error)
                    (if (check-duplicates bindings)
                        (s7-error) 
                        (let ((evaluated-vals 
                               (map (lambda (b) 
                                      (if (not (= (length b) 2)) (s7-error)
                                          (s7-interpret (cadr b) env))) 
                                    bindings))
                              (vars (map car bindings)))
                          (s7-interpret body (cons (map cons vars evaluated-vals) env))))))))

         ;; --- LET* ---
         ((eq? head 'let*)
          (if (< (length expr) 3) (s7-error)
              (let ((bindings (cadr expr))
                    (body (caddr expr)))
                (if (not (list? bindings)) (s7-error)
                    (letrec ((process-let* (lambda (rem-bindings current-env)
                                (if (null? rem-bindings)
                                    (s7-interpret body current-env)
                                    (let* ((binding (car rem-bindings))
                                           (var (car binding))
                                           (val-expr (cadr binding)))
                                      (if (not (symbol? var)) (s7-error)
                                          (let ((val (s7-interpret val-expr current-env)))
                                            (process-let* (cdr rem-bindings) 
                                                          (cons (list (cons var val)) current-env)))))))))
                      (process-let* bindings env))))))

         ;; --- CASE ---
         ((eq? head 'case)
          (if (< (length expr) 3) (s7-error)
              (let ((target-val (s7-interpret (cadr expr) env))
                    (clauses (caddr expr)))
                (define (process-clauses lst)
                  (cond
                    ((null? lst) (s7-error))
                    (else
                     (let ((clause (car lst)))
                       (cond
                         ((not (list? clause)) (s7-error))
                         ((eq? (car clause) 'else)
                          (s7-interpret (cadr clause) env))
                         (else
                          (let ((key-val (s7-interpret (car clause) env)))
                            (if (equal? key-val target-val)
                                (s7-interpret (cadr clause) env)
                                (process-clauses (cdr lst))))))))))
                (process-clauses clauses))))

         ;; --- ARITHMETIC OPERATORS ---
         ((is-operator? head)
          (let ((operands (cdr expr)))
             (if (< (length operands) 2)
                 (s7-error)
                 (let ((evaluated-operands (map (lambda (e) (s7-interpret e env)) operands)))
                   (if (not (and (list? evaluated-operands) 
                                 (every number? evaluated-operands)))
                       (s7-error)
                       (apply-op head evaluated-operands))))))

         (else (s7-error)))))

    (else (s7-error))))

;;; -------------------------------------------------------------------
;;; 5. The REPL (Read-Eval-Print Loop)
;;; -------------------------------------------------------------------

(define cs305
  (lambda ()
    (display "cs305> ")
    (let ((input (read)))
      (if (eof-object? input)
          (display "Exiting.")
          (begin
            (let ((val (s7-interpret input '())))
               (display "cs305: ")
               (display val)
               (newline)
               (cs305)))))))