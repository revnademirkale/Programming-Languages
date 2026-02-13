;; ============================================================
;; CS Semantic Checks (Prefix Notation) - Scheme
;; Implements:
;;  - find-redefined-functions
;;  - find-undefined-parameters
;;  - find-arity-contradictions
;;  - find-missing-function-names
;;  - find-undefined-functions
;; ============================================================

;; ---------- Utilities ----------

(define (builtin-operator? s)
  (and (symbol? s) (memq s '(+ - * / ^))))

(define (reserved-word? s)
  (and (symbol? s) (memq s '(calculate =))))

(define (special-symbol? s)
  (or (builtin-operator? s) (reserved-word? s)))

(define (symbol-member? x xs)
  (cond ((null? xs) #f)
        ((eq? x (car xs)) #t)
        (else (symbol-member? x (cdr xs)))))

(define (assoc-get key alist)
  (cond ((null? alist) #f)
        ((eq? key (caar alist)) (cdar alist))
        (else (assoc-get key (cdr alist)))))

(define (alist-put-first key val alist)
  ;; put only if key not present (keep first definition)
  (if (assoc-get key alist)
      alist
      (cons (cons key val) alist)))

(define (safe-length lst)
  ;; assumes proper list in assignments; still keep separate helper
  (length lst))

;; Program shape:
;; program = ( (definitions...) (calculations...) )
(define (program-definitions prog) (car prog))
(define (program-calculations prog) (cadr prog))

;; Definition shape:
;; (f (x y) (= <expr>))
(define (def-name d) (car d))
(define (def-params d) (cadr d))
(define (def-eq-part d) (caddr d))

(define (def-body d)
  (let ((eqp (def-eq-part d)))
    ;; eqp expected: (= <expr>)
    (if (and (pair? eqp) (symbol? (car eqp)) (eq? (car eqp) '=) (pair? (cdr eqp)))
        (cadr eqp)
        ;; fallback: if unexpected, try to treat whole eqp as body
        eqp)))

;; Calculation shape:
;; (calculate <expr>)
(define (calc-expr c)
  (if (and (pair? c) (symbol? (car c)) (eq? (car c) 'calculate) (pair? (cdr c)))
      (cadr c)
      ;; fallback
      c))

;; ---------- Rule 1: Redefined Functions ----------
(define (find-redefined-functions prog)
  (let ((defs (program-definitions prog)))
    (let loop ((ds defs) (seen '()) (dups '()))
      (if (null? ds)
          (reverse dups)
          (let* ((d (car ds))
                 (fname (def-name d)))
            (if (symbol-member? fname seen)
                (loop (cdr ds) seen (cons fname dups))
                (loop (cdr ds) (cons fname seen) dups)))))))

;; ---------- Rule 2: Undefined Parameters ----------
;; Collect variables used as bare symbols in RHS that are NOT in parameter list.
;; IMPORTANT: Do not count function names/operators in function position.
(define (collect-undefined-vars expr params)
  (cond
    ((null? expr) '())
    ((number? expr) '())
    ((symbol? expr)
     (if (or (special-symbol? expr) (symbol-member? expr params))
         '()
         (list expr)))
    ((pair? expr)
     (let ((head (car expr))
           (tail (cdr expr)))
       (cond
         ;; If head is a symbol, it's operator/function position: don't treat head as a variable
         ((symbol? head)
          (let loopargs ((xs tail) (acc '()))
            (if (null? xs)
                (reverse acc)
                (loopargs (cdr xs)
                          (append (reverse (collect-undefined-vars (car xs) params)) acc)))))
         ;; If head is a list, traverse everything
         (else
          (let loopall ((xs expr) (acc '()))
            (if (null? xs)
                (reverse acc)
                (loopall (cdr xs)
                         (append (reverse (collect-undefined-vars (car xs) params)) acc))))))))
    (else '())))

(define (find-undefined-parameters prog)
  (let ((defs (program-definitions prog)))
    (let loop ((ds defs) (acc '()))
      (if (null? ds)
          (reverse acc)
          (let* ((d (car ds))
                 (params (def-params d))
                 (body (def-body d))
                 (bad (collect-undefined-vars body params)))
            (loop (cdr ds) (append (reverse bad) acc)))))))

;; ---------- Collect function calls in expressions ----------
;; Returns list of records: ( (fname argc) ... ) in preorder appearance.
;; Only records when expr is (fname arg1 arg2 ...) where fname is symbol and not builtin/reserved.
(define (collect-calls expr)
  (cond
    ((null? expr) '())
    ((number? expr) '())
    ((symbol? expr) '())
    ((pair? expr)
     (let ((head (car expr))
           (tail (cdr expr)))
       (cond
         ((symbol? head)
          (let ((here (if (or (special-symbol? head))
                          '()
                          (list (list head (safe-length tail))))))
            (append
             here
             (let loop ((xs tail) (acc '()))
               (if (null? xs)
                   (reverse acc)
                   (loop (cdr xs) (append (reverse (collect-calls (car xs))) acc)))))))
         (else
          (let loop ((xs expr) (acc '()))
            (if (null? xs)
                (reverse acc)
                (loop (cdr xs) (append (reverse (collect-calls (car xs))) acc))))))))
    (else '())))

(define (build-arity-table defs)
  ;; alist: ( (fname . paramcount) ... ) keeping first definition
  (let loop ((ds defs) (tab '()))
    (if (null? ds)
        tab
        (let* ((d (car ds))
               (fname (def-name d))
               (pc (safe-length (def-params d))))
          (loop (cdr ds) (alist-put-first fname pc tab))))))

;; ---------- Rule 3: Arity Contradiction ----------
(define (find-arity-contradictions prog)
  (let* ((defs (program-definitions prog))
         (calcs (program-calculations prog))
         (arity-tab (build-arity-table defs)))
    (let loopcalcs ((cs calcs) (acc '()))
      (if (null? cs)
          (reverse acc)
          (let* ((expr (calc-expr (car cs)))
                 (calls (collect-calls expr)))
            (let loopcalls ((xs calls) (acc2 acc))
              (if (null? xs)
                  (loopcalcs (cdr cs) acc2)
                  (let* ((rec (car xs))
                         (fname (car rec))
                         (argc (cadr rec))
                         (pc (assoc-get fname arity-tab)))
                    ;; Only check arity if function is defined
                    (if (and pc (not (= argc pc)))
                        (loopcalls (cdr xs) (cons fname acc2))
                        (loopcalls (cdr xs) acc2))))))))))

;; ---------- Rule 4: Missing Function Name ----------
;; Any list expression whose first element is also a list: ((x y) ...) etc.
;; Return the offending subexpressions in preorder.
(define (collect-missing-fnames expr)
  (cond
    ((null? expr) '())
    ((number? expr) '())
    ((symbol? expr) '())
    ((pair? expr)
     (let ((head (car expr)))
       (if (pair? head)
           (append
            (list expr)
            ;; preorder: first expr, then children
            (let loop ((xs expr) (acc '()))
              (if (null? xs)
                  (reverse acc)
                  (loop (cdr xs) (append (reverse (collect-missing-fnames (car xs))) acc)))))
           ;; normal list: just traverse children
           (let loop ((xs expr) (acc '()))
             (if (null? xs)
                 (reverse acc)
                 (loop (cdr xs) (append (reverse (collect-missing-fnames (car xs))) acc)))))))
    (else '())))

(define (find-missing-function-names prog)
  (let ((calcs (program-calculations prog)))
    (let loop ((cs calcs) (acc '()))
      (if (null? cs)
          (reverse acc)
          (let* ((expr (calc-expr (car cs)))
                 (bad (collect-missing-fnames expr)))
            (loop (cdr cs) (append (reverse bad) acc)))))))

;; ---------- Rule 5: Undefined Functions ----------
;; Any function used in calculations (in function position) must be defined.
;; Arity mismatch does NOT matter here.
(define (find-undefined-functions prog)
  (let* ((defs (program-definitions prog))
         (calcs (program-calculations prog))
         (arity-tab (build-arity-table defs))) ;; presence implies defined
    (let loopcalcs ((cs calcs) (acc '()))
      (if (null? cs)
          (reverse acc)
          (let* ((expr (calc-expr (car cs)))
                 (calls (collect-calls expr)))
            (let loopcalls ((xs calls) (acc2 acc))
              (if (null? xs)
                  (loopcalcs (cdr cs) acc2)
                  (let* ((rec (car xs))
                         (fname (car rec))
                         (pc (assoc-get fname arity-tab)))
                    (if pc
                        (loopcalls (cdr xs) acc2)
                        (loopcalls (cdr xs) (cons fname acc2)))))))))))

;; ============================================================
;; End of file
;; ============================================================
