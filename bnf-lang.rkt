#lang racket/base

(require ee-lib/define
         (for-syntax
          ee-lib
          syntax/parse
          racket/base))

(define-literal-forms bnf-literals
  "bnf forms cannot be used in a miniKanren or Racket expression context"
  [make-rule
   define-grammar
   #%literal
   #%non-literal])

(begin-for-syntax
  ;;;;;;;; structure types for variable and macro binding

  (struct rule-binding-rep [])
  (struct grammar-binding-rep [])
  (struct rule-macro-rep [])
  (struct grammar-macro-rep [])

  ;;;;;; template of the expander

  (define/hygienic (expand-make-rule stx) #:definition
    (syntax-parse stx
      #:literal-sets (bnf-literals)
      #:literals (quote)
      [(_ rule-name:id rewrite ...+)
       ;; check that rule-name hasn't been taken
       ;; check that rule is not name of this grammar
       #`(make-rule rule-name (stx-map expand-term rewrite) ...)]))

  (define/hygienic (expand-define-grammar stx) #:definition
    (syntax-parse stx
      #:literal-sets (bnf-literals)
      #:literals (quote)
      [(_ grammar-name:id rule ...)
       ;; check that grammar name hasn't been taken
       #`(define-grammar grammar-name (stx-map expand-make-rule rule) ...)])))


