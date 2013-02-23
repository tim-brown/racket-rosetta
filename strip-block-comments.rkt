#lang racket/base
(require (for-syntax racket/base))
(require parser-tools/lex (prefix-in : parser-tools/lex-sre))

(define-syntax define-comment-stripper
  (lambda (stx)
    (syntax-case stx ()    
      [(_ id) ; default to C/Java-style
       #'(define-comment-stripper id "/*" "*/")]
      [(_ id block-comment-start block-comment-end)
       (let ((block-comment-start-1st
              (datum->syntax stx (substring (syntax->datum #'block-comment-start) 0 1))))
       #`(define id
           (lexer
            ;; Optional rule which reads anything up to something
            ;; that is not the beginning of block-comment-start.
            ;; The lexer will then stesp into either the comment-matcher
            ;; or the character-step-by-step.
            [(:: (:+ (:~ #,block-comment-start-1st))) lexeme]
            ;; Cunningness provided by parser-tools/lex manual page
            [(:: block-comment-start
                 (complement (:: any-string block-comment-end any-string))
                 block-comment-end)
             (id input-port)]
            ;; Anything else comes out a single-character-string at a time
            [any-char lexeme])))])))

(with-input-from-file "strip-block-comments.txt"
  (lambda ()
    (define-comment-stripper c-comment-stripper "/*" "*/")
    (let read-more ((tok (c-comment-stripper (current-input-port))))
      (unless (eq? 'eof tok)
        (display tok)
        (read-more (c-comment-stripper (current-input-port)))))))