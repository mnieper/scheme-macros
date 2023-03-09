#!r6rs

(library (languages (2023 03 09))
  (export
    define-language
    nonterminals
    terminals
    rules
    start
    ->
    &syntax-error
    make-syntax-error-condition
    syntax-error-condition?
    parse
    parse-list)
  (import
    (rnrs)
    (languages transformers-abf79192-e4c6-4f01-9ccd-f74bd7f9614c))

  (define-syntax define-language
    define-language-transformer)

  (define parse-list
    (lambda (parser tokens)
      (define who 'parse-list)
      (unless (procedure? parser)
        (assertion-violation who "invalid parser argument" parser))
      (unless (list? tokens)
        (assertion-violation who "invalid token list argument" tokens))
    ((fold-left
       (lambda (parser token)
         (parser token))
       parser tokens))))

  (define parse
    (lambda (parser . tokens)
      (define who 'parse-list)
      (unless (procedure? parser)
        (assertion-violation who "invalid parser argument" parser))
      (parse-list parser tokens)))

  )
