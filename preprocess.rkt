#lang racket
(provide add-active-token def-active-token process-string)

; hash for storing token-function bindings
(define tokenizedFunctions (make-hash))

; returns pair containing the positions of the first matched regexp
; below has car,cdr versions which return the corresponding element of pair.
(define (firstMatched regexp str)
   ( first (regexp-match-positions regexp str)))

(define (carFirstMatched regexp str)
   ( car (first (regexp-match-positions regexp str))))

(define (cdrFirstMatched regexp str)
   ( cdr (first (regexp-match-positions regexp str))))


; method for returning substring starting from a character to the end
(define (StrFromToEnd ch str)
(substring str (cdr (firstMatched ch str)) (string-length str)))

; method for returning substring starting from beginning to a character
(define (StrToChar ch str)
(substring str 0 (+ (car (firstMatched ch str)) 1)))




; adds token to the hash table mapping it with the function
(define (add-active-token token func)
  (hash-set! tokenizedFunctions token func)
  )


; Takes (define-active-token token (params ...) body ...)
; Transforms into (define funcName (params ...) (body ...))
;                    (add-active-token token func)

(define-syntax (def-active-token stx)
  (syntax-case stx()
    [(_ token (args ...) body0 body ...)
          #'( begin
             (define (_ args ...)
              body0 body ...)
            (add-active-token token _))]))

; var-Inference. Takes the sub string checks 
; if it has form " someVariable = new someType(..."
; if it does append someType in the beginning.
(def-active-token "var" (str)
  ( cond [ (and (equal? (string-ref str 0) #\space)
                (< (carFirstMatched "[a-zA-z]" str) (carFirstMatched "=" str)))
           (string-append  (substring str  (cdrFirstMatched "new " str )
                     (carFirstMatched "\\(" str )) str)]
         [ else str]))


; Looks for following #/ and adds the " + (...) + "
; around the triggered blocks.
(def-active-token "#" (str)
  (cond [(not  (regexp-match-positions "#{" str)) (string-append str " ")]
        [else
         ((hash-ref tokenizedFunctions "#")
         (string-append
           (substring str 0 
          (car (firstMatched "#" str)))
          ( string-append "\" + ("
            ( string-append (string-append 
          (substring str  (cdrFirstMatched "{" str)
                     (carFirstMatched "}" str))
          ") + \"" )
          (substring str (cdrFirstMatched "}" str)
                     (string-length str))) )) )]))

;method finds all matching aliases and replaces with  user given rplcmnt
; alias Type = Expression; < is expected then rplcmnt becomes " Expression" 
;Aliases can be followed by special symbols but not letters.
;Also End of File can't be followed since the code ending on
;e.g."... String" are syntatically incorrect in java
(def-active-token "alias" (str)
  ( cond [ (and (equal? (string-ref str 0) #\space)
                (< (carFirstMatched "[a-zA-z0-9]" str)
                   (carFirstMatched "=" str))) 
 ( let ((al (string-append (substring str (carFirstMatched "[a-zA-z0-9]" str) (+ (carFirstMatched "[a-zA-z0-9]( *)?(\t*)?(\n*)?=" str) 1) )
             "[^a-zA-Z0-9]")))
   (let ((rplcmnt ( string-append (substring str (cdrFirstMatched "=" str)  (carFirstMatched ";" str) )) ))
   ( cond [(not  (regexp-match-positions al (StrFromToEnd ";" str))) (substring str (cdrFirstMatched ";" str) )]
          [ else ((hash-ref tokenizedFunctions "alias") ( let (( afterAliasStr ( StrFromToEnd ";" str)))
                  ( string-append* ( list (StrToChar ";" str) (substring afterAliasStr 0 (carFirstMatched al afterAliasStr))
                                      rplcmnt  (substring afterAliasStr 
                                                ( - (cdrFirstMatched al afterAliasStr) 1) (string-length afterAliasStr))))))])))]
         [else str]))

;Called by TknPosPairList for single pair extraction
(define (TknPosPair tkn str)
  ( cond [(not (regexp-match-positions tkn str)) (cons tkn -1)]
         [else (cons tkn (cdr (firstMatched tkn str)))]
   ))

; returns list of (token, pos) pairs.
; Pos is the location of the first matched token in the string
(define (TknPosPairList tknList str)
   (map (curryr TknPosPair str) tknList ))

; recursively returns the pair with min pos
; if no such pair exists returns the first pair w/ pos = -1.
(define (firstTkn tknList minTkn)
  (cond [(empty? tknList) minTkn]
        [ else (cond
                 [(or ( eq? (cdr (first tknList)) -1)
                   (and  (> (cdr (first tknList)) (cdr minTkn))
                         (not ( eq? (cdr minTkn) -1) ) ) )
                  (firstTkn (rest tknList) minTkn)]
                [else (firstTkn (rest tknList) (first tknList))]
        )]))

;(define (findPos tknList tknName curPos)
;  (cond [(equal? (car (first tknList)) tknName) curPos]
;        [findPos (rest tknList) tknName (+ curPos)]))

(define (updatePairList curTkn tkn)
    ( cond [(equal? (car curTkn) (car tkn)) (cons ( car curTkn) (cdr curTkn))
                                            ]
         [else (cons (car tkn) (cdr tkn))]
   ))


; updates the position of curTkns in tknList

(define (updateTknList Tkns curTkn str oldStr)
       ( let ((newCurTkn (map (curry  updatePairList (cons (car curTkn)
                                                     (cond [ (not (regexp-match-positions (car curTkn) str)) -1]
                                                           [else  ( + ( cdr (firstMatched (car curTkn) str)) (- (string-length oldStr) (string-length str)))]
                               ))) Tkns ))) newCurTkn))  


;process-substring returns the updated substring after
;applying the given token to it.
(define (process-substring curTkn str)
  ((hash-ref tokenizedFunctions (car curTkn))
   (substring str (cdr curTkn) (string-length str))
  ))


; if curTkn has pos = -1, i.e.
; str can't be further processed, str is returned.
; Otherwise returns substring processed by token  
(define (process-string-loop str tknList curTkn)
  (cond [ (eq? -1 (cdr curTkn)) str]
        [else
         (let ((newStr (process-substring curTkn str)))
          (cond
            [(equal? newStr (substring str (cdr curTkn) (string-length str)))
             ( let (( newTkns (updateTknList tknList curTkn newStr str)))
              (process-string-loop str newTkns (firstTkn newTkns (first newTkns))))] 
            [else
             (let ((changedStr (string-append (substring str 0 ( - (cdr curTkn) ( string-length (car curTkn)) )) newStr)))
                (let (( tkns (TknPosPairList (hash-keys tokenizedFunctions) changedStr)))
      ( process-string-loop changedStr tkns (firstTkn tkns (first tkns)))) ) ]))]))


; The processor goes over the string and finds the active-tokens to be called
( define (process-string str)
   (let (( tkns (TknPosPairList (hash-keys tokenizedFunctions) str)))
     ( process-string-loop str tkns (firstTkn tkns (first tkns)))))
