;
; Test suite for Treacle
; Chris Pressey, March 2008
;

; Copyright (c)2008 Cat's Eye Technologies.  All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright
;    notices, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notices, this list of conditions, and the following disclaimer in
;    the documentation and/or other materials provided with the
;    distribution.
; 3. Neither the names of the copyright holders nor the names of their
;    contributors may be used to endorse or promote products derived
;    from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

(load "utils.scm")
(load "pattern.scm");------------------------------------------------------

(test pattern-1
  (mk-named 'jim (mk-wildcard))
  #(named jim #(wildcard))
)

(test pattern-2
  (is-ground? (mk-named 'jim (mk-wildcard)))
  #f
)

(test pattern-3
  (is-ground? '(cat dog (rabbit) (oyster pigeon)))
  #t
)

(test pattern-4
  (is-replacement? (mk-named 'jim (mk-wildcard)))
  #f
)

(test pattern-5
  (is-replacement? (mk-named 'jim 0))
  #t
)

(load "index.scm");--------------------------------------------------------

(test index-fetch-1
  (term-index-fetch '(1 2 3) '())
  '(1 2 3)
)

(test index-fetch-2
  (term-index-fetch '(1 2 3) '(0))
  1
)

(test index-fetch-3
  (term-index-fetch '(1 2 (1 (1 2 99) (1 2 3))) '(2 1 2))
  99
)

(test index-store-1
  (term-index-store '(1 2 3) '(0) 99)
  '(99 2 3)
)

(load "unifier.scm");--------------------------------------------------------

(test bind-name-1
  (bind-name
    '(a b c (d e f))
    '(3)
    'ralph
    '()
  )
  '((ralph 3))
)

(test bind-name-2
  (bind-name
    '(a b c (d e f))
    '(3)
    'ralph
    '((ralph 0))
  )
  #f
)

(test bind-name-3
  (bind-name
    '(a b c a)
    '(3)
    'ralph
    '((ralph 0))
  )
  '((ralph 3) (ralph 0))
)

(test expand-vars-1
  (expand-vars
    '(a b c (d e f))
    '(i j #(named ralph k))
    '((ralph 3))
    0
  )
  '(i j (d e f))
)

(test expand-vars-2
  (expand-vars
    '(a b c (d e f))
    '(#(named ralph 0) #(named ed 0))
    '((ralph 3 1))
    0
  )
  '(e #(named ed 0))
)

(test expand-vars-3
  (expand-vars
    '(a b c (d e f))
    '(#(newref))
    '((ralph 3 1))
    33
  )
  '(unique-ref-33)
)

(load "match.scm");-----------------------------------------------------------

(test match-1
  (toplevel-match
    '(a (b c))
    '(a (b c))
  )
  '()
)

(test match-2
  (toplevel-match
    '(a (b c))
    '(b c)
  )
  #f
)

(test match-3
  (toplevel-match
    '(a (b c))
    '(a #(named ralph #(wildcard)))
  )
  '((ralph 1))
)

(test match-4
  (toplevel-match
    '(x right (y 1 2))
    '#(named t (x #(named i #(wildcard)) #(named j #(wildcard))))
  )
  '((t) (j 2) (i 1))
)

(test match-hole-1
  (toplevel-match
    '(a (b b b (c c c (d e)) b b b))
    '(a #(hole innermost e))
  )
  '()
)

(test match-hole-2
  (toplevel-match
    '(a (b b b (c c c (d e)) b b b))
    '(a #(hole innermost f))
  )
  #f
)

(test match-hole-3
  (toplevel-match
    '(a (b b (flag k) (c c c (d (flag a))) b b b))
    '(a #(hole innermost (flag #(named jim #(wildcard)))))
  )
  '((jim 1 2 1))
)

(test match-hole-4
  (toplevel-match
    '(a (b b (flag k) (c c c (d (flag a))) b b b))
    '(a #(hole innermost #(named jim (flag #(wildcard)))))
  )
  '((jim 1 2))
)

(test match-hole-5
  (toplevel-match
    '(a (b b (flag k) (c c c (d (flag a))) b b b))
    '(a #(named jim #(hole innermost (flag #(wildcard)))))
  )
  '((jim 1))
)

(test match-hole-6
  (toplevel-match
    '(pair a (b b (flag k) (c c c (d (flag a))) b b b))
    '(pair #(named jim #(wildcard)) #(hole innermost (flag #(named bones #(named jim #(wildcard))))))
  )
  '((bones 2 3 3 1 1) (jim 2 3 3 1 1) (jim 1))
)

(test match-order-1
  (toplevel-match
    '(thing (flag (world (a b c) (a b (flag k)))) thang)
    #(hole innermost #(named jim (flag #(wildcard))))
  )
  '((jim 1 1 2 2))
)

(test match-order-2
  (toplevel-match
    '(thing (flag (world (a b c) (a b (flag k)))) thang)
    #(hole outermost #(named jim (flag #(wildcard))))
  )
  '((jim 1))
)

(test match-order-3
  (toplevel-match
    '(ast (+ _ (* (lit 2) (lit 3))))
    '(ast #(hole innermost #(named src (lit #(wildcard)))))
  )
  '((src 1 2 1))
)

(load "reduce.scm");----------------------------------------------------------

(test apply-rule-1
  (apply-rule
    '(a b c)
    #(named jim (a b c))
    '((jim . k) (bones 1 2 3))
    0
  )
  'k
)

(test apply-rule-2
  (apply-rule
    '(x this (x descends (x to (x the (x right (y 1 2))))))
    '#(hole innermost #(named t (x #(named i #(wildcard)) #(named j #(wildcard)))))
    '((t . (xx #(named j 0) #(named i 0))))
    0
  )
  '(x this (x descends (x to (x the (xx (y 1 2) right)))))
)

(test reduce-1
  (toplevel-reduce
    '(a b c)
    '(
       ( #(named jim (a b c)) . ((jim . k) (bones 1 2 3)) )
     )
  )
  'k
)

(test reduce-2
  (toplevel-reduce
    '(x this (x descends (x to (x the (x right (y 1 2))))))
    '(
      ( ; rule 1
        #(hole innermost #(named t (x #(named i #(wildcard)) #(named j #(wildcard))))) .
        ((t . (xx #(named j 0) #(named i 0))))
      )
     )
  )
  '(xx (xx (xx (xx (xx (y 1 2) right) the) to) descends) this)
)

(test reduce-3
  (toplevel-reduce
    '(x this (x descends (x to (x the (x right (y 1 2))))))
    '(
      ( ; rule 1
        #(hole innermost #(named t (x #(named i #(wildcard)) #(named j #(wildcard))))) .
        ((t . (xx #(named j 0) #(named i 0))))
      )
      ( ; rule 2
        #(hole innermost #(named p right)) .
        ((p . left))
      )
     )
  )
  '(xx (xx (xx (xx (xx (y 1 2) left) the) to) descends) this)
)

(load "syntax.scm");-----------------------------------------------------------

(test syntax-term-1
  (term-atom (a b c))
  '(a b c)
)

(test syntax-term-2
  (term-list a b c)
  '(a b c)
)

(test syntax-term-3
  (term-atom (a * c))
  '(a #(wildcard) c)
)

(test syntax-term-4
  (term-atom *)
  #(wildcard)
)

(test syntax-term-5
  (term-atom (a (? bob *) (c d @) f g))
  '(a #(named bob #(wildcard)) (c d #(newref)) f g)
)

(test syntax-replacements-1
  (replacements a : (a b @)  b : (? eb *))
  '(
     (a . (a b #(newref)))
     (b . #(named eb #(wildcard)))
   )
)

(test syntax-rules-1
  (rules
    (:i (? t (x (? i *) (? j *)))) -> ( t : (xx (? j 0) (? i 0))    )
    (:i (? p right))               -> ( p : left )
  )
  '(
    (
      #(hole innermost #(named t (x #(named i #(wildcard)) #(named j #(wildcard))))) .
      ((t . (xx #(named j 0) #(named i 0))))
    )
    (
      #(hole innermost #(named p right)) .
      ((p . left))
    )
  )
)

;-------------------------------------------------------------------
; Forest-rewriting, a la Arboretuum.
;-------------------------------------------------------------------

(test rewrite-forest-1
  (toplevel-reduce
    '(forest (ast (+ (lit 3) (* (lit 2) (lit 3))))
             (out halt))
    '(
       ( ; rule 1
         (forest (ast #(hole innermost #(named src (lit #(named val #(wildcard))))))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (push #(named val) halt)))
       )
       ( ; rule 2
         (forest (ast #(hole innermost #(named src (+ _ _))))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (add halt)))
       )
       ( ; rule 3
         (forest (ast #(hole innermost #(named src (* _ _))))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (mul halt)))
       )
     )
  )
  '(forest (ast _) (out (push 3 (push 2 (push 3 (mul (add halt)))))))
)

(test rewrite-forest-2
  (toplevel-reduce
    '(forest (stab (a 4 eot))
             (ast  (+ 1 2 3 a 5 6 a 7 8 9)))
    '(
       ( ; rule 1
         (forest (stab #(hole innermost (#(named n #(wildcard)) #(named v #(wildcard)) #(named tab #(wildcard)))))
                 (ast  #(hole innermost #(named dest #(named n #(wildcard)))))) .
         ((dest . #(named v)))
       )
     )
  )
  '(forest (stab (a 4 eot)) (ast (+ 1 2 3 4 5 6 4 7 8 9)))
)

(test rewrite-forest-3
  (toplevel-reduce
    '(forest (ast (let a (lit 4) (+ (lit 3) (* (var a) (lit 3)))) )
             (stab eot)
             (out halt))
    '(
       ( ; rule 1
         (forest (ast  #(hole innermost #(named src
                          (let #(named n #(wildcard)) #(named v #(wildcard)) #(named expr #(wildcard)))  )))
                 (stab #(hole innermost #(named dest  eot)))
                 (out  #(wildcard))) .
         ((src . #(named expr 0)) (dest . (#(named n 0) #(named v 0) eot)))
       )
       ( ; rule 2
         (forest (ast  #(hole innermost #(named src (var #(named n #(wildcard))))))
                 (stab #(hole innermost (#(named n #(wildcard)) #(named v #(wildcard)) #(wildcard))))
                 (out  #(wildcard))) .
         ((src . #(named v 0)))
       )
       ( ; rule 3
         (forest (ast  #(hole innermost #(named src (lit #(named val #(wildcard))))))
                 (stab #(wildcard))
                 (out  #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (push #(named val) halt)))
       )
       ( ; rule 4
         (forest (ast #(hole innermost #(named src (+ _ _))))
                 (stab #(wildcard))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (add halt)))
       )
       ( ; rule 5
         (forest (ast #(hole innermost #(named src (* _ _))))
                 (stab #(wildcard))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (mul halt)))
       )
     )
  )
  '(forest (ast _)
           (stab (a (lit 4) eot))
           (out (push 3 (push 4 (push 3 (mul (add halt)))))))
)

;
; This test is close to (although not exactly) what we'd like to see, for
; translating "if" statements to machine code.  It uses newref to generate
; labels for the jumps.  It rewrites the AST several times to ensure that
; the jumps and labels are generated in the right order.
;
(test rewrite-forest-4
  (toplevel-reduce
    '(forest (ast (if (> (lit 6) (lit 4)) (print (lit 1)) (print (lit 2))) )
             (out halt))
    '(
       ( ; rule -- get label for if
         (forest (ast #(hole innermost #(named src (if _ #(named then #(wildcard)) #(named else #(wildcard)) ))))
                 (out #(wildcard))) .
         ((src . (iflab #(named then 0) #(named else 0) #(newref))))
       )
       ( ; rule -- reduce if to then
         (forest (ast #(hole innermost #(named src
                         (iflab #(named then #(wildcard)) #(named else #(wildcard)) #(named elselab #(wildcard)))
                 )))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . (then #(named then 0) #(named else 0) #(named elselab 0)))
          (dest . (jmp-if-false #(named elselab 0) halt)))
       )
       ( ; rule -- reduce then to else
         (forest (ast #(hole innermost #(named src (then _ #(named else #(wildcard)) #(named elselab #(wildcard))))))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . #(named else 0)) (dest . (label #(named elselab 0) halt)))
       )
       ( ; rule -- translate operator
         (forest (ast #(hole innermost #(named src (> _ _))))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (gt halt)))
       )
       ( ; rule -- translate command
         (forest (ast #(hole innermost #(named src (print _))))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (print halt)))
       )
       ( ; rule -- translate literal
         (forest (ast #(hole innermost #(named src (lit #(named val #(wildcard))))))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (push #(named val) halt)))
       )
     )
  )
  '(forest (ast _)
           (out (push 6 (push 4 (gt (jmp-if-false unique-ref-16
                                     (push 1 (print (label unique-ref-16 (push 2 (print halt)))))))))))
)

;
; This test is pretty much exactly what we'd like to see for translation of
; "if" statements to machine code.  It relies on the fact that all newrefs
; in a replacement generate the same new reference.  It also uses an auxilliary
; tree, the bpt (branch point table) instead of rewriting the main AST to
; clarify somewhat the dependencies.
;
(test rewrite-forest-5
  (toplevel-reduce
    '(forest (ast (if (> (lit 6) (lit 4)) (print (lit 1)) (print (lit 2))) )
             (bpt eot)
             (out halt))
    '(
       ( ; rule -- get label for if
         (forest (ast #(hole innermost #(named src (if _ #(named then #(wildcard)) #(named else #(wildcard)) ))))
                 (bpt #(hole innermost #(named branch eot)))
                 (out #(hole innermost #(named dest halt)))) .
         ((branch . (then #(newref))) (dest . (jmp-if-false #(newref) halt)))
       )
       ( ; rule -- get label for if
         (forest (ast #(hole innermost #(named src (if _ _ #(named else #(wildcard)) ))))
                 (bpt #(hole innermost #(named branch (then #(named ref #(wildcard))) eot)))  ; XXX???
                 (out #(hole innermost #(named dest halt)))) .
         ((branch . (else #(newref))) (dest . (goto #(newref) (label #(named ref 0) halt))))
       )
       ( ; rule -- get label for if
         (forest (ast #(hole innermost #(named src (if _ _ _) )))
                 (bpt #(hole innermost #(named branch (else #(named ref #(wildcard))) eot)))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (branch . eot) (dest . (label #(named ref 0) halt)))
       )
       ( ; rule -- translate operator
         (forest (ast #(hole innermost #(named src (> _ _))))
                 (bpt #(wildcard))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (gt halt)))
       )
       ( ; rule -- translate command
         (forest (ast #(hole innermost #(named src (print _))))
                 (bpt #(wildcard))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (print halt)))
       )
       ( ; rule -- translate literal
         (forest (ast #(hole innermost #(named src (lit #(named val #(wildcard))))))
                 (bpt #(wildcard))
                 (out #(hole innermost #(named dest halt)))) .
         ((src . _) (dest . (push #(named val 0) halt)))
       )
     ))
  '(forest (ast _)
           (bpt eot)
           (out (push 6 (push 4 (gt (jmp-if-false unique-ref-16
                  (push 1 (print (goto unique-ref-29
                  (label unique-ref-16 (push 2 (print
                  (label unique-ref-29 halt)))))))))))))
)

; Treacle syntax for previous test.

(test rewrite-forest-6
  (toplevel-reduce
    '(forest (ast (if (> (lit 6) (lit 4)) (print (lit 1)) (print (lit 2))) )
             (bpt eot)
             (out halt))
    (rules
      (forest (ast (:i (? src (if _ (? then *) (? else *)))))
              (bpt (:i (? branch eot)))
              (out (:i (? dest halt))))
      -> ( branch : (then @)  dest : (jmp-if-false @ halt) )

      (forest (ast (:i (? src (if _ _ (? else *)))))
              (bpt (:i (? branch (then (? ref *)))))
              (out (:i (? dest halt))))
      -> ( branch : (else @)  dest : (goto @ (label (? ref *) halt)) )

      (forest (ast (:i (? src (if _ _ _))))
              (bpt (:i (? branch (else (? ref *)))))
              (out (:i (? dest halt))))
      -> ( src : _  branch : eot  dest : (label (? ref *) halt) )

      (forest (ast (:i (? src (> _ _ ))))
              (bpt *)
              (out (:i (? dest halt))))
      -> ( src : _  dest : (gt halt) )

      (forest (ast (:i (? src (print _))))
              (bpt *)
              (out (:i (? dest halt))))
      -> ( src : _  dest : (print halt) )

      (forest (ast (:i (? src (lit (? val *)))))
              (bpt *)
              (out (:i (? dest halt))))
      -> ( src : _  dest : (push (? val *) halt) )

    ))
  '(forest (ast _)
           (bpt eot)
           (out (push 6 (push 4 (gt (jmp-if-false unique-ref-16
                  (push 1 (print (goto unique-ref-29
                  (label unique-ref-16 (push 2 (print
                  (label unique-ref-29 halt)))))))))))))
)
