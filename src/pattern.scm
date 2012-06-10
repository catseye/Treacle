;
; Basic data structures for context-rewriting patterns (names, holes, wildcards)
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

;
; Patterns are structured as follows.
;
; Atoms, numbers, and lists are literals, to be matched.
;
; A vector of length 1 where the first and only element is the
; atom 'wildcard' is a wildcard.
;
; A vector of length 3 where the first element is the atom 'named'
; is a named term.  The second element is an atom giving the name
; of the term, and the third element gives the subpattern so named.
;
; A vector of length 3 where the first element is the atom 'hole'
; is a hole.  The second element indicates the match search order
; to apply inside this hole, either 'innermost' or 'outermost'.  The
; third element is the subpattern to be matched inside this hole.
;

(define mk-wildcard
  (lambda ()
    (vector 'wildcard)))

(define mk-named
  (lambda (name subpat)
    (vector 'named name subpat)))

(define mk-hole
  (lambda (order subpat)
    (vector 'hole order subpat)))

(define mk-newref
  (lambda ()
    (vector 'newref)))

;
; Helper predicate for following predicates.
;
(define is-type?
  (lambda (label pattern)
    (and (vector? pattern)
         (eq? (vector-ref pattern 0) label))))

;
; Return #t if the given pattern is a named pattern, #f otherwise.
;
(define is-named?
  (lambda (pattern)
    (is-type? 'named pattern)))

;
; Return the name of the given pattern variable.
; Assumes that its input is in fact a pattern variable.
;
(define get-name
  (lambda (named-pattern)
    (vector-ref named-pattern 1)))     ; just return the 2nd element of the vector

;
; Return the name of the given pattern variable.
; Assumes that its input is in fact a pattern variable.
;
(define get-named-subpat
  (lambda (named-pattern)
    (vector-ref named-pattern 2)))     ; just return the 3nd element of the vector

;
; Return #t if the given pattern is a hole, #f otherwise.
;
(define is-hole?
  (lambda (pattern)
    (is-type? 'hole pattern)))

;
; Return the search order of the given hole.
; Assumes that its input is in fact a hole.
;
(define get-hole-order
  (lambda (hole)
    (vector-ref hole 1)))      ; just return the 2nd element of the vector

;
; Return the subpattern to search for in the given hole.
; Assumes that its input is in fact a hole.
;
(define get-hole-subpat
  (lambda (hole)
    (vector-ref hole 2)))      ; just return the 3rd element of the vector

;
; Return #t if the given pattern is a wildcard, #f otherwise.
;
(define is-wildcard?
  (lambda (pattern)
    (is-type? 'wildcard pattern)))

;
; Return #t if the given pattern is a newref, #f otherwise.
;
(define is-newref?
  (lambda (pattern)
    (is-type? 'newref pattern)))

;
; Ground terms are a subset of patterns which may not contain
; wildcards, holes, or named terms.
;
(define is-ground?
  (lambda (term)
    (cond
      ((is-wildcard? term)
        #f)
      ((is-hole? term)
        #f)
      ((is-named? term)
        #f)
      ((is-newref? term)
        #f)
      ((null? term)
        #t)
      ((list? term)
        (and (is-ground? (car term))
             (is-ground? (cdr term))))
      ((number? term)
        #t)
      ((symbol? term)
        #t)
      (else
        #f))))

;
; Replacements are a subset of patterns which may contain named
; terms, but may not contain wildcards or holes.
;
; In addition, replacements may contain newrefs, which are replaced
; with unique symbols upon expansion.
;
(define is-replacement?
  (lambda (term)
    (cond
      ((is-wildcard? term)
        #f)
      ((is-hole? term)
        #f)
      ((is-named? term)
        (is-replacement? (get-named-subpat term)))
      ((is-newref? term)
        #t)
      ((null? term)
        #t)
      ((list? term)
        (and (is-replacement? (car term))
             (is-replacement? (cdr term))))
      ((number? term)
        #t)
      ((symbol? term)
        #t)
      (else
        #f))))
