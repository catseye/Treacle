;
; Provisional Syntax for Treacle Forms
; Chris Pressey, April 2008
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

(load "pattern.scm")

;
; Syntax for atomic terms, including patterns and replacements.
;
(define-syntax term-atom
  (syntax-rules (* ? :i :o @)
    ((term-atom *)
      (mk-wildcard))
    ((term-atom @)
      (mk-newref))
    ((term-atom (? name subterm))
      (mk-named 'name (term-atom subterm)))
    ((term-atom (:i subterm))
      (mk-hole 'innermost (term-atom subterm)))
    ((term-atom (:o subterm))
      (mk-hole 'outermost (term-atom subterm)))
    ((term-atom (inner ...))
      (term-list inner ...))
    ((term-atom other)
      'other)))

;
; Syntax for list terms.
;
(define-syntax term-list
  (syntax-rules ()
    ((term-list)
      '())
    ((term-list atom rest ...)
      (cons (term-atom atom) (term-list rest ...)))))

;
; Syntax for replacements.
;
(define-syntax replacements
  (syntax-rules (:)
    ((replacements)
      '())
    ((replacements name : replacement rest ...)
      (cons (cons 'name (term-atom replacement)) (replacements rest ...)))
  ))

;
; Syntax for rules.
;
(define-syntax rules
  (syntax-rules (->)
    ((rules)
      '())
    ((rules pattern -> (repls ...) rest ...)
      (cons (cons (term-atom pattern) (replacements repls ...)) (rules rest ...)))
  ))
