;
; Support for term indices (pointers to subterms of terms)
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
; A term index is represented by a list of integers.  It uniquely
; identifies a subterm position of a term.
;

;
; Create a basic term index that refers to the entire term.
;
(define mk-base-index
  (lambda ()
    '()))

;
; Return a term index which points to the leftmost subterm of the
; term pointed to by the given term index.
;
(define descend-index
  (lambda (index)
    (cons 0 index)))

;
; Return a term index which points to the closest sibling term to
; the right of term pointed to by the given term index.
;
(define next-index
  (lambda (index)
    (cons (+ (car index) 1) (cdr index))))

;
; Retrieve the subterm of the given term at the given term index.
;
(define term-index-fetch
  (lambda (term index)
    (cond ((null? index)
            term)
          (else
            (term-index-fetch (list-ref term (car index)) (cdr index))))))

;
; Return a new term where the subterm at the given term index is replaced
; by the given replacement subterm.
;
(define term-index-store
  (lambda (term index replacement)
    (cond ((null? index)
            replacement)
          (else
            (let* ((nth-subterm (list-ref term (car index)))
                   (new-index   (cdr index))
                   (new-subterm (term-index-store nth-subterm new-index replacement)))
              (list-replace term (car index) new-subterm))))))

;
; Helper function for term-index-store.
;
(define list-replace
  (lambda (elems pos elem)
    (cond ((eq? pos 0)
            (cons elem (cdr elems)))
          (else
            (cons (car elems) (list-replace (cdr elems) (- pos 1) elem))))))
