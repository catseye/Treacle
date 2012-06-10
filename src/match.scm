;
; Support for matching context-rewriting patterns (names, wildcards, holes)
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
; Shorthand for common usage of match.
;
(define toplevel-match
  (lambda (subject pattern)
    (match subject subject pattern (mk-empty-unifier) (mk-base-index))))

;
; Attempt to find a unifier (list of substitutions) which makes
; the given pattern equal to the given term, and return it.
; Return #f if the pattern does not match.
;
; Note that match, upon matching a hole, calls search-match.
; This is mutually recursive, since search-match also calls match.
;
(define match
  (lambda (subject term pattern unifier index)
    (cond ((is-wildcard? pattern)
            unifier)
          ((is-named? pattern)
            (let* ((name     (get-name pattern))
                   (subpat   (get-named-subpat pattern))
                   (submatch (match subject term subpat unifier index)))
              (cond (submatch
                      ; note that we pass the whole subject here
                      (bind-name subject (reverse index) name submatch))
                    (else
                      #f))))
          ((is-hole? pattern)
            (let* ((order  (get-hole-order pattern))
                   (subpat (get-hole-subpat pattern)))
              (cond ((eq? order 'innermost)
                      (search-match-innermost subject term subpat unifier index))
                    ((eq? order 'outermost)
                      (search-match-outermost subject term subpat unifier index))
                    (else
                      #f))))
          ((and (list? term) (list? pattern))
            (match-lists subject term pattern unifier (descend-index index)))
          ((eqv? term pattern)
            unifier)
          (else
            #f))))

;
; Helper function for match.
; Given a term and a pattern, where we know both are lists,
; fold over both of them, matching all the corresponding elements.
;
(define match-lists
  (lambda (subject term pattern unifier index)
    (cond ((and (null? term) (null? pattern))  ; end of both
            unifier)
          ((or (null? term) (null? pattern))   ; end of one but not the other
            #f)
          (else
            (let ((new-unifier (match subject (car term) (car pattern) unifier index))
                  (new-index   (next-index index)))
              (if new-unifier
                (match-lists subject (cdr term) (cdr pattern) new-unifier new-index)
                #f))))))

;
; Match the given pattern to any subterm of the given term, if possible.
; Give priority to the leftmost outermost subterm that matches.
; Returns a new unifier, or #f.
;
; While searching, this tracks a term index which points to the subterm
; that matches.  We pass this back in the returned unifier.
;
(define search-match-outermost
  (lambda (subject term pattern unifier index)
    (let* ((new-unifier (match subject term pattern unifier index)))
      (cond (new-unifier
              new-unifier)
            ((list? term)
              (search-match-list-outermost subject term pattern unifier (descend-index index)))
            (else
              #f)))))

;
; Helper function.  Try to match the given pattern to each term in the
; given list, left to right.  Return new unifier on first successful
; match, or #f is there is none.
;
(define search-match-list-outermost
  (lambda (subject terms pattern unifier index)
    (cond ((null? terms)
            #f)
          (else
            (let* ((new-unifier (search-match-outermost subject (car terms) pattern unifier index))
                   (new-index   (next-index index)))
              (if new-unifier
                new-unifier
                (search-match-list-outermost subject (cdr terms) pattern unifier new-index)))))))

;
; Match the given pattern to any subterm of the given term, if possible.
; Give priority to the leftmost innermost subterm that matches.
; Returns a new unifier, or #f.
;
; While searching, this tracks a term index which points to the subterm
; that matches.  We return this in the returned unifier.
;
(define search-match-innermost
  (lambda (subject term pattern unifier index)
    (cond ((list? term)
            (let* ((new-unifier (search-match-list-innermost subject term pattern unifier (descend-index index))))
              (if new-unifier
                new-unifier
                (match subject term pattern unifier index))))
          (else
            (match subject term pattern unifier index)))))

;
; Helper function.  Try to match the given pattern to each term in the
; given list, left to right.  Return new unifier on first successful
; match, or #f is there is none.
;
(define search-match-list-innermost
  (lambda (subject terms pattern unifier index)
    (cond ((null? terms)
            #f)
          (else
            (let* ((new-unifier (search-match-innermost subject (car terms) pattern unifier index))
                   (new-index   (next-index index)))
              (if new-unifier
                new-unifier
                (search-match-list-innermost subject (cdr terms) pattern unifier new-index)))))))
