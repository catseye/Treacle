;
; Support for matching of patterns containing contexts (holes)
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
; A traditional unifier is a set of (variable name, value) pairs indicating
; what value is bound to each variable name.  In our case, unifiers contain
; (name, term index) pairs which bind named to indices into the subject term.
; In a sense, conventional unifiers are unifiers "by value" while Treacle's
; are unifiers "by reference".
;
; Note also that in these unifiers, the same name can be bound to *multiple*
; positions within the subject, since the name may occur in any number of
; positions in the pattern, and will match as long as those subterms are
; equivalent.
;

;
; Create and return a new, empty unifier.
;
(define mk-empty-unifier
  (lambda ()
    '()))

;
; Extend the given unifier to one where the given name is associated with
; the given term index in the given subject term.  If such an extension is
; not possible (i.e. the name is already bound to an inequivalent term at
; a different index in the subject,) then #f is returned.
;
(define bind-name
  (lambda (subject index name unifier)
    (if (scour-unifier? subject index name unifier)
      (cons (cons name index) unifier)
      #f)))

;
; Helper function for bind-name.  Returns #t if it's OK to extend the
; unifier with the given name->index association, #f otherwise
;
(define scour-unifier?
  (lambda (subject index name unifier)
    (cond
      ((null? unifier)
        #t)
      (else
        (let* ((pair        (car unifier))
               (bound-name  (car pair))
               (bound-index (cdr pair)))
          (cond
            ((not (eq? name bound-name))
              (scour-unifier? subject index name (cdr unifier)))
            ((eqv? index bound-index) ; already bound to same place: ok
              (scour-unifier? subject index name (cdr unifier)))
            ((eqv? (term-index-fetch subject index) ; already bound to equiv
                   (term-index-fetch subject (cdr pair))) ; term: alright
              (scour-unifier? subject index name (cdr unifier)))
            (else                ; already bound to something else: not good
              #f)))))))

;
; Given a subject, a replacement, and a unifier, return a term which is like
; the replacement except where where each of the placeholders in the replacement
; has been replaced by the associated term referenced in the unifier.
;
(define expand-vars
  (lambda (subject replacement unifier generation-id)
    (cond ((is-named? replacement)      ; variable - replace if in unifier
            (let* ((pair (assq (get-name replacement) unifier)))
              (cond ((pair? pair)
                      (term-index-fetch subject (cdr pair)))
                    (else
                      replacement))))
          ((is-newref? replacement)
            (string->symbol (string-append "unique-ref-" (number->string generation-id))))
          ((list? replacement)          ; list - recurse
            (map (lambda (subpattern)
                   (expand-vars subject subpattern unifier generation-id))
                 replacement))
          (else                         ; ground term - leave it alone.
            replacement))))
