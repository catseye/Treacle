;
; Support for reducing terms via context-rewriting
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
; Given a rule (a pair of a pattern and a map of replacements,)
; apply the rule to the given subject.  If the pattern part of
; the rule matches the subject, replace the subterms that matched
; named subpatterns with the expanded replacement whose key in
; the map is that name.
;
(define apply-rule
  (lambda (subject pattern replacements generation-id)
    (let* ((unifier (match subject subject pattern (mk-empty-unifier) (mk-base-index))))
      (if unifier
        (apply-unifier subject subject unifier unifier replacements generation-id)
        #f))))

;
; Helper function for apply-rule.  For each substitution in the unifier whose
; name is present in some replacement, expand that replacement with values from
; the unifier, and graft it into the subject at the position given in the unifier.
;
(define apply-unifier
  (lambda (complete-subject subject complete-unifier unifier replacements generation-id)
    (if (null? unifier)
      subject
      (let* ((unif-pair         (car unifier))
             (rest-of-unif      (cdr unifier))
             (name              (car unif-pair))
             (index             (cdr unif-pair))
             (repl-pair         (assq name replacements)))
        (if repl-pair
          (let* ((replacement   (cdr repl-pair))
                 (expanded-repl (expand-vars complete-subject replacement complete-unifier generation-id))
                 (new-subject   (term-index-store subject index expanded-repl)))
            (apply-unifier complete-subject new-subject complete-unifier rest-of-unif replacements generation-id))
          (apply-unifier complete-subject subject complete-unifier rest-of-unif replacements generation-id))))))

;
; Given a set of rules, apply repeatedly to subject until none apply.
;
(define reduce
  (lambda (subject complete-rules rules generation-id)
    (if (null? rules)
      subject
      (let* ((rule-pair     (car rules))
             (rest-of-rules (cdr rules))
             (pattern       (car rule-pair))
             (replacements  (cdr rule-pair))
             (new-gen-id    (+ generation-id 1))
             (new-subject   (apply-rule subject pattern replacements generation-id)))
        (if new-subject
          (reduce new-subject complete-rules complete-rules new-gen-id)
          (reduce subject complete-rules rest-of-rules new-gen-id))))))

;
; Useful shortcut for calling reduce.
;
(define toplevel-reduce
  (lambda (subject complete-rules)
    (reduce subject complete-rules complete-rules 0)))
