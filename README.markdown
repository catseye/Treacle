The Treacle Programming Language
================================

Language version 1.0.  
Chris Pressey, Cat's Eye Technologies

Introduction
------------

Treacle is a programming language based on an extended form of
term-rewriting which we shall call, somewhat inaccurately (or at least
arbitrarily,) _context rewriting_.

Like Arboretuum, its successor built around _forest-rewriting_, Treacle
was intended as a language for specifying compilers. Treacle is somewhat
more successful at pulling it off, however; context rewriting
encompasses, and is more expressive than, forest-rewriting.

Context rewriting is meant to refer to the fact that Treacle's rewriting
patterns may contain _holes_ – designated "containers" for subpatterns
which may match not just the *immediate* child of the term which the
parent pattern matched (as in conventional term-rewriting) but also *any
one of that child's descendents*, no matter how deeply nested.

When a hole is matched to some term, that term is searched for the
subpattern given inside the hole. The search may be performed in either
leftmost-innermost or leftmost-outermost order; this is specified by a
qualifier associated with the hole. Because of this, Treacle need not
specify a language-wide reduction order; the hole construct acts as a
kind of search operator which explicitly encodes search order into each
pattern.

Context rewriting also deconstructs the conventional concept of the
variable, splitting it into a _name_ and a _wildcard_. Any pattern or
subpattern may be named, not just wildcards. Even holes may be named. At
the same time, wildcards, which match arbitrary terms, may occur
unnamed. Upon a successful match, only those terms which matched named
patterns are recorded in the unifier.

Further, each rule in Treacle may contain multiple terms (_replacements_)
on the right-hand side of a rewriting rule, and each of these may have
its own name. When the term undergoing rewriting (called the _subject_) is
rewritten, each named replacement is substituted into the subject at the
position matched by the part of the pattern that is labelled by that
same name.

Lastly, replacements may contain special atomic terms called _newrefs_.
When a newref is written into the subject, it takes the form of a new,
unique symbol, guaranteed (or at least reasonably assumed) to be
different from all other symbols that are in, or could be in, the
subject. When multiple newrefs (possibly in multiple replacements) in
the same rule are written into the subject at the same time (i.e., on
the same rewriting step,) they all take the same form (and so are equal
to each other, and only to each other – nothing else.) In Treacle's
capacity as a compiler-definition language, newrefs are useful for
generating internal labels for, e.g., translating control structures to
machine code jumps.

It is important to remember that, while subpatterns may be nested in
holes, and these may in turn contain more holes, there is no
corresponding hierarchical nature to the *bindings* which occur in
Treacle patterns: all variables of the same name must unify to
equivalent terms, regardless of where they occur in the pattern (inside
or outside a hole.)

Syntax
------

We're almost ready to give some examples to elucidate all this, but
first we need a syntax to give them in. Here it is:

-   atoms are denoted by strings of lower-case letters;
-   terms are denoted by lists of subterms inside parentheses;
-   named terms are denoted by `(? name subterm)`;
-   holes are denoted by `(:i subterm)` or `(:o subterm)`, corresponding
    to innermost and outermost search order, respectively;
-   wildcards are denoted by `*`;
-   newrefs are denoted by `@`; and
-   named replacements are denoted `X : term`.

Examples
--------

Now we are ready to give some examples.

### Patterns ###

-   The pattern `(a b (? X *))` matches `(a b (c (d b)))`, with the
    unifier `X=(c (d b))`. Also, `(a (? Y *) (c (d (? Y *))))` matches
    the same subject with `Y=b`. This is all quite conventional.
-   We can also match `(a (? X b) *)` to this subject. The unifier will
    *always* be `X=b` when this pattern matches, regardless of the
    subject. This tells us nothing we did not already know. But it
    demonstrates the decoupling of names and wildcards in Treacle. (It
    will also become useful when we get to replacements, since that
    atomic `b` term named by `X` can be supplanted by something: we have
    named not just a subterm, but a location in the subject.)
-   The pattern `(a b (:i (d b)))` matches the subject as well. Observe
    how the hole allowed `(d b)` to be sought inside the subterm at the
    location where the hole matched. Note also that the pattern would
    just as easily match the subject `(a b (w x (w y (w z (d b)))))`,
    because it doesn't matter how deep `(d b)` is embedded in the
    subterm.
-   If the pattern included a name, like `(a b (? X (:i (d b))))`, the
    match with the subject would result in the unifier `X=(c (d b))`.
    Likewise, the pattern `(a b (:i (? X (d b))))` would match the
    subject with the unifier `X=(d b)`.
-   The pattern `(a (? X *) (:i (d (? X *))))` also matches the subject,
    with the unifier `X=b`. This is a good example of the expressive
    power of pattern-matching in Treacle: we are basically asking to
    search the contents of the 3rd subterm, for whatever the 2nd subterm
    is.

### Rules ###

-   Say we have a rule where the pattern is `(a b (:i (? X (d b))))`,
    and the lone replacement is `X : a`. This rule would match the
    original subject `(a b (c (d b)))`, unifying with `X=(d b)`, and
    would rewrite the subject to `(a b (c a))`.
-   Or, say our rule's pattern is `(a (? Y *) (:i (? X (d *))))`, and
    the set of replacements is {`X : (? Y)`, `Y : (? X)`}. This rule
    would also match the subject, with a unifier of {`X=(d b)`, `Y=b`},
    and would rewrite the subject to `(a (d b) (c b))`. Again, notice
    the expressivity of this rule: we're basically asking Treacle to
    swap whatever occurs next to the `a`, with whatever occurs alongside
    a `d` somewhere inside the term that occurs next to that.

Mechanism
---------

We can think of the mechanism by which context rewriting is undertaken,
as follows.

We pattern-match "as usual": recursively traverse the pattern and the
subject. Where there are literals in the pattern, we make sure those
same values appear in the subject, in the same place. Where there are
named subpatterns in the pattern, we bind the name to the position in
the subject, and insert that binding into a unifier, before trying to
match the subpattern to that position. (We do an occurs check first, to
make sure that the name isn't already bound to something else.)

Note that we bind the name, not to a subterm in the subject, but to a
*position* in the subject. If you like, you can think of context
rewriting building a "unifier by reference" rather than the rather more
conventional "unifier by value". This is useful, because the presence of
holes means that we will have more of a need to know where we want to
install a replacement.

When we encounter a hole in the pattern, we take the subpattern that
appears in the hole and begin searching for that subpattern in the
subterm of the subject whose position corresponds to the hole. We pass
this subsearch our unifier (so that it can use the variable bindings
already established for occurs checks.) If the subsearch fails to match,
then we also fail to match. If the subsearch succeeds, we continue the
pattern-matching process with the unifier it produced.

If everything succeeds, we have a unifier. We go through the
replacements, look up the name of each replacement in the unifier to
find the location in the subject where it matched, expand all the
variable names in the replacement with the contents of the unifier, and
"poke" the expanded replacement into the subject at the location.

Implementation
--------------

Like Arboretuum, there is a reference implementation of Treacle in
relatively pure Scheme, meant to normatively fill in any gaps in the
description of the language given in this document.

Discussion
----------

You may wonder, why forest-rewriting, or context rewriting? To be sure,
it does not add any computational power to term-rewriting, which is
already Turing-complete. But it does add a significant amount of
expressiveness. While this expressiveness seems to come at a signficant
cost (at least, as imagined in a naïve implementation,) there are two
advantages it might provide, one practical and one theoretical, which
I'll get to in a second.

The idea latent in forest-rewriting, which I didn't explain too well in
the Arboretuum documentation, is to *partition the subject*. Context
rewriting continues and generalizes this idea; while in forest-rewriting
it is obvious what the partitions are (named trees in a forest,) in
context-rewriting, the partitions would be subterms of some given term
(for example, the top-level term.) An engine implementing
context-rewriting might need some supplementary information or deductive
ability in order to "see" and exploit the partitions, but they could
nonetheless be identified.

One major effect of partitioning is to ease the locality constraint. If
you've ever tried programming in pure term-rewriting, you notice that
you have to "keep all your state together": if there are multiple pieces
of information in the tree of terms that relate to the reduction you
want to accomplish, they have to be in a bounded distance from, and in a
fixed relationship with, each other. If some piece is far away, it will
have to be brought to bear on the situation – *literally* brought, by
moving it through the tree through successive "bubbling" rewrites.

Forest-rewriting eases this by having multiple independent trees: some
piece of information can be anywhere in some other tree. Context
rewriting eases it by having holes in which the piece of information can
be found anywhere.

Partitioning the subject could have the practical benefit of improving
locality of reference in the rewriting engine. Each partition can reside
in its own memory buffer which is fixed in some way, for example in one
or more cache lines. Since we don't need to "bubble" information through
the term, each partition can stay in its own cached area, and we should
see fewer cache misses.

Partitioning the subject could also have the theoretical benefit of
making it easier to prove that the rewriting terminates. If you look
through some of the unit tests in `tests.scm`, you might notice that
some of them go to some lengths to avoid rewriting certain trees to
anything larger than they were. The size of each partition is then
monotonically decreasing, and so it will eventually "run out", at which
point the rewriting process must of course terminate. We might not be
able to achieve the ideal case where, on each rewrite, at least one of
the partitions shrinks and the rest stay the same size. The closer we
can come to it, however, the less burdensome should be the task of
proving that the entire system terminates, because many of the cases
should be trivial.

Happy whacky rewriting all sorts of fun ways!  
Chris Pressey  
Chicago, Illinois  
April 12, 2008
