# DATA ANALYSIS WITH HASKELL

A repository of adhoc analyses using the programming
language Haskell.

## Rationale

I'm a fan of proof oriented languages like Prolog and
Haskell because I think they are the closest thing 
we have to a "language for thought". Even so, it is
uncommon to see analyses in these languages outside
of finance, so I was compelled to produce some examples
for posterity.

The common complaint is library support, so part of
what I aim to show is that if you pay attention to what
you're doing when performing analyses, there is often
not much you need that you cannot write yourself quickly.
For example, the first analysis committed herein was a
[UK property price model](uk-property/). It features a
tricky regression problem solved using differentiable
programming. Yet all the code needed including a custom
input parser and optimiser is just 55 lines of Haskell.
More generally, I'll aim to show that armed with basics
like autodiff, we can largely bootstrap outselves.

## Contents

[UK property price model](uk-property/) -- A model to 
interpolate the full price series for a UK property from
just public price sold information.
