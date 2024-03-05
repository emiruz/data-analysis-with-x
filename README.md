# DATA ANALYSIS WITH HASKELL

A repository of adhoc analyses and algorithmic
demonstrators using the programming language Haskell.


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

More generally, I'll aim to show that given a few basic
ingredients, we can effectively bootstrap outselves.
I'll try to keep the lines of code and the Haskell
vocabulary minimal whilst maintaining a reasonable
performance.


## Contents

[Sparse non-linear regression](uk-property/) -- A model
to interpolate the price series for a UK property from
public price paid data by setting up a large sparse
non-linear regression. `regression`, `sparse`, 
`parameter-estimation`, `autodiff`, `optimisation`

[Jointly learning transformations and fitting a regression](joint-regression/)
-- A 5 parameter non-linear pipeline composed of a two
Yeo-Johnson transforms, a sigmoid function which bisects
the model, and a linear regression. It is jointly fitted
using "threshold accepting" annealing which is implemented
herein. `feature-learning`, `regression`,
`global-optimisation`, `parameter-estimation`.
