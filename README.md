# DATA ANALYSIS WITH X, where $x \in {prolog, haskell, fortran, c, ...}$

A repository of adhoc analyses and algorithmic
demonstrators using various programming languages.

## Rationale

I'm a fan of proof oriented languages like Prolog and
Haskell because I think they are the closest thing 
we have to a "language for thought". Even so, it is
uncommon to see analyses in these languages outside
of finance, so I was compelled to produce some examples
for posterity.

I'm also a fan of more specialised programming languages
like Fortran, which I had just learned at the time of
writing to give the [1BRC](https://github.com/emiruz/1brc)
challenge a go. I was amazed by how easy it is to pick
up and produce high performance code with, so I'm very
excited to see what uses I can find for it.

The most buoyant turn-key eco-systems for ML and
statistics are centred on Python, R and Julia. Outside
of these, the common complaint is library support, so
part of what I aim to show is that if we pay attention
to what we are doing when writing algorithms and
performing analyses, there is often not much we need
that we cannot quickly write yourselves.

## Contents

[Sparse non-linear regression](uk-property/) -- A model
to interpolate the price series for a UK property from
public price paid data by setting up a large sparse
non-linear regression. `haskell`, `regression`,
`sparse`, `parameter-estimation`, `autodiff`,
`optimisation`

[Jointly learning transformations and fitting a regression](joint-regression/)
-- A 5 parameter non-linear pipeline composed of a two
Yeo-Johnson transforms, a sigmoid function (which bisects
the model), and a linear regression. It is jointly fitted
using "threshold accepting" annealing (implemented
herein). `haskell`, `feature-learning`, `regression`,
`global-optimisation`, `parameter-estimation`.

Convolutional logistic regression -- Next up! 
Classification of digits from the
[MNIST 1D](https://github.com/greydanus/mnist1d) dataset
by jointly fitting convolutional filters and a logistic
regression model using the thresholding accepting
algorithm. `fortran`, `feature-learning`,
`classification`, `autodiff`
