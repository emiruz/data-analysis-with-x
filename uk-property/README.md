# UK HOUSE PRICING MODEL

## Introduction

In the UK, residential house price sales are public record,
so when a house sells its possible to know what price it
sold for. But what about its price at all other times? If I
 make some simplifying assumptions about how house prices
relate to each other, it can be straightforward to interpolate
the price at all times, just as long as something is selling.

Let $p^t_i$ be the price of property $i$ at time $t$. The key
assumption is that house prices grow in direct proportion to
each other. That is, if house $A$ is worth twice as much as
house $B$, it remains the case at all times. I can then
decompose $p^t_i$ as follows:

$$ p^t_i = \alpha_i\beta_t $$

where $\alpha_i$ is a time invariant, house specific constant,
and $\beta_t$ is a house invariant, time specific constant.
Intuitively, if all prices are proportional, they can be
represented by a constant per house, and a time specific 
multiple.

The associated optimisation problem to fit the $\alpha,\beta$
parameters form data is given by:

$$\underset{\alpha,\beta}{\mathrm{arg\,min}} \sum (x^t_i - \alpha_i\beta_t)^2 $$

where $x^t_i$ is the actual house price at time $t$. Note that
the cost function is convex, so I should be able to fit it
properly with gradient descent.

Note that $t$, is something like a calendar month. As a result,
any given $\beta_t$ applies to many sales Similarly, houses are
often resold, so any given $\alpha_i$ often applies across many
time periods.

For practical purposes, I pick a target property, draw a 2.5km
box around it, and collect the sales history for all properties
in that catchment. This is what's given in `example.tsv`. I've
removed exact address information, since it doesn't matter for
this analysis. For posterity, you can calculate a bounding box
given a target coordinate like this:

```
box :: (Float,Float) -> Float -> (Float,Float,Float,Float)
box (lon,lat) n = (minLon, maxLon, minLat, maxLat)
  where minLon = lon - n  / (111.139 * (cos lat'))
        maxLon = lon + n  / (111.139 * (cos lat'))
        minLat = lat - n  / 111.139
        maxLat = lat + n  / 111.139
        lat'   = lat * pi / 180
```
where `lon`, `lat` and `n` are longitude, latitude and half the
width of the bounding square in kilometres respectively.

Once the equation is fitted, since I then have all $\alpha,\beta$
parameters, $\alpha_i\beta_t$, for all $t$ generates a full time
series of interpolated price.

Here is an example of the output produced using `gnuplot` and the
approximate gradient descent solution. Note, the red dot(s) are
actual paid prices, the gray dots are interpolated prices (one
per month) and the solid line is a cublic spline.

![Interpolated price series example](approx.png)

Here is an example output fitted with the exact algorithm.

![Interpolated price series example](exact.png)


## Implementations

I've coded two implementations: in Haskell, an approximate,
gradient descent based solution using auto-differentiation and
the Adam optimiser, and in Fortran, an exact solution using the
[sgels](https://netlib.org/lapack/explore-html-3.6.1/d0/db8/group__real_g_esolve_gacd49b6b29636a826370633a8856bd3bd.html)
LAPACK dense matrix solver.

### A gradient descent approximation (in Haskell)

This implementation uses an automatic differentiation library,
the optimiser itself is written herein.

Here is a quick sketch of the solution.

1. Write down the optimisation function and use the `ad`
   package to produce its gradient function.

2. Implement the [adamax](https://arxiv.org/abs/1412.6980)
   optimiser to do gradient descent.

3. Parse `example.tsv` into a form that fits the cost function.

4. Use the adaMax function to fit $\alpha,\beta$. Wrange the
   output into TSV.

There is no cabal file or build management for simplicity of 
exposition. Install these before running.

```
cabal install --lib containers random ad deepseq
```

Compile, execute, and plot:

```
ghc -O2 approx.hs
./approx > interpolated.tsv
gnuplot plot.gp > approx.png
```

The example has a 2.5km half width catchment, which is large.
It takes about 5 minutes to calculate the optimisation problem
for >60k data points and >34k parameters.

### Exact solution (in Fortran)

The model $p^t_i = \alpha_i\beta_t$ can be linearised to 
$log(p^t_i) = log(\alpha_i) + log(\beta_t)$ and then solved
using a standard dense solver from LAPACK since a 60k x 34k
matrix is still small enough to fit into memory given that I 
use 32-bit reals. Since the problem is over-determined I use
the [sgels](https://netlib.org/lapack/explore-html-3.6.1/d0/db8/group__real_g_esolve_gacd49b6b29636a826370633a8856bd3bd.html)
routine. 

Compile, execute and plot:
```
gfortran -O3 -o exact exact.f90 -llapack
./exact > interpolated.tsv
gnuplot plot.gp > exact.png
```
The solver executes in just under 9 minutes, using all 4 cores
on my laptop.

## Notes

* The Haskell approximate model converges on a variety of data,
but several things were necessary to facilitate this: (1) using
`Double` instead of `Float` to avoid division by zero due to
round, or adding a small constant to all divisions, (2)
normalising prices to a range of zero to 1 so that variables
could be initialised to random values in the same range, and
(3) a minimum gradient no smaller than `1e-4` since it either
hits saddle points at that level or a precision limit.

* The Fortan exact solution takes significantly longer to complete
(9min vs 5min) and it is much more computationally expensive but
the difference in precision is very significant even by eye
(see graphs above).

* I've left out some pre-processing steps -- namely connecting
lon/lat coordinates to postcodes -- as not to detract from the
analysis. They are straightforward -- I do them directly in SQL.


## Acknowledgements

Thank you to the members of the Haskell community, who kindly
helped a neophyte like me muddle through some of Haskell's
idiosyncracies.
