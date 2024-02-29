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

The associated optimisation problem which I can use to fit the
$\alpha,\beta$ parameters is as follows:

$$\argmin_{\alpha,\beta} \sum (x^t_i - \alpha_i\beta_t)^2 $$

Note that the cost function is convex, so I should be able to
fit it properly with gradient descent.

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
parameters, for any given property $i$, $\alpha_i\beta_t$, for
all $\t$ generates a full time series of its interpolated price.

Here is an example of the output produced using `gnuplot` as
follows:

```
gnuplot plot.gp > example.png
``` 

![Interpolated price series example](example.png)


## Implementation

The implementation is in Haskell using just an automatic
differentiation library. That is, everything else is written
from scratch in about 55 lines of code (LoC).

Here is a quick sketch of the solution.

1. Write down the optimisation function and use the `ad`
   package to produce its gradient function.

2. Implement the adamax optimiser to do gradient descent.

3. Parse `example.tsv` into a form that fits the cost function.

4. Use the adaMax function to fit $\alpha,\beta$. Wrange the
   output into TSV.

## Dependencies

There is no cabal file or build management for simplicity of 
exposition. Install these before running.

```
cabal install --lib containers ad deepseq
```

## Running it

Compile, execute, and plot:

```
ghc -O2 analysis.hs
./analysis > interpolated.tsv
gnuplot plot.gp > example.png
```
