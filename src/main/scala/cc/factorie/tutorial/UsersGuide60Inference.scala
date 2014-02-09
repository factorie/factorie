/*& Inference */
/*&

# Inference

The basic use of a graphical model is to perform _inference_: making
predictions about the values of unobserved variables, conditioned on
the values of observed variables and the parameters.

In FACTORIE terminology, inference is a process which takes a list of
variables and a model and produces a Summary, which is a container of
marginals which also has a marginalization constant, referred to as logZ.

## Main players

The main traits involved in the process of inference in FACTORIE are
Marginal, FactorMarginal, Summary, and Infer.

### Marginal

The main byproduct of inference in FACTORIE are marginals over random
variables. FACTORIE does not specify much in the Marginal trait itself,
only that a Marginal should be over some variables and have a method to
set those variables to their highest-probability values according to those
marginals.

The most common type of Marginal in factorie is the DiscreteMarginal1,
which represents a marginal over a single varying DiscreteVar. To do so it
stores a tensor with the marginal probability of each value of the
variable, in its proportions method. If doing MAP inference this tensor
is a singleton tensor with only one non-zero entry, representing the fact
that this marginal does not represent any uncertainty.

### FactorMarginal

Another equally important byproduct of inference in FACTORIE is marginals
over Factors. These FactorMarginals are mainly used for learning, as the
gradient of all objectives for training graphical models involve a
difference between target and expected sufficient statistics of the factors
under some distribution.

All that the FactorMarginal trait provides is a pointer to Factor whose
marginal it stores and a tensor representing the expected sufficient
statistics of that Factor.

FACTORIE has automatic functionality to build FactorMarginals from simpler
Marginals, in classes such as DiscreteMarginal1Factor2, which represents
a marginal over one discrete variable and a factor marginal over a factor
which touches that variable and another.

### Summary

In FACTORIE a Summary is the main way to obtain Marginals and
FactorMarginals. A Summary is an object which, given a variable or a factor
can produce its Marginal or FactorMarginal. Summaries also store the
normalization constant produced during inference, in its method logZ,
which is necessary for learning.

FACTORIE provides some pre-implemented instances of Summary, most
notably the MAPSummary, which can store the results of MAP inference given
an Assignment and a set of Factors.

### Infer

The main interface for inference-capable objects is the trait Infer,
which defines a single method, infer(), taking a list of variables, a
model, and a preexisting Summary (to deal with variables which are being
marginalized in EM), and produces a Summary.

It is easy to implement your own inferencer. The most convenient way
to do so is by reusing one of the existent types of Summary, such as the
MAPSummary for MAP inference, and filling it in as needed.

### Maximize

Maximize in FACTORIE is a subtype of Infer which represents MAP inference.
It adds no functionality except a maximize() method which calls infer and
then calls setToMaximize on the returned Summary.

## Implemented algorithms

FACTORIE has implementations of many common inference algorithms.

In general most objects whose name start with InferBy are marginal
inference algorithms, and similarly objects with names starting with
MaximizeBy are MAP inference algorithms.

There are many variants belief propagation (BP), for both marginalization and
maximization, including InferByBPChain, InferByBPTree, InferByBPLoopy, and
InferByBPLoopyTreewise. Each of these has an analogous Maximize version.

There is also InferByGibbsSampling, and InferByMeanField, which marginalize
using sampling and a mean-field variational algorithm for discrete variables.

For maximization we also have MaximizeByIteratedConditionalModes, which is
the maximization analogue of Gibbs sampling, and MaximizeByMPLP, which
uses dual coordinate ascent on the LP relaxation of inference.

Finally EM and Dual Decomposition are implemented outside of the inference
API, by the EMInferencer and DualDecomposition classes.

 */