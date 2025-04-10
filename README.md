## Machine Learning in Haskell

Haskell library implementing foundamental machine learning algorithms.
Developed for the "Introduction to Machine Learning" course at the University
of Trento (2024/2025).

### Walkthrough

Each algorithm implemented in this library is organized into 3 files:

- `src/AlgorithmName.hs`: implementation code
- `theory/AlgorithmName.md`: mathematical derivation, explanations, proofs
- `example/algorithm-name.hs`: usage example

To run the examples:

```bash
cabal run algorithm-name
```

### Core Foundations

**Mathematical Framework for ML** \
Formal definitions and theoretical basis for all implementations \
[Source](src/Model.hs) • [Theory](theory/Model.md)  


**Polynomial Regression** \
Non-linear regression through polynomial basis expansion \
[Source](src/PolyRegr.hs) • [Theory](theory/PolyRegr.md) • [Example](examples/poly-regr.hs)

**k-Nearest Neighbors** \
Non parametric classifier \
[Source](src/KNN.hs) • [Theory](theory/KNN.md) • [Example](examples/knn.hs)
