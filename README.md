## Machine Learning in Haskell

Haskell library implementing foundamental machine learning algorithms.
Developed for the "Introduction to Machine Learning" course at the University
of Trento (2024/2025).

### Walkthrough

Each algorithm implemented in this library is organized into 3 files:

- `src/Algorithm.hs`: implementation code
- `example/Algorithm.hs`: usage example
- `theory/Algorithm.md`: mathematical derivation, explanations, proofs

To run the examples:

```bash
cabal run Algorithm
```

### Core Foundations

**Mathematical Framework for ML** \
Formal definitions and theoretical basis for all implementations \
[Source](src/Model.hs) • [Theory](theory/Model.md)  


**Polynomial Regression** \
Non-linear regression through polynomial basis expansion \
[Source](src/PolyRegr.hs) • [Theory](theory/PolyRegr.md) • [Example](examples/PolyRegr.hs)

**k-Nearest Neighbors** \
Non parametric classifier \
[Source](src/KNN.hs) • [Theory](theory/KNN.md) • [Example](examples/KNN.hs)
