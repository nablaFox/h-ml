## Machine Learning in Haskell

A Haskell library implementing foundamental machine learning algorithms, paired
with comprehensive theoretical explanations. Developed for the "Introduction to
Machine Learning" course at the University of Trento (2024/2025).

### Documentation Strategy

Each `src/Algorithm.hs` is paired with `theory/Algorithm.md`, cotaining:

- **Practical Usage**: Concrete examples demonstrating the implementation's API
- **Theoretical Foundation**: Mathematical derivation of the algorithm

### Core Foundations

**Mathematical Framework for ML**  
[Model.hs](src/Model.hs) • [Theory](theory/Model.md)  
*Formal definitions and theoretical basis for all implementations*  

**Polynomial Regression**  
[PolyRegr.hs](src/PolyRegr.hs) • [Theory](theory/PolyRegr.md)  
*Non-linear regression through polynomial basis expansion*  

**k-Nearest Neighbors**  
[KNN.hs](src/KNN.hs) • [Theory](theory/KNN.md)  
*Instance-based learning with distance-weighted voting*
