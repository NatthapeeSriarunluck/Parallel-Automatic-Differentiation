# Parallel-Automatic-Differentiation

## Overview
This project is part of the ICCS311: Functional and Parallel Programming course and focuses on implementing both forward and reverse mode automatic differentiation. The goal is to efficiently compute derivatives of mathematical expressions using both sequential and parallel processing techniques.

## Interactive Mode
The program will prompt you to enter:

Variable names (comma separated)
An expression to differentiate
Corresponding variable values (comma separated). Negative values aren't yet properly supported

## Expressions
Currently, the project supports these expressions:
-  Basic arithmetics: + - * / 
- Trigonometric functions: sin, cos, tan, sec, csc, cot, arcsin, arccos, arctan, arcsec, arccsc, arccot 

Planned to expand to:
- Exponential (e)

## Architecture
### Expression Hierarchy
The core of the project is the Expression trait, which is extended by various case classes representing different mathematical operations (e.g., Sum, Prod, Power, Sin, Cos, etc.). Each case class implements methods for both forward and reverse mode differentiation.

### Parallel Execution
Parallel execution is achieved using Scala's concurrency primitives, such as Future and Await. This allows the differentiation computations to be distributed across multiple threads, leveraging multi-core processors for improved performance.

### Performance Comparison
The performance of sequential and parallel executions is compared by measuring the time taken to compute derivatives. The results are displayed alongside the computed derivatives for easy comparison.
