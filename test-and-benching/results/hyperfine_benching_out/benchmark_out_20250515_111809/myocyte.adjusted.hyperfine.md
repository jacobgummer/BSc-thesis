## Adjusted Benchmark Results for myocyte.fut (Typechecking Focus)

| Metric                             | New Futhark (futhark-new-2.0) | Old Futhark (futhark-automap-commit) | Unit |
|------------------------------------|---------------------------------|---------------------------------|------|
| Mean 'futhark check' time          | 0.61414078           | 0.6113585524666667           | s    |
| Mean 'futhark check-syntax' time   | 0.043698019106666666        | 0.043826816639999995        | s    |
| **Adjusted Mean Time (Typechecking)** | **.570442760893333334**    | **.567531735826666705**    | s    |

Winner (based on adjusted typechecking time): Old Futhark (adjusted)

Raw 'futhark check' hyperfine report: [myocyte.hyperfine.md](./myocyte.check.hyperfine.md)

Raw 'futhark check-syntax' hyperfine report: [myocyte.syntax.hyperfine.md](./myocyte.syntax.hyperfine.md)
