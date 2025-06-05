## Adjusted Benchmark Results for myocyte.fut (Typechecking Focus)

| Metric                             | New Futhark (futhark-new-2.1) | Old Futhark (futhark-automap-commit) | Unit |
|------------------------------------|---------------------------------|---------------------------------|------|
| Mean 'futhark check' time          | 0.6193837616000001           | 0.6247467192666666           | s    |
| Mean 'futhark check-syntax' time   | 0.04489436122        | 0.04207556868666666        | s    |
| **Adjusted Mean Time (Typechecking)** | **.5744894003800001**    | **.58267115057999994**    | s    |

Winner (based on adjusted typechecking time): New Futhark (adjusted)

Raw 'futhark check' hyperfine report: [myocyte.hyperfine.md](./myocyte.hyperfine.md)
Raw 'futhark check-syntax' hyperfine report: [myocyte.syntax.hyperfine.md](./myocyte.syntax.hyperfine.md)
