## Adjusted Benchmark Results for myocyte.fut (Typechecking Focus)

| Metric                             | New Futhark (futhark-new-1.0) | Old Futhark (futhark-automap-commit) | Unit |
|------------------------------------|---------------------------------|---------------------------------|------|
| Mean 'futhark check' time          | 0.6172578130266667           | 0.61123943256           | s    |
| Mean 'futhark check-syntax' time   | 0.04699965328000001        | 0.04434759534666667        | s    |
| **Adjusted Mean Time (Typechecking)** | **.57025815974666669**    | **.56689183721333333**    | s    |

Winner (based on adjusted typechecking time): Old Futhark (adjusted)

Raw 'futhark check' hyperfine report: [myocyte.hyperfine.md](./myocyte.check.hyperfine.md)

Raw 'futhark check-syntax' hyperfine report: [myocyte.syntax.hyperfine.md](./myocyte.syntax.hyperfine.md)
