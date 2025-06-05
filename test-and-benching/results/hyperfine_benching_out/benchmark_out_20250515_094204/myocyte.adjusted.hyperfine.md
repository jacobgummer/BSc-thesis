## Adjusted Benchmark Results for myocyte.fut (Typechecking Focus)

| Metric                             | New Futhark (futhark-new-2.1) | Old Futhark (futhark-automap-commit) | Unit |
|------------------------------------|---------------------------------|---------------------------------|------|
| Mean 'futhark check' time          | 0.6873976915           | 0.6834927598333332           | s    |
| Mean 'futhark check-syntax' time   | 0.04642570872666667        | 0.04514525126000001        | s    |
| **Adjusted Mean Time (Typechecking)** | **.64097198277333333**    | **.63834750857333319**    | s    |

Winner (based on adjusted typechecking time): Old Futhark (adjusted)

Raw 'futhark check' hyperfine report: [myocyte.hyperfine.md](./myocyte.check.hyperfine.md)

Raw 'futhark check-syntax' hyperfine report: [myocyte.syntax.hyperfine.md](./myocyte.syntax.hyperfine.md)
