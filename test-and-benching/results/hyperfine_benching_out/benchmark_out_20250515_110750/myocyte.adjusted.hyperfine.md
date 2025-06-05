## Adjusted Benchmark Results for myocyte.fut (Typechecking Focus)

| Metric                             | New Futhark (futhark-new-0.0) | Old Futhark (futhark-automap-commit) | Unit |
|------------------------------------|---------------------------------|---------------------------------|------|
| Mean 'futhark check' time          | 0.74902397104           | 0.6448240985066666           | s    |
| Mean 'futhark check-syntax' time   | 0.04439730548666667        | 0.04344954188666666        | s    |
| **Adjusted Mean Time (Typechecking)** | **.70462666555333333**    | **.60137455661999994**    | s    |

Winner (based on adjusted typechecking time): Old Futhark (adjusted)

Raw 'futhark check' hyperfine report: [myocyte.hyperfine.md](./myocyte.check.hyperfine.md)

Raw 'futhark check-syntax' hyperfine report: [myocyte.syntax.hyperfine.md](./myocyte.syntax.hyperfine.md)
