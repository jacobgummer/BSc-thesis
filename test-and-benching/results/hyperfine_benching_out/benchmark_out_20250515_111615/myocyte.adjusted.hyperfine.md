## Adjusted Benchmark Results for myocyte.fut (Typechecking Focus)

| Metric                             | New Futhark (futhark-new-2.0) | Old Futhark (futhark-automap-commit) | Unit |
|------------------------------------|---------------------------------|---------------------------------|------|
| Mean 'futhark check' time          | 0.6598988936666665           | 0.6338053502666666           | s    |
| Mean 'futhark check-syntax' time   | 0.04780920722666667        | 0.041539757026666665        | s    |
| **Adjusted Mean Time (Typechecking)** | **.61208968643999983**    | **.592265593239999935**    | s    |

Winner (based on adjusted typechecking time): Old Futhark (adjusted)

Raw 'futhark check' hyperfine report: [myocyte.hyperfine.md](./myocyte.check.hyperfine.md)

Raw 'futhark check-syntax' hyperfine report: [myocyte.syntax.hyperfine.md](./myocyte.syntax.hyperfine.md)
