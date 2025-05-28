# benchmark_out_20250508_133519
## Used binaries
`futhark_new`: `futhark-new-2` (maybe 2.0 ?)

`futhark_old`: `futhark-automap`
## Notes
Debugging was enabled during this test run which could have influenced the runtime.
`futhark-automap` seems to be a faulty version of the implementation (might be because renaming of files has been done incorrectly at compile time).
Run on all files in `futhark-benchmarks`.

# benchmark_out_20250514_155005
## Used binaries
`futhark_new`: `futhark-new-2.1`

`futhark_old`: `futhark-automap`
## Notes
Faulty `futhark-automap` used.

# benchmark_out_20250514_163751
## Used binaries
`futhark_new`: `futhark-new-2.1`

`futhark_old`: `futhark-automap`
## Notes
Faulty `futhark-automap` used.

# benchmark_out_20250515_085746
## Used binaries
`futhark_new`: `futhark-new-2.1`

`futhark_old`: `futhark-automap-branch`
## Notes
`futhark-automap-branch` used.
Shows, that our new implementation isn't measureably faster when doing `futhark check`.

# benchmark_out_20250515_090421
## Used binaries
`futhark_new`: `futhark-new-2.1`

`futhark_old`: `futhark-automap-commit`
## Notes
`futhark-automap-commit` used.
Shows, that our new implementation isn't measureably faster when doing `futhark check`.

# benchmark_out_20250515_093335
## Used binaries
`futhark_new`: `futhark-new-2.1`

`futhark_old`: `futhark-automap-commit`
## Notes
Now corrects for `check-syntax`.
Shows small win for new implementation (tie).

# benchmark_out_20250515_094204
## Used binaries
`futhark_new`: `futhark-new-2.1`

`futhark_old`: `futhark-automap-commit`
## Notes
Same as previous, with some minor formatting changes in the script.
Shows small win for old implementation (tie).

# benchmark_out_20250515_110750
## Used binaries
`futhark_new`: `futhark-new-0.0`

`futhark_old`: `futhark-automap-commit`
## Notes
Shows win for old implementation.

# benchmark_out_benchmark_out_20250515_110958
## Used binaries
`futhark_new`: `futhark-new-1.0`

`futhark_old`: `futhark-automap-commit`
## Notes
Big improvement in the `occursCheck` in the new implementation improved runtime substantially.
Shows minor win for old implementation (tie).

# benchmark_out_20250515_111226
## Used binaries
`futhark_new`: `futhark-new-1.1`

`futhark_old`: `futhark-automap-commit`
## Notes
Small improvement in the `occursCheck` in the new implementation did not measureably improve runtime.
Shows minor win for old implementation (tie).

# benchmark_out_20250515_111615
## Used binaries
`futhark_new`: `futhark-new-2.0`

`futhark_old`: `futhark-automap-commit`
## Notes
Improvement regarding `scopeCheck` in the new implementation did not measureably improve runtime.
Shows minor win for old implementation (tie).

# benchmark_out_20250515_111618
## Used binaries
`futhark_new`: `futhark-new-2.0`

`futhark_old`: `futhark-automap-commit`
## Notes
Rerun of previous, due to large range.
Improved statistical security in results.