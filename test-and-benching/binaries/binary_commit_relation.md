# futhark-automap
## futhark-automap 
Repo: jakobgummer/futhark

Branch: automap-tysolve-optim 

Commit: 63894b7
### Notes
Not sure why this was chosen?
Must be because of the (re)naming of the files resulting in the original automap implementaion being used.


# futhark-new
## futhark-new-0.0
Repo: jakobgummer/futhark

Branch: automap-tysolve-optim

Commit: 5f615bc

### Notes
`convertUF` was being used inside the `occursCheck` function.


## futhark-new-1.0
Repo: jakobgummer/futhark

Branch: automap-solve-optim

Commit: 97c05eb

### Notes
`convertUF` has been removed, which should improve performance.


## futhark-new-1.1
Repo: jakobgummer/futhark

Branch: automap-tysolve-optim

Commit: a1dc346 

### Notes
A small optimisation was made to the `occursCheck`function.



## futhark-new-2.0
Repo: jakobgummer/futhark

Branch: automap-tysolve-optim

Commit: 03dd601

### Notes
`scopeCheck`is done only inside the `bindTyVar` function. This created wrong solutions (`tests/types/level1.fut`), but might have better performance than `futhark-new-2.1`


## futhark-new-2.1
Repo: jakobgummer/futhark

Branch: automap-tysolve-optim

Commit: 3e4fc08

### Notes
A `scopeCheck` call has now been added in the `solveTyVar` function to fix the bug when checking `tests/types/level1.fut`, but might have worse performance.



# _prof binaries
Binaries appended `_prof` has been compiled from the repo, branch and commit as the binary not appended with `_prof`.