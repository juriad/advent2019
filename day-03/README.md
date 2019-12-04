Run as:
```
gcc prg.f90 -lgfortran -o prg && ./prg input
```

Fortran is nice once you get used to it; however these were just the biggest pains:

* Reading from file took much longer to do as it was not possible to read into a allocatable array.
* Uninitialized variables and the need of their declaration at the top caused a lot of mistakes.
* Case insensitivity and shared namespaces between types and variables.

The nice thing however was that dynamic arrays know their sizes.
