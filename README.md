# Modern fortran implementation of a Sobol sequence

The [Sobol sequence](https://en.wikipedia.org/wiki/Sobol_sequence) is a [low-discrepancy sequence](https://en.wikipedia.org/wiki/Low-discrepancy_sequence) that can be used for more efficient (Quasi-)Monte-Carlo integration.

This repository contains a module, `mod_sobseq.f90`, which can be used to generate points in Sobol series, in a continuous or strided (2**n) manner.

## How to use
To generate sobol sequences in many dimensions, direction numbers are needed.
A good set of these was made by [Joe and Kuo](http://web.maths.unsw.edu.au/~fkuo/sobol/).
The direction numbers for the first 8 dimensions are reproduced here
```fortran
integer, parameter, dimension(1:8)   :: s = (/1,2,3,3,4,4,5,5/)
integer, parameter, dimension(1:8)   :: a = (/0,1,1,2,1,4,2,4/)
integer, parameter, dimension(5,1:8) :: m = reshape((/1,0,0,0,0, &
					1,3,0,0,0, &
					1,3,1,0,0, &
					1,1,1,0,0, &
					1,1,3,3,0, &
					1,3,5,13,0,&
					1,1,5,5,17,&
					1,1,5,5,5/), (/5,8/))
```

The following code snippet contains a small example, in 1 dimension.
Use 1 QRNG per dimension.
```fortran
! Initialization
call rng%initialize(s(i), a(i), m(:,i))

! Generation
do i=1,n_vars
  out(i) = rng%next()
end do
```


### Strided operation
The module also contains functions for generating numbers in a strided manner, which is useful for parallel operation.
To use this, set n_streams to the 2-logarithm of the number of streams you want (if n_streams is not a power of 2 you might get distribution problems)
and use the rng%next_strided() generator.


## Authors

* Koen Beljaars <k.p.beljaars@tue.nl>
* Daan van Vugt <daanvanvugt@gmail.com>

## License
This work is released under the MIT license. A copy can be found in the file LICENSE in the repository.
