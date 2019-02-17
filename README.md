# Modern fortran implementation of a Sobol sequence

The [Sobol sequence](https://en.wikipedia.org/wiki/Sobol_sequence) is a [low-discrepancy sequence](https://en.wikipedia.org/wiki/Low-discrepancy_sequence) that can be used for more efficient (Quasi-)Monte-Carlo integration.

This repository contains a module, `mod_sobseq.f90`, which can be used to generate points in Sobol series, in a continuous or strided (2**n) manner.

## How to use
To generate sobol sequences in many dimensions, direction numbers are needed.
A good set of these was made by [Joe and Kuo](http://web.maths.unsw.edu.au/~fkuo/sobol/).
The direction numbers for the first eight are reproduced here, and more can be found at the link above.
```fortran
integer, parameter, dimension(1:12)   :: s = (/1,2,3,3,4,4,5,5,5,5,5,5/)
integer, parameter, dimension(1:12)   :: a = (/0,1,1,2,1,4,2,4,7,11,13,14/)
integer, parameter, dimension(5,1:12) :: m = reshape((/1,0,0,0,0, &
					1,3,0,0,0, &
					1,3,1,0,0, &
					1,1,1,0,0, &
					1,1,3,3,0, &
					1,3,5,13,0,&
					1,1,5,5,17,&
					1,1,5,5,5,&
					1,1,7,11,19,&
					1,1,5,1,1,&
					1,1,1,3,11,&
					1,3,5,5,31/), (/5,12/))
```

The following code snippet contains a small example.
```fortran
integer, parameter :: n_dim=9
integer, parameter :: n_samples=200
type(sobol_state), dimension(n_dim) :: rng
integer :: i, j
real :: u(n_dim)

! Initialization
do i=1,n_dim
  call rng%initialize(s(i), a(i), m(:,i))
end do

! Generation
do j=1,n_samples
  do i=1,n_dim
    u(i) = rng(i)%next()
  end do

  ! do something with u
end do
```


### Strided operation
The module also contains functions for generating numbers in a strided manner, which is useful for parallel operation.
To use this, set n_streams to the 2-logarithm of the number of streams you want (if n_streams is not a power of 2 you might get distribution problems)
and use the rng%next_strided() generator.


## Authors

* Daan van Vugt <daanvanvugt@gmail.com>
* Koen Beljaars <k.p.beljaars@tue.nl>

## License
This work is released under the MIT license. A copy can be found in the file LICENSE in the repository.
