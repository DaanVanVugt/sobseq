program test_sobseq
  use mod_sobseq
  type(sobol_state) :: rng, rng2
  integer :: i
  integer, parameter :: s=1, a=0, m(1) = (/1/)
  integer, parameter :: N_samples = 1000000
  real :: t0, t1
  real, dimension(:), allocatable :: tmp
  allocate(tmp(N_samples))

  write(*,*) "Skip-ahead"
  call rng%initialize(s,a,m)
  do i=1,17
    write(*,*) i, rng%skip_ahead(i)
  enddo

  write(*,*) "Next"
  call rng%initialize(s,a,m)
  do i=1,17
    write(*,*) i, rng%next()
  enddo

  write(*,"(A,i10,A)",advance="no") "Testing time skip_ahead (", N_samples, "): "
  call rng%initialize(s,a,m)
  call cpu_time(t0)
  do i=1,N_samples
    tmp(i) = rng%skip_ahead(i)
  enddo
  call cpu_time(t1)
  write(*,*) (t1-t0)/N_samples, sum(tmp)/N_samples
    

  write(*,"(A,i10,A)",advance="no") "Testing time next (", N_samples, "): "
  call rng%initialize(s,a,m)
  call cpu_time(t0)
  do i=1,N_samples
    tmp(i) = rng%next()
  enddo
  call cpu_time(t1)
  write(*,*) (t1-t0)/N_samples, sum(tmp)/N_samples


  write(*,"(A,i10,A)") "Testing time next stride 1 (", N_samples, "): "
  call rng%initialize(s,a,m, stride=1)
  write(*,*) 0, rng%skip_ahead(0)
  call rng2%initialize(s,a,m, stride=1)
  write(*,*) 1, rng2%skip_ahead(1)
  do i=2,17
    if (mod(i,2) == 0) then
      write(*,*) i, rng%next()
    else
      write(*,*) i, rng2%next()
    endif
  enddo

    
    
end program test_sobseq
