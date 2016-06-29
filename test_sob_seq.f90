program test_sob_seq
  use mod_sob_seq
  type(sobol_state) :: rng
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
    
    
end program test_sob_seq
