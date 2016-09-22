!> Module containing a state type for implementing parallel
!> Sobol series generators with strided and skip-ahead generation.
!> Note that this uses the gray code implementation, so
!> Generated numbers are shuffled compared to the original series.
module mod_sobseq
  implicit none
  private

  integer, parameter :: N_M = 31 ! Generate at most 2^31 points
  ! Problems with sign bit when generating N_M = 32

  !> Type containing the state of a sobol sequence
  type, public :: sobol_state
    private
      integer :: v(N_M)   !< Direction numbers
      integer :: i = 1    !< Current number
      integer :: x = 0    !< Current value
      integer :: stride=0 !< Skip 2^this many values when generating
  contains
      procedure, public :: initialize !< Initialize direction numbers
      procedure, public :: skip_ahead !< Skip ahead to a specific position and return this value
      procedure, public :: next         !< Generate the next value in the sequence
      procedure, public :: next_strided !< Generate the next value in the sequence (strided version)
  end type sobol_state
contains


!> Initialize the direction numbers using a primitive polynomial
subroutine initialize(state, s, a, m_in, stride)
  implicit none
  class (sobol_state), intent(inout) :: state
  integer, intent(in) :: s !< Number of direction numbers / Mathematical polynomial basis of degree s
  integer, intent(in) :: a !< Coefficients of primitive polynomial
  integer, intent(in), dimension(s) :: m_in !< First direction numbers
  integer, intent(in), optional :: stride

  integer, dimension(N_M) :: m
  integer :: k, i, tmp

  m(1:s) = m_in

  do k=s+1, N_M
    tmp=ieor(2**s * m(k-s), m(k-s))
    do i=1,s-1
      tmp = ieor(m(k-i) * 2**i * ai(a, s-i), &
                 tmp)
    end do
    m(k) = tmp
  end do

  do k=1, N_M
    state%v(k) = (2**(N_M-k))*m(k)
  end do

  state%x = 0
  state%i = 0
  state%stride = 0
  if (present(stride)) state%stride = stride
end subroutine initialize


!> Generate a value at a specific position i
function skip_ahead(state, i) result(output)
  implicit none
  class (sobol_state), intent(inout) :: state
  integer, intent(in) :: i
  real    :: output
  integer :: g ! Gray code representation of i
  integer :: j, tmp
  
  g = ieor(i,i/2)
  state%x = 0
  state%i = i
  
  tmp = ai(g,1) * state%v(1)
  do j=2, N_M
    tmp = ieor(tmp,ai(g,j) * state%v(j))
  end do
  output = real(tmp) * 2.d0**(-N_M)
  state%x = tmp
  
end function skip_ahead


!> Generate the next value in a series
!> And update the state function
function next(state)
  implicit none
  class (sobol_state), intent(inout) :: state
  real :: next
  state%x = ieor(state%x, state%v(i4_bit_lo0(state%i)))
  state%i = state%i + 1
  next = real(state%x) * 2.d0**(-N_M)
end function next

!> Generate the next value in a series
!> And update the state function
function next_strided(state)
  implicit none
  class (sobol_state), intent(inout) :: state
  real :: next_strided
  state%x = ieor(state%x, ieor(state%v(state%stride), state%v(&
            i4_bit_lo0(ior(state%i, 2**state%stride - 1)))))
  state%i = state%i + 2**state%stride
  next_strided = real(state%x) * 2.d0**(-N_M)
end function next_strided


!> Returns the value of the bit at position i (1-based index)
function ai(a,i)
implicit none
integer, intent(in) :: a, i
integer :: ai

if (btest(a,i-1)) then
  ai = 1
else
  ai = 0
end if
end function ai

!> Return the position of the lowest (rightmost) 0 bit in a int(4)
function i4_bit_lo0(num)
  implicit none

  integer(kind=4), intent(in) :: num
  integer :: i4_bit_lo0

  do i4_bit_lo0=1,bit_size(num)
    if (.not. btest(num,i4_bit_lo0-1)) return
  enddo
end function i4_bit_lo0
end module mod_sobseq
