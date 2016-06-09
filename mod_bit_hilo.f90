module mod_bit_hilo
contains
function i4_bit_hi1(num)
  implicit none

  integer(kind=4), intent(in) :: num
  integer :: i4_bit_hi1

  do i4_bit_hi1=bit_size(num),1,-1
    if (btest(num,i4_bit_hi1-1)) return
  enddo
end function i4_bit_hi1

function i4_bit_lo0(num)
  implicit none

  integer(kind=4), intent(in) :: num
  integer :: i4_bit_lo0

  do i4_bit_lo0=1,bit_size(num)
    if (.not. btest(num,i4_bit_lo0-1)) return
  enddo
end function i4_bit_lo0
end module mod_bit_hilo
