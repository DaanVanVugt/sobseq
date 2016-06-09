program test_mod_bit_hilo
  use mod_bit_hilo
  integer :: i
  integer :: hilo_ref(0:17)

  write(*,*) "Test i4_bit_hi1"
  hilo_ref = (/0,1,2,2, &
    3,3,3,3, 4,4,4,4,4,4,4,4, &
    5,5/)
  do i=0,17
    call write_test_line(i,hilo_ref(i),i4_bit_hi1(i))
  enddo


  write(*,*) "Test i4_bit_lo0"
  hilo_ref = (/1,2,1,3,1,2,1,4,&
    1,2,1,3,1,2,1,5,1,2/)
  do i=0,17
    call write_test_line(i,hilo_ref(i),i4_bit_lo0(i))
  enddo
  
contains
subroutine write_test_line(num,ref,hilo)
  implicit none

  integer,intent(in) :: num, ref, hilo
  write(*,"(i6,b12,i6,i6)") num, num, ref, hilo
end subroutine write_test_line
end program test_mod_bit_hilo
