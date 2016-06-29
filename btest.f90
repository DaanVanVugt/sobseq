program test_btest
  use mod_bit_hilo

  integer :: i = 32768 + 1024 + 64 + 2

  write(*,*) btest(i, (/(j, j=0,bit_size(i))/))
  write(*,*) bit_size(j)

  write(*,*) i4_bit_hi1(i)
  write(*,*) minloc((/(j, j=0,bit_size(i))/), &
      btest(i, (/(j, j=bit_size(i),0, -1)/)))
end program test_btest
