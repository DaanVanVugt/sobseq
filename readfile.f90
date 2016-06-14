PROGRAM makem

  implicit none
  INTEGER, parameter   :: dim_num = 3, N_M_MAX = 5
  integer              :: i,j,tmp,k
  INTEGER              :: d(dim_num),s(dim_num),a(dim_num)
  integer, allocatable, dimension(:,:) :: m
  
  open (unit=99, file='new-joe-kuo-6.21201.txt', status='old', action='read')
      read(99,*)
  DO i = 1, dim_num
       read(99,*) d(i),s(i),a(i)
  END DO
  rewind(99)
  allocate(m(N_M_MAX,dim_num))
  read(99,*)
  
  DO i = 1, dim_num
       read(99,*) d(i),s(i),a(i),m(1:s(i),i)
  END DO
  
  Close(unit=99)
  
  DO j = 1, dim_num 
  DO k=(s(j)+1), N_M_MAX
    tmp=ieor(2**s(j)*m(k-s(j),j), m(k-s(j),j))
    DO i = k - s(j) + 1, k - 1
      tmp = ieor(m(i,j) * 2**(k-i) * ai(a(j), k-i), &
                 tmp)
    END DO
    m(k,j) = tmp
  END DO
  END DO
  
   write(*,*) s
   write(*,*) "next line"
   write(*,"(32I3)") m
contains
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
END
  
  