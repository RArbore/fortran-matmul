program matmul
  implicit none

  integer, dimension(8) :: seed
  integer :: i
  integer :: j
  real, dimension(1000, 1000) :: a
  real, dimension(1000, 1000) :: b
  real, dimension(1000, 1000) :: c

  seed = 760013
  call random_seed(put = seed)
  do i = 1, 1000
    do j = 1, 1000
      call random_number(a(i, j))
      call random_number(b(i, j))
      print *, a(i, j), b(i, j)
    end do
  end do
  c(:, :) = 0
  

  
end program matmul
