#define SIZE 1024

program matmul
  implicit none

  integer, dimension(8) :: seed
  integer :: i, j
  integer :: before, after, rate, max
  real, dimension(SIZE, SIZE) :: a, b, c

  call system_clock(count_max = max, count_rate = rate)
  seed = 760013
  call random_seed(put = seed)
  do i = 1, SIZE
     do j = 1, SIZE
        call random_number(a(i, j))
        call random_number(b(i, j))
     end do
  end do
  
  call system_clock(before)
  c = matmul_func(a, b)
  call system_clock(after)

  print *, "Time: ", real(after - before) / real(rate)
  
contains

function matmul_func(a, b) result(c)
  implicit none

  integer :: i, j, k
  real, dimension(SIZE, SIZE), intent(in) :: a, b
  real, dimension(SIZE, SIZE) :: c

  c = 0
  do j = 1, SIZE
     do k = 1, SIZE
        do i = 1, SIZE
           c(i, j) = c(i, j) + a(i, k) * b(k, j)
        end do
     end do
  end do

end function matmul_func

end program matmul
