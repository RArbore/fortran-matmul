program matmul
  implicit none

  integer, dimension(8) :: seed
  integer :: i, j, k
  integer :: before, after, rate, max
  real, dimension(1000, 1000) :: a, b, c

  call system_clock(count_max = max, count_rate = rate)
  seed = 760013
  call random_seed(put = seed)
  do i = 1, 1000
     do j = 1, 1000
        call random_number(a(i, j))
        call random_number(b(i, j))
     end do
  end do
  
  call system_clock(before)
  do i = 1, 1000
     do j = 1, 1000
        c(i, j) = 0
        do k = 1, 1000
           c(i, j) = c(i, j) + a(i, k) * b(k, j)
        end do
     end do
  end do
  call system_clock(after)

  print *, "Time: ", real(after - before) / real(rate)
  
end program matmul
