#define SIZE 2048
#define CUTOFF 1024

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
  c = matmul_func(a, b, SIZE, SIZE / 2)
  call system_clock(after)

  print *, "Time: ", real(after - before) / real(rate)
  
contains

recursive function matmul_func(a, b, n, h) result(c)
  implicit none

  integer :: i, j, k, hp, hh
  integer, intent(in) :: n, h
  real, dimension(n, n), intent(in) :: a, b
  real, dimension(h, h) :: a11, a12, a21, a22, b11, b12, b21, b22
  real, dimension(n, n) :: c
  hp = h + 1
  hh = h / 2

  if (n < CUTOFF) then
     c = 0
     do j = 1, n
        do k = 1, n
           do i = 1, n
              c(i, j) = c(i, j) + a(i, k) * b(k, j)
           end do
        end do
     end do
  else
     a11 = a(1:h, 1:h)
     a12 = a(1:h, hp:n)
     a21 = a(hp:n, 1:h)
     a22 = a(hp:n, hp:n)
     b11 = b(1:h, 1:h)
     b12 = b(1:h, hp:n)
     b21 = b(hp:n, 1:h)
     b22 = b(hp:n, hp:n)
     c(1:h, 1:h) = matmul_func(a11, b11, h, hh) + matmul_func(a12, b21, h, hh)
     c(1:h, hp:n) = matmul_func(a11, b12, h, hh) + matmul_func(a12, b22, h, hh)
     c(hp:n, 1:h) = matmul_func(a21, b11, h, hh) + matmul_func(a22, b21, h, hh)
     c(hp:n, hp:n) = matmul_func(a22, b12, h, hh) + matmul_func(a22, b22, h, hh)
  end if

end function matmul_func

end program matmul
