!   This file is part of fortran-matmul.
!   fortran-matmul is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   any later version.
!   fortran-matmul is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!   You should have received a copy of the GNU General Public License
!   along with fortran-matmul. If not, see <https://www.gnu.org/licenses/>.

#define SIZE 8192
#define CUTOFF 512

program matmul
  use omp_lib
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
  c = matmul_func(a, b, SIZE, SIZE / 2, 0)
  call system_clock(after)

  print *, "Time: ", real(after - before) / real(rate)
  
contains

recursive function matmul_func(a, b, n, h, d) result(c)
  implicit none

  integer :: i, j, k, hp, hh, dp
  integer, intent(in) :: n, h, d
  real, dimension(n, n), intent(in) :: a, b
  real, dimension(h, h) :: a11, a12, a21, a22, b11, b12, b21, b22, m1, m2, m3, m4, m5, m6, m7
  real, dimension(n, n) :: c
  hp = h + 1
  hh = h / 2
  dp = d + 1

  if (n < CUTOFF) then
     c = 0
     !$OMP PARALLEL
     !$OMP DO
     do j = 1, n
        do k = 1, n
           do i = 1, n
              c(i, j) = c(i, j) + a(i, k) * b(k, j)
           end do
        end do
     end do
     !$OMP END DO
     !$OMP END PARALLEL
  else
     !$OMP PARALLEL
     !$OMP SECTIONS
     !$OMP SECTION
     a11 = a(1:h, 1:h)
     !$OMP SECTION
     a12 = a(1:h, hp:n)
     !$OMP SECTION
     a21 = a(hp:n, 1:h)
     !$OMP SECTION
     a22 = a(hp:n, hp:n)
     !$OMP SECTION
     b11 = b(1:h, 1:h)
     !$OMP SECTION
     b12 = b(1:h, hp:n)
     !$OMP SECTION
     b21 = b(hp:n, 1:h)
     !$OMP SECTION
     b22 = b(hp:n, hp:n)
     !$OMP END SECTIONS
     !$OMP END PARALLEL
     !$OMP PARALLEL
     !$OMP SECTIONS
     !$OMP SECTION
     m1 = matmul_func(a11 + a22, b11 + b22, h, hh, dp)
     !$OMP SECTION
     m2 = matmul_func(a21 + a22, b11, h, hh, dp)
     !$OMP SECTION
     m3 = matmul_func(a11, b12 - b22, h, hh, dp)
     !$OMP SECTION
     m4 = matmul_func(a22, b21 - b11, h, hh, dp)
     !$OMP SECTION
     m5 = matmul_func(a11 + a12, b22, h, hh, dp)
     !$OMP SECTION
     m6 = matmul_func(a21 - a11, b11 + b12, h, hh, dp)
     !$OMP SECTION
     m7 = matmul_func(a12 - a22, b21 + b22, h, hh, dp)
     !$OMP END SECTIONS
     !$OMP END PARALLEL
     !$OMP PARALLEL
     !$OMP SECTIONS
     !$OMP SECTION
     c(1:h, 1:h) = m1 + m4 - m5 + m7
     !$OMP SECTION
     c(1:h, hp:n) = m3 + m5
     !$OMP SECTION
     c(hp:n, 1:h) = m2 + m4
     !$OMP SECTION
     c(hp:n, hp:n) = m1 - m2 + m3 + m6
     !$OMP END SECTIONS
     !$OMP END PARALLEL
  end if

end function matmul_func

end program matmul
