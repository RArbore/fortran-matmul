CC=gfortran -g -cpp -O3 -march=native -ftree-vectorize -funroll-loops -ffast-math

matmul: main.f90
	$(CC) $^ -o $@
clean:
	rm matmul
