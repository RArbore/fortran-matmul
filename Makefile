CC=gfortran -g -cpp -Ofast -march=native -ftree-vectorize -funroll-loops -ffast-math -fopenmp -fmax-stack-var-size=100000000

matmul: main.f90
	$(CC) $^ -o $@
clean:
	rm matmul
