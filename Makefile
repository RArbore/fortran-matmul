CC=gfortran -g -cpp -Ofast -march=native -ftree-vectorize -funroll-loops -ffast-math -fmax-stack-var-size=100000000 -fopenmp

matmul: main.f90
	$(CC) $^ -o $@
clean:
	rm matmul
