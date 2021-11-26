CC=gfortran -Ofast -cpp

matmul: main.f90
	$(CC) $^ -o $@
clean:
	rm matmul
