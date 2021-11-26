CC=gfortran

matmul: main.f90
	$(CC) $^ -o $@
clean:
	rm matmul
