test_sob_seq: test_sob_seq.f90 mod_sob_seq.f90 mod_bit_hilo.f90
	gfortran $^ -o $@

test_mod_bit_hilo: test_mod_bit_hilo.f90 mod_bit_hilo.f90
	gfortran $^ -o $@

%.mod: %.f90
	gfortran $< -o $(patsubst %.f90,%.o,$<)
