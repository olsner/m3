module lib::gmp;

type mpz = struct { int alloc; int size; [void] limbs; };

extern "C" void mpz_mul_2exp([mpz], [mpz], int);
extern "C" void mpz_init([mpz]);
extern "C" void mpz_init_set_ui([mpz], int);
