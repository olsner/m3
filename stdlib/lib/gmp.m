module lib::gmp;

type mpz = struct { int alloc; int size; [void] limbs; };
type mpz_t = [mpz];

extern "C" void mpz_init([mpz]) = "__gmpz_init";
extern "C" void mpz_init_set_ui([mpz], int);
extern "C" void mpz_add([mpz], [mpz], [mpz]);
extern "C" void mpz_mul_2exp([mpz], [mpz], int);
extern "C" void mpz_mul_ui([mpz], [mpz], int);
extern "C" void mpz_tdiv_q(mpz_t q, mpz_t n, mpz_t d);
extern "C" /*unsigned long*/ int mpz_get_ui([mpz]);
extern "C" void mpz_submul_ui(mpz_t rop, mpz_t op1, /*unsigned long*/ int op2);
extern "C" void mpz_addmul_ui(mpz_t rop, mpz_t op1, /*unsigned long*/ int op2);
extern "C" int mpz_cmp([mpz], [mpz]);
