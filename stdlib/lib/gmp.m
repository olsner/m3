module lib::gmp;

type mpz = struct { int alloc; int size; [void] limbs; };
type mpz_t = [mpz];

extern "C" void mpz_init([mpz]) = "__gmpz_init";
extern "C" void mpz_init_set_ui([mpz], int) = "__gmpz_init_set_ui";
extern "C" void mpz_add([mpz], [mpz], [mpz]) = "__gmpz_add";
extern "C" void mpz_mul_2exp([mpz], [mpz], int) = "__gmpz_mul_2exp";
extern "C" void mpz_mul_ui([mpz], [mpz], int) = "__gmpz_mul_ui";
extern "C" void mpz_tdiv_q(mpz_t q, mpz_t n, mpz_t d) = "__gmpz_tdiv_q";
extern "C" /*unsigned long*/ int mpz_get_ui([mpz]) = "__gmpz_get_ui";
extern "C" void mpz_submul_ui(mpz_t rop, mpz_t op1, /*unsigned long*/ int op2) = "__gmpz_submul_ui";
extern "C" void mpz_addmul_ui(mpz_t rop, mpz_t op1, /*unsigned long*/ int op2) = "__gmpz_addmul_ui";
extern "C" int mpz_cmp([mpz], [mpz]) = "__gmpz_cmp";
extern "C" [char] mpz_get_str([char] string, int base, [mpz] integer) = "__gmpz_get_str";
