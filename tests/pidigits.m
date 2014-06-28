import lib::gmp;
import std::io;
import std::lib;

module pidigits;

mpz[1] tmp1, tmp2, acc, den, num;
type ui = /*unsigned*/ int;

ui extract_digit(ui nth) {
   // joggling between tmp1 and tmp2, so GMP won't have to use temp buffers
   mpz_mul_ui(tmp1, num, nth);
   mpz_add(tmp2, tmp1, acc);
   mpz_tdiv_q(tmp1, tmp2, den);

   return mpz_get_ui(tmp1);
}

void eliminate_digit(ui d) {
   mpz_submul_ui(acc, den, d);
   mpz_mul_ui(acc, acc, 10);
   mpz_mul_ui(num, num, 10);
}

void next_term(ui k) {
   ui k2 = k * 2 + 1;

   mpz_addmul_ui(acc, num, 2);
   mpz_mul_ui(acc, acc, k2);
   mpz_mul_ui(den, den, k2);
   mpz_mul_ui(num, num, k);
}

int main(int argc, [[const char]] argv) {
   ui d, k, i;
   int n = atoi(argv[1]);

   mpz_init(tmp1);
   mpz_init(tmp2);

   mpz_init_set_ui(acc, 0);
   mpz_init_set_ui(den, 1);
   mpz_init_set_ui(num, 1);

   i = k = 0;
   while (i < n) {
      next_term(++k);
      if (mpz_cmp(num, acc) > 0)
         continue;

      d = extract_digit(3);
      if (d != extract_digit(4))
         continue;

      putchar('0' + d);
      if (++i % 10 == 0)
         printf("\t:%u\n", i);
      eliminate_digit(d);
   }

   return 0;
}
