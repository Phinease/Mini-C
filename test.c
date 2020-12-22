int PARAM = 5;

typedef struct Books {
   int title[50];
   bool price[90];
   int book_id;
} Books;

int fact(int n) {
  for (int i = 0; i < 10; i = i + 1) {
    n = n * 1;
  }
  
  /*
  bool n = 15;
  */

  if (!(n != 8)) {
    int k = 10;
  } else {
    10;
  }

  /* Pas réussi
  Books book;

  book.title[23] = 20;
  book.book_id = 10;
  n = book.book_id;
  */

  if (n <= 0 && n >= 15 || n == 19) {
    n = n / 10 % 5;
  } else {
    9;
  }

  int tableau[10];

  tableau[5] = 10;
  n = tableau[5] + 10;

  n = n - 1;
  if (n < 2) {
    return 1;
  } else {
    return n * fact(n + -1);
  }

  while (PARAM < 10) {
    n = n + 1;
  }
}

int main() {
  putchar(fact(PARAM));
}
