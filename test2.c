int hauteur = 10;
int largeur = 20;
int PARAM = 10;

typedef struct Books {
   int title[50];
   bool price[90];
   int book_id;
} Books;

int fact(int n) {
  if (n < 2) {
    return 1;
  } else {
    return n * fact(n + -1);
  }
}

int tableau() {
  int t[10];

  for (int i=0; i < 10; i = i + 1) {
    t[i] = i;
  }

  putchar(65);

  return 2*60;
}

int main() {
  putchar(65);
  putchar(tableau());
}
