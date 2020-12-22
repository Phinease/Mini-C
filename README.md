# DM - Mini C - Compilation
## CHEN Shuangrui

Run:
```
ocamlbuild -use-menhir main.native
./main.native test.c
```

### Structure:
1. Analyseur lexical - lexer.mll
2. Analyseur syntaxique - parser.mly
3. Vérificateur de type - verificateur.ml (nom de fonctions qui commence par typage_)
4. Main et gestion d'erreur - main.ml
5. Interprèteur - Interpreteur.ml

### Travail réalisé:
1. Partie minimale
2. Boucle for (réalisé par while comme un sucre syntaxique)
3. Opérateurs (/, mod, <=, >, >=, ==, !=, !, &&, ||)
4. Tableaux (réalisé par definir un typ Table of typ)
  ```
  int table[10];    # definition
  table[5];         # accès
  table[6] = 10;    # écriture
  ```
5. Struct définition
6. Afficheur (nom de fonctions qui commence par print_)
7. Interprète


### Partie pas complète:
1. Struct accès et écriture
2. Colonne de la position d'erreur
3. Dans l'interpreteur, il ne connait pas des variable de parametres


### Test file
1. test.c - Typage pour les phrases complèxes
2. test2.c - Simple expérimentation pour l'interpreteur
3. test3.c - Tester des erreurs (dans le commentaire)
