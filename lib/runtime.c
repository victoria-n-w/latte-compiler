#include "stdlib.h"
#include "stdio.h"
#include "string.h"

int readInt() {
    int x;
    scanf("%d", &x);
    return x;
}

void printInt(int x) {
    printf("%d", x);
}

char *readString() {
    char *s = malloc(100);
    scanf("%s", s);
    return s;
}

void printString(char *s) {
    printf("%s", s);
}

char *concat(char *a, char *b){
    char *s = malloc(strlen(a) + strlen(b) + 1);
    strcpy(s, a);
    strcat(s, b);
    return s;
}
