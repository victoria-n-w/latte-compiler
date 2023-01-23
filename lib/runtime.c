#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int readInt() {
    int x;
    scanf("%d%*c", &x);
    return x;
}

void printInt(int x) {
    printf("%d\n", x);
}

char *readString() {
    size_t buffsize = 16;
    char *res = malloc(buffsize * sizeof(char));
    getline(&res, &buffsize, stdin);
    size_t len = strlen(res);
    if (len > 0 && res[len - 1] == '\n') {
        res[len - 1] = '\0';
    }
    return res;
}

void printString(const char *s) {
    printf("%s\n", s);
}

char *concat(const char *a, const char *b){
    char *s = malloc(strlen(a) + strlen(b) + 1);
    strcpy(s, a);
    strcat(s, b);
    return s;
}

void* new(size_t size) {
    return malloc(size);
}
