#include <stdio.h>

int foo() {
	return 69;
}

void main() {
	int x;
	x = foo();
	printf("%d", x);
}
