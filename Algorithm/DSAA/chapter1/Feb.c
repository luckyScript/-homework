#include <stdio.h>
int 
main(void)
{
    long feb(long n);
    printf("%ld",feb(23));
    return 0;
}

long
feb(long n)
{
    if (n == 0)
        return 0;
    else if (n == 1)
        return 1;
    else
        return feb(n-1) + feb(n-2);
}