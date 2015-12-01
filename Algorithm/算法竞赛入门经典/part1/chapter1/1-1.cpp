#include <stdio.h>
int
main(void)
{
    int a, b, c;
    scanf("%d%d%d",&a,&b,&c);
    double d = (double)(a+b+c);
    printf("%.3lf",d/3.0);
}