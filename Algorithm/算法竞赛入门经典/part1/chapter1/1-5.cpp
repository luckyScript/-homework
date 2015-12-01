#include <stdio.h>
int
main(void)
{
    int n;
    scanf("%d",&n);
    double price = (double)(n * 95);
    if(n*95 >= 300)
    {
        printf("%.3lf",price*0.85);
    }else
    {
        printf("%.3lf",price);
    }
    return 0;
}