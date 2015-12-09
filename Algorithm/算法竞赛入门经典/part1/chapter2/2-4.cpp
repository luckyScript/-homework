#include <stdio.h>

int 
main()
{
    long n,m;
    scanf("%ld%ld",&n,&m);
    if(m > n) 
    {   
        double sum = 0.0;
        for(int i = n;i <= m; i++)
        {
            sum += 1.0/(i*i);
            printf("%lf",sum);
        }
        printf("%.5lf",sum);
    }
    else 
    {
        return 0;
    }
    return 0;
}