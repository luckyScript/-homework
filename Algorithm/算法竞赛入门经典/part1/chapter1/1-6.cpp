#include <stdio.h>
int
main(void)
{
    int a,b,c;
    scanf("%d%d%d",&a,&b,&c);
    if(a+b>c && a+c>b && b+c>a) {
        if(a^2+b^2 == c^2 || a^2+c^2 == b^2 || b^2+c^2 == a^2)
        {
            printf("yes");
        }else
        {
            printf("no");
        }
    }else 
    {
        printf("not a triangle");
    }
    return 0;
}