#include <stdio.h>
int
main(void)
{
    int a,b,c;
    scanf("%d%d%d",&a,&b,&c);
    int num;
    for(num = 10;num <= 100;num++)
    {
        if(num%3 == a) 
        {
            if(num%5 == b)
            {
                if(num%7 == c)
                {
                    printf("%d",num);               
                }
            }
        }
    }
    return 0;
}