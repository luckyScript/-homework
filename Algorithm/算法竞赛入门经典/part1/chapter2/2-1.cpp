#include <stdio.h>
int
main(void)
{
    for(int a = 1;a < 10;a++) 
    {
        for(int b = 0;b < 10;b++) 
        {
            for(int c = 0;c < 10;c++)
            {
                if ((a^2+b^2+c^2) == (100*a+10*b+c))
                {
                    printf("%d\n",100*a+10*b+c);
                }
                else{}
            }
        }
    }
    return 0;
}