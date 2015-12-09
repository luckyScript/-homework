#include <stdio.h>
int
main()
{   
    int n;
    scanf("%d",&n);
    for(int i = 0;i <= n-1;i++)
    {
        for(int j = 0;j <= (2*n-2);j++)
        {
            if(j >= i && ((2*n-2)-j) >= i)
            {
                printf("#");
            }
            else
            {
                printf(" ");
            }
        }
        printf("\n");
    }
    return 0;
}