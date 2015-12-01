#include <stdio.h>
int
main(void)
{
    int n,sum;
    scanf("%d",&n);
    sum = 0;
    for(int i = 0;i < n;i+=1){
        sum+=(i+1);
    }
    printf("%d",sum);
    return 0;
}