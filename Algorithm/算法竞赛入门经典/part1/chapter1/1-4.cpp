#include <stdio.h>
#include <math.h>
int
main(void)
{
   unsigned int n;
   scanf("%d",&n);
   if(n <= 360) 
   {
        printf("%lf\n",sin(n));
        printf("%lf",cos(n));
   } else
   {
        return 0;
   }
   return 0;
}