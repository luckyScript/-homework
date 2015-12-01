#include <stdio.h>
int
main(void)
{
   double f,c;
   scanf("%lf",&f);
   c = 5*(f-32)/9;
   printf("%.3lf",c);
   return 0;
}