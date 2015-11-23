#include <stdio.h>
int main(void)
{
    void PrintOut(int N);
    PrintOut(2326);
    return 0;
}
void 
PrintOut(int N)
{
    if(N >= 10)
        PrintOut(N/10);
    else
        printf("%d",N);
}