#include <stdio.h>
#include <stdlib.h>

void assert_ft(int cond, int num)
{
  if (!cond)
    {
      fprintf(stderr,"**Possible soft-error detected at %d. Exiting program.\n",num);
      exit(66);
    }
}

void assert_cfg_ft(int cond, int GSR, int num)
{
  if (!cond)
    {
      fprintf(stderr,"**Possible soft-error detected at due to control flow into block %d (%d). Exiting program.\n",GSR,num);
      exit(66);
    }
}
