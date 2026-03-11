#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <netinet/in.h>
#include <resolv.h>

int check_mx(char *domain)
{
  u_char nsbuf[4096];
  return res_query(domain, ns_c_in, ns_t_mx, nsbuf, sizeof (nsbuf)) > 0;
}
