#if !defined(HS_WINDNS_H)
#define HS_WINDNS_H

#include <windows.h>
#include <windns.h>

static inline DNS_RECORDA *
hs_free_record(DNS_RECORDA *data)
{
  if (data)
    DnsRecordListFree(data, DnsFreeRecordList);
  return NULL;
}


static inline DNS_RECORDA *
hs_dns_query(const char *hostname, WORD wtype, long *pstat)
{
  DNS_RECORDA *data = NULL;
  DNS_STATUS status = DnsQuery_A(hostname, wtype, DNS_QUERY_STANDARD, NULL, &data, NULL);

  if (pstat)
    *pstat = status;

  if (status)
    data = hs_free_record(data);

  return data;
}


#endif
