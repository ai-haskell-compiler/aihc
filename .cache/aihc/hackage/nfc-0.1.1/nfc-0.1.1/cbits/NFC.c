#include "nfc/nfc.h"
#include "nfc/nfc-types.h"

/* These functions are needed because the libnfc author used #pragma pack(1).
 * c2hs does not recognize this compiler directive.
 */
nfc_modulation *hs_nfc_get_nm(nfc_target *t) { return &t->nm; }

size_t hs_nfc_target_size() { return sizeof(nfc_target); }

uint8_t *hs_nfc_get_nai_abtAtqa(nfc_target *t)  { return t->nti.nai.abtAtqa;  }
uint8_t  hs_nfc_get_nai_btSak(nfc_target *t)    { return t->nti.nai.btSak;    }
size_t   hs_nfc_get_nai_szUidLen(nfc_target *t) { return t->nti.nai.szUidLen; }
uint8_t *hs_nfc_get_nai_abtUid(nfc_target *t)   { return t->nti.nai.abtUid;   }
size_t   hs_nfc_get_nai_szAtsLen(nfc_target *t) { return t->nti.nai.szAtsLen; }
uint8_t *hs_nfc_get_nai_abtAts(nfc_target *t)   { return t->nti.nai.abtAts;   }

/* Normally I would have c2hs make this for me, but I need to return Nothing or
 * Just NFCTarget based on the return value, which c2hs does not support at this
 * time.
 */
int __wrapped__nfc_initiator_select_passive_target (nfc_device *pnd,
                                                    const nfc_modulation *nm,
                                                    const uint8_t *pbtInitData,
                                                    const size_t szInitData,
                                                    nfc_target *pnt)
{
  return nfc_initiator_select_passive_target (pnd,
                                              *nm,
                                              pbtInitData,
                                              szInitData,
                                              pnt);
}
