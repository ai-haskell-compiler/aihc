#ifndef COINOR_HS_SUPPORT_H
#define COINOR_HS_SUPPORT_H

#include "Clp_C_Interface.h"
#include "ClpPlusMinusOneMatrix.hpp"


#ifdef __cplusplus
extern "C" {
#endif


ClpPlusMinusOneMatrix *
  Clp_newPlusMinusOneMatrix (
    int numberRows,
    int numberColumns,
    bool columnOrdered,
    const int *indices,
    const CoinBigIndex *startPositive,
    const CoinBigIndex *startNegative
  );

void
  Clp_deletePlusMinusOneMatrix (
    ClpPlusMinusOneMatrix *matrix
  );

void
  Clp_loadProblemFromMatrix (
    Clp_Simplex *model,
    const ClpMatrixBase *matrix,
    const double *collb,
    const double *colub,
    const double *obj,
    const double *rowlb,
    const double *rowub,
    const double *rowObjective
  );


CoinPackedMatrix *
  Clp_newCoinPackedMatrix (
    const bool colordered,
    const int minor,
    const int major,
    const CoinBigIndex numels,
    const double *elem,
    const int *ind,
    const CoinBigIndex *start,
    const int *len
  );

void
  Clp_deleteCoinPackedMatrix (
    CoinPackedMatrix *matrix
  );

void
  Clp_loadProblemFromCoinMatrix (
    Clp_Simplex *model,
    const CoinPackedMatrix *matrix,
    const double *collb,
    const double *colub,
    const double *obj,
    const double *rowlb,
    const double *rowub,
    const double *rowObjective
  );

void Clp_dumpMatrix (Clp_Simplex *model);



#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* COINOR_HS_SUPPORT_H */
