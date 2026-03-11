#define CLP_EXTERN_C
#include "ClpSimplex.hpp"
#include "support.h"


ClpPlusMinusOneMatrix *
  Clp_newPlusMinusOneMatrix (
    int numberRows,
    int numberColumns,
    bool columnOrdered,
    const int *indices,
    const CoinBigIndex *startPositive,
    const CoinBigIndex *startNegative
  ) {

  return
    new ClpPlusMinusOneMatrix (
      numberRows, numberColumns,
      columnOrdered, indices,
      startPositive, startNegative
    );
}

void
  Clp_deletePlusMinusOneMatrix (
    ClpPlusMinusOneMatrix *matrix
  ) {
  delete matrix;
}

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
  ) {

  model->model_->loadProblem (
      *matrix,
      collb, colub, obj,
      rowlb, rowub, rowObjective
    );
}


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
  ) {
  return
    new CoinPackedMatrix (
      colordered,
      minor, major, numels,
      elem, ind, start, len
    );
}

void
  Clp_deleteCoinPackedMatrix (
    CoinPackedMatrix *matrix
  ) {
  delete matrix;
}

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
  ) {

  model->model_->loadProblem (
      *matrix,
      collb, colub, obj,
      rowlb, rowub, rowObjective
    );
}


void Clp_dumpMatrix (Clp_Simplex *model) {
  model->model_->matrix()->dumpMatrix ();
}
