#include "sqlite3.h"

/* A version of sqlite3_close_v2 that doesn't return a value. */
void squeather_close_v2 (sqlite3* handle)
{
  sqlite3_close_v2(handle);
  return;
}

/* A version of sqlite3_finalize that doesn't return a value. */
void squeather_finalize (sqlite3_stmt* pStmt)
{
  sqlite3_finalize(pStmt);
  return;
}
