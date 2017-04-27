#include <climits> // UCHAR_MAX
#include <cmath>   // log
#include <cstdint> // uint64
#include <cstddef> // size_t
#include <cstring>

#include "rc.h"

#define R_NO_REMAP
#include <Rmath.h>       // Rf_rexp
#include <R_ext/Print.h> // Rprintf
#include <R_ext/Random.h> // unif_rand
#undef R_NO_REMAP

using std::size_t;

#define NA_LEVEL (UCHAR_MAX - 1)

extern "C" {

void printObs(const unsigned char* x_i, size_t nCol);

struct Data {
  const unsigned char* xt;
  size_t nRow;
  size_t nCol;
  const size_t* nLev;
};

struct Param {
  size_t k;
  
  double alpha; // shape of gamma term on k
  double beta;  // rate of gamma term on k
  double gamma; // weight of gamma term on k, 1 - gamma is weight on NA loss term
  
  double* theta; // weights for each NA component
  
  size_t nBurn;
  size_t nSamp;
  bool verbose;
};

struct State {
  unsigned char* xt;
  size_t* kTable;
  size_t minK;
  double objective;
};

void printState(const Data& data, const Param& param, const State& state);

State ksupp(const Data& data, const Param& param);
void pruneResult(const Data& data, const Param& param, State& state);
void calculateKTable(const Data& data, size_t* kTable);
size_t calculateK(const Data& data, const size_t* kTable, size_t* currK); // returns min K

void decrementKTable(const unsigned char* x_i, size_t nCol, const size_t* nLev, size_t currCol, size_t offset, size_t stride, size_t* kTable);
void incrementKTable(const unsigned char* x_i, size_t nCol, const size_t* nLev, size_t currCol, size_t offset, size_t stride, size_t* kTable);

double getObjective(const Data& data, const Param& param, const unsigned char* xt, size_t minK);

uint64_t simulateUnsignedIntegerUniformInRange(uint64_t min_inclusive, uint64_t max_exclusive)
{
  uint64_t actualMin, actualMax;
  if (min_inclusive < max_exclusive) {
    actualMin = min_inclusive;
    actualMax = max_exclusive;
  } else {
    actualMin = max_exclusive;
    actualMax = min_inclusive;
  }
  
  double range = (double) (actualMax - actualMin);
  
  double u = unif_rand();
  
  return actualMin + (uint64_t) (u * range);
}

SEXP calcK(SEXP xExpr)
{
  SEXP classExpr = rc_getClass(xExpr);
  if (std::strcmp(CHAR(STRING_ELT(classExpr, 0)), "data.frame") != 0) Rf_error("x argument to anonymize must be of 'data.frame' class");
  
  size_t nCol = rc_getLength(xExpr);
  if (nCol == 0) Rf_error("x argument to anonymize cannot be an empty data frame");
  
  size_t nRow = rc_getLength(VECTOR_ELT(xExpr, 0));
  if (nCol == 0) Rf_error("x argument to anonymize cannot be an empty data frame");
  
  size_t* nLev = new size_t[nCol];
  unsigned char* xt = new unsigned char[nCol * nRow];
  
  for (size_t col = 0; col < nCol; ++col) {
    SEXP x_jExpr = VECTOR_ELT(xExpr, col);
    if (!Rf_isFactor(x_jExpr)) Rf_error("column %lu of x not of factor type", col + 1);
    
    nLev[col] = rc_getLength(rc_getLevels(x_jExpr));
    
    if (nLev[col] > UCHAR_MAX - 1) Rf_error("column %lu of x has more levels than maximum (%lu > %hu)", nLev[col], UCHAR_MAX - 1);
    
    int* x_j = INTEGER(x_jExpr);
    
    for (size_t row = 0; row < nRow; ++row)
      xt[col + row * nCol] = x_j[row] == R_NaInt ? NA_LEVEL : x_j[row] - 1;
  }
  
  Data data = { xt, nRow, nCol, nLev };
  
  
  size_t tableSize = data.nLev[0];
  for (size_t col = 1; col < data.nCol; ++ col) tableSize *= data.nLev[col];
  
  size_t* kTable = new size_t[tableSize];
  size_t* kVec = new size_t[data.nRow];
  
  for (size_t i = 0; i < tableSize; ++i) kTable[i] = 0;
  
  calculateKTable(data, kTable);
  (void) calculateK(data, kTable, kVec);
  
  SEXP result = PROTECT(rc_newInteger(nRow));
  int* result_int = INTEGER(result);
  
  for (size_t row = 0; row < nRow; ++row) result_int[row] = static_cast<int>(kVec[row]);
  
  delete [] kVec;
  delete [] kTable;
  
  delete [] xt;
  delete [] nLev;
  
  UNPROTECT(1);
  
  return result;
}

SEXP anonymize(SEXP xExpr, SEXP kExpr, SEXP alphaExpr, SEXP gammaExpr, SEXP thetaExpr, SEXP nBurnExpr, SEXP nSampExpr, SEXP verboseExpr)
{
  SEXP classExpr = rc_getClass(xExpr);
  if (std::strcmp(CHAR(STRING_ELT(classExpr, 0)), "data.frame") != 0) Rf_error("x argument to anonymize must be of 'data.frame' class");
  
  size_t nCol = rc_getLength(xExpr);
  if (nCol == 0) Rf_error("x argument to anonymize cannot be an empty data frame");
  
  size_t nRow = rc_getLength(VECTOR_ELT(xExpr, 0));
  if (nCol == 0) Rf_error("x argument to anonymize cannot be an empty data frame");
  
  if (!Rf_isInteger(kExpr)) Rf_error("k argument to anonymize must be integer type");
  if (rc_getLength(kExpr) != 1) Rf_error("k argument to anonymize must be of length 1");
  if (INTEGER(kExpr)[0] < 1) Rf_error("k must be a positive integer");
  
  if (!Rf_isReal(alphaExpr)) Rf_error("alpha argument to anonymize must be double type");
  if (rc_getLength(alphaExpr) != 1) Rf_error("alpha argument to anonymize must be of length 1");
  if (REAL(alphaExpr)[0] <= 1.0) Rf_error("alpha must be a greater than 1");
  
  if (!Rf_isReal(gammaExpr)) Rf_error("gamma argument to anonymize must be double type");
  if (rc_getLength(gammaExpr) != 1) Rf_error("gamma argument to anonymize must be of length 1");
  if (REAL(gammaExpr)[0] < 0.0 || REAL(gammaExpr)[0] > 1.0) Rf_error("gamma must be in [0,1]");
  
  if (!Rf_isReal(thetaExpr)) Rf_error("theta argument to anonymize must be double type");
  if (rc_getLength(thetaExpr) != nCol) Rf_error("theta argument to anonymize must be of length equal to number of columns in x");
  for (size_t i = 0; i < nCol; ++i) if (REAL(thetaExpr)[i] < 0.0) Rf_error("theta must be positive");
  
  if (!Rf_isInteger(nBurnExpr)) Rf_error("nBurn argument to anonymize must be integer type");
  if (rc_getLength(nBurnExpr) != 1) Rf_error("nBurn argument to anonymize must be of length 1");
  if (INTEGER(nBurnExpr)[0] < 0) Rf_error("nBurn must be a non-negative integer");
  
  if (!Rf_isInteger(nSampExpr)) Rf_error("nSamp argument to anonymize must be integer type");
  if (rc_getLength(nSampExpr) != 1) Rf_error("nSamp argument to anonymize must be of length 1");
  if (INTEGER(nSampExpr)[0] < 1) Rf_error("nSamp must be a positive integer");
  
  if (INTEGER(nSampExpr)[0] <= INTEGER(nBurnExpr)[0]) Rf_error("nSamp must be strictly greater than nBurn");
  
  if (!Rf_isLogical(verboseExpr)) Rf_error("verbose argument to anonymize must be logical type");
  if (rc_getLength(verboseExpr) != 1) Rf_error("verboseExpr argument to anonymize must be of length 1");
  
  Param param = {
    static_cast<size_t>(INTEGER(kExpr)[0]),
    REAL(alphaExpr)[0],
    (REAL(alphaExpr)[0] - 1.0) / static_cast<double>(INTEGER(kExpr)[0]),
    REAL(gammaExpr)[0],
    REAL(thetaExpr),
    static_cast<size_t>(INTEGER(nBurnExpr)[0]),
    static_cast<size_t>(INTEGER(nSampExpr)[0]),
    static_cast<bool>(INTEGER(verboseExpr)[0])
  };
  
  double thetaTotal = 0.0;
  for (size_t i = 0; i < nCol; ++i) thetaTotal += param.theta[i];
  for (size_t i = 0; i < nCol; ++i) param.theta[i] *= static_cast<double>(nCol) / thetaTotal;
  
  size_t* nLev = new size_t[nCol];
  unsigned char* xt = new unsigned char[nCol * nRow];
  
  for (size_t col = 0; col < nCol; ++col) {
    SEXP x_jExpr = VECTOR_ELT(xExpr, col);
    if (!Rf_isFactor(x_jExpr)) Rf_error("column %lu of x not of factor type", col + 1);
    
    nLev[col] = rc_getLength(rc_getLevels(x_jExpr));
    
    if (nLev[col] > UCHAR_MAX - 1) Rf_error("column %lu of x has more levels than maximum (%lu > %hu)", nLev[col], UCHAR_MAX - 1);
    
    int* x_j = INTEGER(x_jExpr);
    
    for (size_t row = 0; row < nRow; ++row)
      xt[col + row * nCol] = x_j[row] == R_NaInt ? NA_LEVEL : x_j[row] - 1;
  }
  
  Data data = { xt, nRow, nCol, nLev };
    
  // Rprintf("nLev: %lu", nLev[0]);
  // for (size_t i = 1; i < nCol; ++i) Rprintf(", %lu", nLev[i]);
  // Rprintf("\n");
  
  GetRNGstate();
  
  State best = ksupp(data, param);
  
  PutRNGstate();
  
  // see if there are any NAs that we can safely restore without increasing k
  // greedy algorithm style
  pruneResult(data, param, best);
  
  // package up results into a list
  SEXP result = PROTECT(rc_newList(3));
  SEXP xNew = SET_VECTOR_ELT(result, 0, rc_newList(nCol + 1));
  for (size_t col = 0; col < nCol; ++col) {
    SEXP x_j_old = VECTOR_ELT(xExpr, col);
    SEXP x_j_new = SET_VECTOR_ELT(xNew, col, rc_newInteger(nRow));
    
    int* x_j = INTEGER(x_j_new);
    
    for (size_t row = 0; row < nRow; ++row)
      x_j[row] = best.xt[col + row * data.nCol] == NA_LEVEL ? R_NaInt : best.xt[col + row * data.nCol] + 1;
    
    Rf_setAttrib(x_j_new, R_LevelsSymbol, rc_getLevels(x_j_old));
    Rf_setAttrib(x_j_new, R_ClassSymbol, rc_getClass(x_j_old));
  }
  SET_VECTOR_ELT(xNew, nCol, rc_newInteger(nRow));
  
  size_t* temp = new size_t[nRow];
  calculateK(data,  best.kTable, temp);
  int* k_int = INTEGER(VECTOR_ELT(xNew, nCol));
  for (size_t row = 0; row < nRow; ++row)
    k_int[row] = static_cast<int>(temp[row]);
  delete [] temp;
  
  Rf_setAttrib(xNew, R_ClassSymbol, rc_getClass(xExpr));
  Rf_setAttrib(xNew, R_RowNamesSymbol, Rf_getAttrib(xExpr, R_RowNamesSymbol));
  rc_setNames(xNew, PROTECT(rc_newCharacter(nCol + 1)));
  
  SEXP names_old = rc_getNames(xExpr);
  SEXP names_new = rc_getNames(xNew);
  for (size_t col = 0; col < nCol; ++col)
    SET_STRING_ELT(names_new, col, STRING_ELT(names_old, col));
  SET_STRING_ELT(names_new, nCol, Rf_mkChar("k"));
  
  // store numeric results
  SET_VECTOR_ELT(result, 1, Rf_ScalarReal(best.objective));
  double naTerm = 0.0;
  for (size_t row = 0; row < data.nRow; ++row) {
    for (size_t col = 0; col < data.nCol; ++col) if (best.xt[col + row * data.nCol] == NA_LEVEL) naTerm += param.theta[col]; 
  }
  naTerm /= static_cast<double>(data.nRow);
  SET_VECTOR_ELT(result, 2, Rf_ScalarReal(naTerm));
  
  
  rc_setNames(result, PROTECT(rc_newCharacter(2)));
  SEXP namesExpr = rc_getNames(result);
  SET_STRING_ELT(namesExpr, 0, Rf_mkChar("x"));
  SET_STRING_ELT(namesExpr, 1, Rf_mkChar("obj"));
  SET_STRING_ELT(namesExpr, 2, Rf_mkChar("n"));
    
  delete [] best.kTable;
  delete [] best.xt;
  
  delete [] xt;
  delete [] nLev;
  
  UNPROTECT(3);
  
  return result;
}

State ksupp(const Data& data, const Param& param)
{
  size_t tableSize = data.nLev[0];
  for (size_t col = 1; col < data.nCol; ++ col) tableSize *= data.nLev[col];
  // Rprintf("allocating table of size: %lu\n", tableSize);
  
  State curr = { /* xt        = */ new unsigned char[data.nRow * data.nCol], 
                 /* kTable    = */ new size_t[tableSize],
                 /* minK      = */ static_cast<size_t>(-1),
                 /* objective = */ -HUGE_VAL };
  
  // draw from "prior"
  std::memcpy(curr.xt, data.xt, data.nRow * data.nCol * sizeof(unsigned char));
  for (size_t i = 0; i < tableSize; ++i) curr.kTable[i] = 0;
  
  // only needed here to print stuff during draws from prior
  // size_t* kVec = new size_t[data.nRow];
  /* calculateKTable(data, curr.kTable);
  curr.minK = calculateK(data, curr.kTable, kVec);
  curr.objective = getObjective(data, param, curr.xt, curr.minK);  */
  
  /* for (size_t i = 0; i < 5; ++i) {
    Rprintf("%lu: ", i);
    printObs(data.xt + i * data.nCol, data.nCol);
    Rprintf(", k: %lu\n", kVec[i]);
  } */
  
  for (size_t i = 0; i < param.nBurn / 4; ++i) {
    size_t row = static_cast<size_t>(simulateUnsignedIntegerUniformInRange(0, static_cast<uint64_t>(data.nRow)));
    size_t col = static_cast<size_t>(simulateUnsignedIntegerUniformInRange(0, static_cast<uint64_t>(data.nCol)));
    
    /* Rprintf("prior iter %lu, state: ", i + 1);
    printState(data, param, curr);
    Rprintf("\n"); */
    
    if (curr.xt[col + row * data.nCol] == NA_LEVEL) {
      if (data.xt[col + row * data.nCol] == NA_LEVEL) continue; // proposed and current state are identical, burn an obs
      
      curr.xt[col + row * data.nCol] = data.xt[col + row * data.nCol];
    } else {
      curr.xt[col + row * data.nCol] = NA_LEVEL;
    }
    
    /* calculateKTable(data, curr.kTable);
    curr.minK = calculateK(data, curr.kTable, kVec);
    curr.objective = getObjective(data, param, curr.xt, curr.minK); */
  }
  
  calculateKTable(data, curr.kTable);
  curr.minK = calculateK(data, curr.kTable, NULL);
  curr.objective = getObjective(data, param, curr.xt, curr.minK);
  
  
  State prop = { new unsigned char[data.nRow * data.nCol], new size_t[tableSize],
                 static_cast<size_t>(-1), -HUGE_VAL };
  State best = { new unsigned char[data.nRow * data.nCol], new size_t[tableSize],
                 static_cast<size_t>(-1), -HUGE_VAL };
  
  std::memcpy(best.xt, data.xt, data.nRow * data.nCol * sizeof(unsigned char));
  
  for (size_t i = 0; i < param.nSamp; ++i) {
    std::memcpy(prop.xt, const_cast<const unsigned char*>(curr.xt), data.nRow * data.nCol * sizeof(unsigned char));
    std::memcpy(prop.kTable, const_cast<const size_t*>(curr.kTable), tableSize * sizeof(size_t));
    
    size_t row = static_cast<size_t>(simulateUnsignedIntegerUniformInRange(0, static_cast<uint64_t>(data.nRow)));
    size_t col = static_cast<size_t>(simulateUnsignedIntegerUniformInRange(0, static_cast<uint64_t>(data.nCol)));
    
    if (curr.xt[col + row * data.nCol] == NA_LEVEL) {
      if (data.xt[col + row * data.nCol] == NA_LEVEL) continue; // proposed and current state are identical, burn an obs
      
      prop.xt[col + row * data.nCol] = data.xt[col + row * data.nCol];
    } else {
      prop.xt[col + row * data.nCol] = NA_LEVEL;
    }
    
    decrementKTable(curr.xt + row * data.nCol, data.nCol, data.nLev, 0, 0, 1, prop.kTable);
    incrementKTable(prop.xt + row * data.nCol, data.nCol, data.nLev, 0, 0, 1, prop.kTable);
    
    // can do some minor optimization here, reusing parts of the objective function that are unchanged
    prop.minK = calculateK(data, prop.kTable, NULL);
    prop.objective = getObjective(data, param, prop.xt, prop.minK);
    
    double ratio = prop.objective - curr.objective;
    
    if (-Rf_rexp(1.0) < ratio) {
      std::memcpy(curr.xt, const_cast<const unsigned char*>(prop.xt), data.nRow * data.nCol * sizeof(unsigned char));
      std::memcpy(curr.kTable, const_cast<const size_t*>(prop.kTable), tableSize * sizeof(size_t));
      curr.minK = prop.minK;
      curr.objective = prop.objective;
      
      // we only save the first one during burn-in, in case there isn't another
      if ((i < param.nBurn && best.objective == -HUGE_VAL && curr.minK >= param.k) ||
          (i >= param.nBurn && curr.objective > best.objective && curr.minK >= param.k)) {
        if (param.verbose) {
          Rprintf("iter %lu: updating best from ", i);
          printState(data, param, best);
          Rprintf(" to ");
          printState(data, param, curr);
          Rprintf("\n");
        }
        
        std::memcpy(best.xt, const_cast<const unsigned char*>(curr.xt), data.nRow * data.nCol * sizeof(unsigned char));
        std::memcpy(best.kTable, const_cast<const size_t*>(curr.kTable), tableSize * sizeof(size_t));
        best.minK = curr.minK;
        best.objective = curr.objective;
      }
    }
    if (param.verbose && (i + 1) % 100 == 0) Rprintf("iter: %lu\n", i + 1);
  }
  
  delete [] prop.kTable;
  delete [] prop.xt;

  delete [] curr.kTable;
  delete [] curr.xt;
  
  // delete [] kVec;
  
  return best;
}

void permuteIndexArray(size_t* indices, size_t length);

void pruneResult(const Data& data, const Param& param, State& state)
{
  size_t tableSize = data.nLev[0];
  for (size_t col = 1; col < data.nCol; ++ col) tableSize *= data.nLev[col];
  
  State temp = { new unsigned char[data.nRow * data.nCol], new size_t[tableSize],
                 state.minK, state.objective };
  
  
  size_t* indices = new size_t[data.nRow];
  for (size_t row = 0; row < data.nRow; ++row) indices[row] = row;
  permuteIndexArray(indices, data.nRow);
  
  
  for (size_t row = 0; row < data.nRow; ++row) {
    size_t row_i = indices[row];
    
    for (size_t col = 0; col < data.nCol; ++col) {
      if (state.xt[col + row_i * data.nCol] == NA_LEVEL && data.xt[col + row_i * data.nCol] != NA_LEVEL) {
        std::memcpy(temp.xt, const_cast<const unsigned char*>(state.xt), data.nRow * data.nCol * sizeof(unsigned char));
        std::memcpy(temp.kTable, const_cast<const size_t*>(state.kTable), tableSize * sizeof(size_t));
        
        temp.xt[col + row_i * data.nCol] = data.xt[col + row_i * data.nCol];
        
        decrementKTable(state.xt + row_i * data.nCol, data.nCol, data.nLev, 0, 0, 1, temp.kTable);
        incrementKTable(temp.xt + row_i * data.nCol, data.nCol, data.nLev, 0, 0, 1, temp.kTable);
        temp.minK = calculateK(data, temp.kTable, NULL);
        
        if (temp.minK >= param.k) {
          std::memcpy(state.xt, const_cast<const unsigned char*>(temp.xt), data.nRow * data.nCol * sizeof(unsigned char));
          std::memcpy(state.kTable, const_cast<const size_t*>(temp.kTable), tableSize * sizeof(size_t));
          state.minK = temp.minK;
          state.objective = getObjective(data, param, state.xt, state.minK);
        }
      }
    }
  }
}

void permuteIndexArray(size_t* indices, size_t length)
{
  size_t temp, swapPos;
  for (size_t i = 0; i < length - 1; ++i) {
    swapPos = static_cast<size_t>(simulateUnsignedIntegerUniformInRange(i, length));
    
    temp = indices[i];
    indices[i] = indices[swapPos];
    indices[swapPos] = temp;
  }
}

void incrementKTable(const unsigned char* x_i, size_t nCol, const size_t* nLev, size_t currCol, size_t offset, size_t stride, size_t* kTable)
{
  if (currCol == nCol - 1) {
    /* Rprintf("for x_i = ");
    printObs(x_i, nCol);
    Rprintf(" incrementing: "); */
    
    if (x_i[currCol] != NA_LEVEL) {
      ++kTable[offset + x_i[currCol] * stride];
      //Rprintf("%lu\n", offset + x_i[currCol] * stride);
    } else {
      //Rprintf("%lu", offset);
      ++kTable[offset];
      for (size_t i = 1; i < nLev[currCol]; ++i) {
        //Rprintf(", %lu", offset + i * stride);
        ++kTable[offset + i * stride];
      }
      // Rprintf("\n");
    }
  } else {
    if (x_i[currCol] != NA_LEVEL) incrementKTable(x_i, nCol, nLev, currCol + 1, offset + x_i[currCol] * stride, stride * nLev[currCol], kTable);
    else {
      for (size_t i = 0; i < nLev[currCol]; ++i)
        incrementKTable(x_i, nCol, nLev, currCol + 1, offset + i * stride, stride * nLev[currCol], kTable);
    }
  }
}

void decrementKTable(const unsigned char* x_i, size_t nCol, const size_t* nLev, size_t currCol, size_t offset, size_t stride, size_t* kTable)
{
  if (currCol == nCol - 1) {
    if (x_i[currCol] != NA_LEVEL) {
      --kTable[offset + x_i[currCol] * stride];
    } else {
      for (size_t i = 0; i < nLev[currCol]; ++i)
        --kTable[offset + i * stride];
    }
  } else {
    if (x_i[currCol] != NA_LEVEL) decrementKTable(x_i, nCol, nLev, currCol + 1, offset + x_i[currCol] * stride, stride * nLev[currCol], kTable);
    else {
      for (size_t i = 0; i < nLev[currCol]; ++i)
        decrementKTable(x_i, nCol, nLev, currCol + 1, offset + i * stride, stride * nLev[currCol], kTable);
    }
  }
}

size_t getKFromTable(const unsigned char* x_i, size_t nCol, const size_t* nLev, size_t currCol, size_t offset, size_t stride, const size_t* kTable)
{
  if (currCol == nCol - 1) {
    if (x_i[currCol] != NA_LEVEL) {
      return kTable[offset + x_i[currCol] * stride];
    } else {
      size_t minK = kTable[offset];
      for (size_t i = 1; i < nLev[currCol]; ++i)
        if (kTable[offset + i * stride] < minK) minK = kTable[offset + i * stride];
      return minK;
    }
  } else {
    if (x_i[currCol] != NA_LEVEL) return getKFromTable(x_i, nCol, nLev, currCol + 1, offset + x_i[currCol] * stride, stride * nLev[currCol], kTable);
    else {
      size_t minK = getKFromTable(x_i, nCol, nLev, currCol + 1, offset, stride * nLev[currCol], kTable);
      for (size_t i = 1; i < nLev[currCol]; ++i) {
        size_t k = getKFromTable(x_i, nCol, nLev, currCol + 1, offset + i * stride, stride * nLev[currCol], kTable);
        if (k < minK) minK = k;
      }
      return minK;
    }
  }
}

void calculateKTable(const Data& data, size_t* kTable)
{
  const unsigned char* xt = data.xt;
  for (size_t row = 0; row < data.nRow; ++row) {
    incrementKTable(xt, data.nCol, data.nLev, 0, 0, 1, kTable);
    xt += data.nCol;
  }
}

size_t calculateK(const Data& data, const size_t* kTable, size_t* currK)
{
  const unsigned char* xt = data.xt;
  size_t minK = static_cast<size_t>(-1);
  
  for (size_t row = 0; row < data.nRow; ++row) {
    size_t k_i = getKFromTable(xt, data.nCol, data.nLev, 0, 0, 1, kTable);
    if (k_i < minK) minK = k_i;
    
    xt += data.nCol;
    
    if (currK != NULL) currK[row] = k_i;
  }
  return minK;
}

// (1 - gamma) * -log(mean(naTerm)) + log(kTerm)
// mean(naTerm) is weighted average of number of NAs in data
// kTerm is a gamma density
double getObjective(const Data& data, const Param& param, const unsigned char* xt, size_t minK)
{
  double naTerm = 0.0;
  
  for (size_t row = 0; row < data.nRow; ++row) {
    for (size_t col = 0; col < data.nCol; ++col) if (xt[col + row * data.nCol] == NA_LEVEL) naTerm += param.theta[col]; 
  }
  naTerm /= static_cast<double>(data.nRow);
  
  double k_double = static_cast<double>(minK);
  double kTerm = (param.alpha - 1.0) * std::log(k_double) - param.beta * k_double;
  
  return param.gamma * kTerm + (1.0 - param.gamma) * -std::log(naTerm);
}

void printObs(const unsigned char* x_i, size_t nCol)
{
  Rprintf("(");
  if (x_i[0] != NA_LEVEL) Rprintf("%hu", x_i[0]); else Rprintf("NA");
  
  for (size_t j = 1; j < nCol; ++j) {
    if (x_i[j] != NA_LEVEL) Rprintf(", %hu", x_i[j]); else Rprintf(", NA");
  }
  Rprintf(")");
}

void printState(const Data& data, const Param& param, const State& state)
{
  double naTerm = 0.0;
  
  for (size_t row = 0; row < data.nRow; ++row) {
    for (size_t col = 0; col < data.nCol; ++col) if (state.xt[col + row * data.nCol] == NA_LEVEL) naTerm += param.theta[col]; 
  }
  naTerm /= static_cast<double>(data.nRow);
 
  Rprintf("(o = %.4f, k = %lu, n = %.4f)", state.objective, state.minK, naTerm);
}

#define DEF_FUNC(_N_, _F_, _A_) { _N_, reinterpret_cast<DL_FUNC>(&_F_), _A_ }

static R_CallMethodDef R_callMethods[] = {
  DEF_FUNC("anonymize", anonymize, 8),
  DEF_FUNC("calcK", calcK, 1),
  { NULL, NULL, 0 }
};

#undef DEF_FUNC

void R_init_mcsupp(DllInfo* info)
{
  R_registerRoutines(info, NULL, R_callMethods, NULL, NULL);
  R_useDynamicSymbols(info, static_cast<Rboolean>(FALSE));
}

}
