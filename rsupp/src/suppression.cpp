#include <climits> // UCHAR_MAX
#include <cmath>   // log
#include <cstring> // strcmp
#include <stdint.h> // uint64
#include <cstddef> // size_t

#include "rc.h"
#include "random.h"

#define R_NO_REMAP
#include <Rinternals.h>  // SEXP
#include <Rmath.h>       // Rf_rexp
#include <R_ext/Print.h> // Rprintf
#include <R_ext/Random.h> // GetRNGState/PutRNGState/unif_rand
#undef R_NO_REMAP

#include "data.hpp"
#include "definitions.hpp"
#include "param.hpp"
#include "riskFunction.hpp"
#include "state.hpp"

using std::size_t;

using rsupp::Data;
using rsupp::MCMCParam;
using rsupp::Param;
using rsupp::State;
using rsupp::RiskFunction;
using rsupp::KRiskFunction;
using rsupp::DivRiskFunction;

#define MAX_RANDOM_INIT_ITERATIONS 500

namespace {
  bool initializeStateWithRandomSuppressions(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, State& state);
  State* mcmcSuppression(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& state);
  void pruneNAs(const Data& data, const Param& param, RiskFunction& calculateRisk, State& state);
  
  Data* subsetDataOnAtRiskAndSimilar(const Data& data, const Param& param, RiskFunction& calculateRisk,
                                     size_t** subsetIndices, size_t* subsetLength);
  State* mergeSubset(const Data& origData, const Data& subsetData, const State& subsetState, size_t* subsetIndices, size_t subsetLength);
  
  double getObjective(const Data& data, const MCMCParam& param, const unsigned char* xt, double minRisk);
  
  SEXP packageSuppressionResult(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& state, SEXP xExpr);
}
/*
#undef new
void* operator new (size_t size, const char* filename, int line) {
  void* ptr = new char[size];
  Rprintf("alloc size = " SIZE_T_FMT ", file = %s, line = %d\n", size, filename, line);
  return ptr;
}
void* operator new[] (size_t size, const char* filename, int line) {
  void* ptr = new char[size];
  Rprintf("alloc size = " SIZE_T_FMT ", file = %s, line = %d\n", size, filename, line);
  return ptr;
}
#define new new(__FILE__, __LINE__)
*/

namespace rsupp {
  // not in anonymous in case it needs to be externed for testing purposes
  void printObs(const rsupp::Data& data, const unsigned char* x_i);
}

using rsupp::printObs;

extern "C" {

SEXP calcRisk(SEXP xExpr, SEXP riskFunctionExpr)
{
  Data data(xExpr);
  
  RiskFunction* calculateRiskPtr = NULL;
  try {
    if (riskFunctionExpr == R_NilValue) calculateRiskPtr = new KRiskFunction();
    else calculateRiskPtr = new DivRiskFunction(data, riskFunctionExpr);
  } catch (const char* error) {
    // get around Rf_error not unwinding stack
    data.Data::~Data();
    Rf_error(error);
  }
  RiskFunction& calculateRisk(*calculateRiskPtr);
  
  State state(data);
  
  state.calculateFreqTable(data);
  
  SEXP result = PROTECT(rc_newReal(data.nRow));
  calculateRisk(data, state, REAL(result));
  
  delete calculateRiskPtr;
  
  UNPROTECT(1);
  
  return result;
}

SEXP getAtRiskSubset(SEXP xExpr, SEXP riskFunctionExpr, SEXP thresholdExpr)
{
  if (!Rf_isReal(thresholdExpr)) Rf_error("threshold parameter must be real type");
  if (rc_getLength(thresholdExpr) != 1) Rf_error("threshold parameter must be of length 1");
  if (REAL(thresholdExpr)[0] <= 0.0) Rf_error("threshold parameter must be non-negative");
  
  Data origData(xExpr);
  
  RiskFunction* calculateRiskPtr = NULL;
  try {
    if (riskFunctionExpr == R_NilValue) calculateRiskPtr = new KRiskFunction();
    else calculateRiskPtr = new DivRiskFunction(origData, riskFunctionExpr);
  } catch (const char* error) {
    origData.Data::~Data();
    Rf_error(error);
  }
  RiskFunction& calculateRisk(*calculateRiskPtr);
  
  Param* paramPtr = NULL;
  try {
    paramPtr = new Param(origData, riskFunctionExpr != R_NilValue ? calculateRiskPtr : NULL, REAL(thresholdExpr)[0], 0);
  } catch (const char* error) {
    delete calculateRiskPtr;
    origData.Data::~Data();
    Rf_error(error);
  }
  Param& param(*paramPtr);
    
  size_t* subsetIndices;
  size_t subsetLength;
  Data* subsetDataPtr = subsetDataOnAtRiskAndSimilar(origData, param, calculateRisk, &subsetIndices, &subsetLength);
  Data& subsetData(*subsetDataPtr);
  
  SEXP xNew = PROTECT(rc_newList(subsetData.nCol + 1));
  for (size_t col = 0; col < subsetData.nCol; ++col) {
    SEXP x_j_old = VECTOR_ELT(xExpr, col);
    SEXP x_j_new = SET_VECTOR_ELT(xNew, col, rc_newInteger(subsetData.nRow));
    
    int* x_j = INTEGER(x_j_new);
    
    for (size_t row = 0; row < subsetData.nRow; ++row)
      x_j[row] = subsetData.xt[col + row * subsetData.nCol] == NA_LEVEL ? R_NaInt : (subsetData.xt[col + row * subsetData.nCol] + 1);
    
    Rf_setAttrib(x_j_new, R_LevelsSymbol, rc_getLevels(x_j_old));
    Rf_setAttrib(x_j_new, R_ClassSymbol, rc_getClass(x_j_old));
  }
  SET_VECTOR_ELT(xNew, subsetData.nCol, rc_newReal(subsetData.nRow));
  
  // calculate risk for original data
  State state(origData);
  state.calculateFreqTable(origData);
  double* origRisk = new double[origData.nRow];
  calculateRisk(origData, state, origRisk);
  double* risk = REAL(VECTOR_ELT(xNew, subsetData.nCol));
  for (size_t row = 0; row < subsetLength; ++row)
    risk[row] = origRisk[subsetIndices[row]];
  delete [] origRisk;
  
  // set attributes
  Rf_setAttrib(xNew, R_ClassSymbol, rc_getClass(xExpr));
  
  // rownames requires turning 1:nRow into characters
  SEXP rowNames = PROTECT(rc_newCharacter(subsetData.nRow));
  size_t numDigits = 0;
    for (size_t temp = subsetData.nRow + 1; temp > 0; temp /= 10) ++numDigits;
  char* rowName = new char[numDigits + 1];
  for (size_t row = 0; row < subsetData.nRow; ++row) {
    std::sprintf(rowName, SIZE_T_FMT, row + 1);
    SET_STRING_ELT(rowNames, row, Rf_mkChar(rowName));
  }
  delete [] rowName;
    
  Rf_setAttrib(xNew, R_RowNamesSymbol, rowNames);
  rc_setNames(xNew, PROTECT(rc_newCharacter(subsetData.nCol + 1)));
  
  SEXP names_old = rc_getNames(xExpr);
  SEXP names_new = rc_getNames(xNew);
  for (size_t col = 0; col < origData.nCol; ++col)
    SET_STRING_ELT(names_new, col, STRING_ELT(names_old, col));
  SET_STRING_ELT(names_new, origData.nCol, Rf_mkChar("orig.risk"));
  
  delete subsetDataPtr;
  delete [] subsetIndices;
  
  delete paramPtr;
  delete calculateRiskPtr;
 
  UNPROTECT(3);
  
  return xNew;
}

SEXP localSuppression(SEXP xExpr, SEXP riskFunctionExpr, SEXP paramExpr, SEXP skipRandomInitExpr)
{
  if (!Rf_isLogical(skipRandomInitExpr)) Rf_error("skipRandomInit parameter must be logical type");
  if (rc_getLength(skipRandomInitExpr) != 1) Rf_error("skipRandomInit parameter must be of length 1");
  bool skipRandomInit = LOGICAL(skipRandomInitExpr)[0];
  
  Data origData(xExpr);
  
  RiskFunction* calculateRiskPtr = NULL;
  try {
    if (riskFunctionExpr == R_NilValue) calculateRiskPtr = new KRiskFunction();
    else calculateRiskPtr = new DivRiskFunction(origData, riskFunctionExpr);
  } catch (const char* error) {
    origData.Data::~Data();
    Rf_error(error);
  }
  RiskFunction& calculateRisk(*calculateRiskPtr);
  
  MCMCParam* paramPtr = NULL;
  try {
    paramPtr = new MCMCParam(origData, riskFunctionExpr != R_NilValue ? calculateRiskPtr : NULL, paramExpr);
  } catch (const char* error) {
    delete calculateRiskPtr;
    origData.Data::~Data();
    Rf_error(error);
  }
  MCMCParam& param(*paramPtr);
  
  if (param.threshold >= 1.0 && param.threshold > static_cast<double>(origData.nRow)) {
    // no solution possible, NA everything
    State state(origData);
    
    for (size_t col = param.keyStartCol; col < origData.nCol; ++col) {
      for (size_t row = 0; row < origData.nRow; ++row)
        state.xt[col + row * origData.nCol] = NA_LEVEL;
    }
    
    SEXP result = packageSuppressionResult(origData, param, calculateRisk, state, xExpr);
    delete paramPtr;
    delete calculateRiskPtr;
    
    return result;
  }
  
  if (param.threshold < 1.0 && param.verbose > 0 && param.suppressValues != NULL) {
    Rprintf("created percent risk function, supresssing values:\n");
    for (size_t i = 0; i < origData.nLev[0]; ++i)
      if (param.suppressValues[i])
        Rprintf("  %s\n", origData.levelNames[0][i]);
  }
  
  
  size_t* subsetIndices = NULL;
  size_t subsetLength;
  Data* subsetDataPtr = subsetDataOnAtRiskAndSimilar(origData, param, calculateRisk, &subsetIndices, &subsetLength);
  Data& subsetData(*subsetDataPtr);
  
  if (subsetData.nRow == 0) {
    // already above risk threshold, just return
    delete subsetDataPtr;
    
    State state(origData);
    
    SEXP result = packageSuppressionResult(origData, param, calculateRisk, state, xExpr);
    delete paramPtr;
    delete calculateRiskPtr;    
    
    return result;
  }
   
  State subsetState(subsetData);
  
  GetRNGstate();
  
  bool randomInitFailed = false;
  if (!skipRandomInit) {
    randomInitFailed = !initializeStateWithRandomSuppressions(subsetData, param, calculateRisk, subsetState);
  } else {
    subsetState.minRisk = calculateRisk(subsetData, subsetState, NULL);
    subsetState.objective = getObjective(subsetData, param, subsetState.xt, subsetState.minRisk);
  }
  
  if (randomInitFailed) {
    State state(origData);
     
    for (size_t col = param.keyStartCol; col < origData.nCol; ++col) {
      for (size_t row = 0; row < origData.nRow; ++row)
        state.xt[col + row * origData.nCol] = NA_LEVEL;
    }
    
    SEXP result = packageSuppressionResult(origData, param, calculateRisk, state, xExpr);
    delete subsetDataPtr;
    delete [] subsetIndices;
    delete paramPtr;
    delete calculateRiskPtr;
      
    return result;
  }
  
  if (param.nSamp > 0) {
    State* randomState = mcmcSuppression(subsetData, param, calculateRisk, subsetState);
    
    // see if there are any NAs that we can safely restore without increasing k
    // greedy algorithm style
    pruneNAs(subsetData, param, calculateRisk, subsetState);
    if (randomState != NULL) {
      pruneNAs(subsetData, param, calculateRisk, *randomState);
      if (randomState->objective > subsetState.objective) subsetState.copyFrom(subsetData, *randomState);
      delete randomState;
    }
  } else {
    pruneNAs(subsetData, param, calculateRisk, subsetState);
  }
  
  PutRNGstate();
  
  State* fullState = mergeSubset(origData, subsetData, subsetState, subsetIndices, subsetLength); 
  
  delete subsetDataPtr;
  delete [] subsetIndices;
  
   if (randomInitFailed) {
    // no solution possible, NA everything
    for (size_t col = param.keyStartCol; col < origData.nCol; ++col) {
      for (size_t row = 0; row < origData.nRow; ++row) {
        fullState->xt[col + row * origData.nCol] = NA_LEVEL;
      }
    }
  } else {
    fullState->calculateFreqTable(origData);
  }
  
  SEXP result = packageSuppressionResult(origData, param, calculateRisk, *fullState, xExpr);
    
  delete fullState;
  delete paramPtr;
  delete calculateRiskPtr;
    
  return result;
}

}

namespace {
  SEXP packageSuppressionResult(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& state, SEXP xExpr)
  {
    // package up results into a list
    SEXP result = PROTECT(rc_newList(3));
    SEXP xNew = SET_VECTOR_ELT(result, 0, rc_newList(data.nCol + 1));
    for (size_t col = 0; col < data.nCol; ++col) {
      SEXP x_j_old = VECTOR_ELT(xExpr, col);
      SEXP x_j_new = SET_VECTOR_ELT(xNew, col, rc_newInteger(data.nRow));
      
      int* x_j = INTEGER(x_j_new);
      
      for (size_t row = 0; row < data.nRow; ++row)
        x_j[row] = state.xt[col + row * data.nCol] == NA_LEVEL ? R_NaInt : (state.xt[col + row * data.nCol] + 1);
      
      Rf_setAttrib(x_j_new, R_LevelsSymbol, rc_getLevels(x_j_old));
      Rf_setAttrib(x_j_new, R_ClassSymbol, rc_getClass(x_j_old));
    }
    
    // store risk
    SET_VECTOR_ELT(xNew, data.nCol, rc_newReal(data.nRow));
    
    double minRisk = calculateRisk(data, state, REAL(VECTOR_ELT(xNew, data.nCol)));
    double objective = getObjective(data, param, state.xt, minRisk);
    
    Rf_setAttrib(xNew, R_ClassSymbol, rc_getClass(xExpr));
    Rf_setAttrib(xNew, R_RowNamesSymbol, Rf_getAttrib(xExpr, R_RowNamesSymbol));
    
    // set colnames
    rc_setNames(xNew, PROTECT(rc_newCharacter(data.nCol + 1)));
    SEXP names_old = rc_getNames(xExpr);
    SEXP names_new = rc_getNames(xNew);
    for (size_t col = 0; col < data.nCol; ++col)
      SET_STRING_ELT(names_new, col, STRING_ELT(names_old, col));
    SET_STRING_ELT(names_new, data.nCol, Rf_mkChar("risk"));
    
    // store numeric results
    SET_VECTOR_ELT(result, 1, Rf_ScalarReal(objective));
    // rebuild NA term
    double naTerm = 0.0;
    for (size_t row = 0; row < data.nRow; ++row) {
      for (size_t col = 0; col < param.numKeyCols; ++col) {
        if (state.xt[col + row * data.nCol + param.keyStartCol] == NA_LEVEL &&
            data.xt[col + row * data.nCol + param.keyStartCol] != NA_LEVEL)
          naTerm += param.theta[col]; 
      }
    }
    naTerm /= static_cast<double>(data.nRow);
    SET_VECTOR_ELT(result, 2, Rf_ScalarReal(naTerm));
    
    rc_setNames(result, PROTECT(rc_newCharacter(2)));
    SEXP namesExpr = rc_getNames(result);
    SET_STRING_ELT(namesExpr, 0, Rf_mkChar("x"));
    SET_STRING_ELT(namesExpr, 1, Rf_mkChar("obj"));
    SET_STRING_ELT(namesExpr, 2, Rf_mkChar("n"));
    
    UNPROTECT(3);
  
    return result;
  }
  
  inline bool getRowAtRisk(const Data& data, const Param& param, size_t row, double risk) {
    return risk < param.threshold &&
      (param.suppressValues == NULL ? true :
        data.xt[row * data.nCol] != NA_LEVEL &&
        param.suppressValues[data.xt[row * data.nCol]] == false);
  }
  
  Data* subsetDataOnAtRiskAndSimilar(const Data& data, const Param& param, RiskFunction& calculateRisk,
                                     size_t** subsetIndicesPtr, size_t* subsetLengthPtr)
  {
    State state(data);
    
    state.calculateFreqTable(data);
    
    double* risk = new double[data.nRow];
    calculateRisk(data, state, risk);
    
    bool* rowAtRisk = new bool[data.nRow];
    for (size_t row = 0; row < data.nRow; ++row) {
      rowAtRisk[row] = getRowAtRisk(data, param, row, risk[row]);
    }
   
    
    size_t*& subsetIndices(*subsetIndicesPtr);
    size_t& subsetLength(*subsetLengthPtr);
    
    // we keep:
    //   those at risk
    //   those matching keys with those at risk (matters for % only)
    //   those similar to those at risk (who can be suppressed)
    subsetLength = 0;
    bool* keepRow = new bool[data.nRow];
    for (size_t row = 0; row < data.nRow; ++row) {
      if (risk[row] < param.threshold) { // same as at risk, unless cannot be suppressValues != NULL
        keepRow[row] = true;
        ++subsetLength;
        continue;
      }
      
      if (param.suppressValues != NULL &&
          data.xt[row * data.nCol] != NA_LEVEL &&
          param.suppressValues[data.xt[row * data.nCol]] == false)
      {
        keepRow[row] = false;
        continue;
      }
      
      keepRow[row] = false;
      // if it shares any column with any row at risk, keep it
      for (size_t atRiskRow = 0; atRiskRow < data.nRow && !keepRow[row]; ++atRiskRow) {
        if (atRiskRow == row || !rowAtRisk[atRiskRow]) continue;
        
        for (size_t col = param.keyStartCol; col < data.nCol; ++col) {
          if (data.xt[col + row * data.nCol] == data.xt[col + atRiskRow * data.nCol]) {
            keepRow[row] = true;
            ++subsetLength;
            break;
          }
        }
      }
    }
    
    delete [] risk;
    delete [] rowAtRisk;
    
    subsetIndices = new size_t[subsetLength];
    size_t subsetRow = 0;
    for (size_t row = 0; row < data.nRow; ++row)
      if (keepRow[row]) subsetIndices[subsetRow++] = row;
    
    delete [] keepRow;
    
    return new Data(data, subsetIndices, subsetLength);
  }
  
  State* mergeSubset(const Data& origData, const Data& subsetData, const State& subsetState, size_t* subsetIndices, size_t subsetLength)
  {
    State* result = new State(origData);
    
    size_t subsetRow = 0;
    for (size_t row = 0; row < origData.nRow; ++row) {
      if (subsetRow < subsetLength && subsetIndices[subsetRow] == row) {
        std::memcpy(result->xt + row * origData.nCol, subsetState.xt + subsetRow * origData.nCol, origData.nCol * sizeof(unsigned char));
        ++subsetRow;
      } else {
        std::memcpy(result->xt + row * origData.nCol, origData.xt + row * origData.nCol, origData.nCol * sizeof(unsigned char));
      }
    }
    
    return result;
  }
  
  // fills in probs_t as nCol x nRow
  void getAtRiskProbs(const Data& data, const MCMCParam& param, const State& state,
                      const bool* originallyAtRisk, const double* risk, double* probs_t);
  // fills in probs as nRow
  bool getNotAtRiskProbs(const Data& data, const MCMCParam& param, const State& state,
                         const bool* originallyAtRisk, size_t row_atRisk, size_t col_atRisk,
                         double* probs);
  
  bool initializeStateWithRandomSuppressions(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, State& state)
  {
    state.calculateFreqTable(data);
    
    double* risk = new double[data.nRow];
    
    state.minRisk = calculateRisk(data, state, risk);
    
    if (state.minRisk >= param.threshold) {
      state.objective = getObjective(data, param, state.xt, state.minRisk);
      delete [] risk;
      return true;
    }
    
    size_t numProbs = param.numKeyCols * data.nRow;
    double* probs_t = new double[numProbs];
    
    bool* originallyAtRisk = new bool[data.nRow];
    
    for (size_t row = 0; row < data.nRow; ++row)
      originallyAtRisk[row] = getRowAtRisk(data, param, row, risk[row]);
    
    size_t numSuppressions = 0;
    size_t numFailures = 0;
    size_t iter = 0;
    if (param.verbose > 0) {
      Rprintf("random initialization:\n  min risk at start: %.2f", state.minRisk);
      if (param.verbose > 1)
        Rprintf("\n");
    }
    
    do {
      ++iter;
      getAtRiskProbs(data, param, state, originallyAtRisk, risk, probs_t);
      
      // randomly pick a row at risk and a column from that row
      size_t index = rng_drawFromDiscreteDistribution(probs_t, numProbs);
      if (index == RNG_DISCRETE_DRAW_FAILURE) {
        /* Rprintf("failure at iter %lu\n", iter);
        for (size_t row = 0; row < data.nRow; ++row) {
          Rprintf("  ");
          Rprintf("%s", originallyAtRisk[row] ? "+" : " ");
          Rprintf("%s", getRowAtRisk(data, param, row, risk[row]) ? "*" : " ");
          Rprintf(" %.2f ", risk[row]);
          printObs(data, data.xt + row * data.nCol);
          Rprintf(" -> ");
          printObs(data, state.xt + row * data.nCol);
          Rprintf("\n");
        }*/
         
        delete [] originallyAtRisk;
        delete [] probs_t;
        delete [] risk;
        
        Rf_warning("random initialization failed with inability to find at risk rows - this is not normal");
        return false;
      }
 
      size_t row_atRisk = index / param.numKeyCols;
      size_t col_atRisk = index % param.numKeyCols;
      
      size_t dataCol_atRisk = col_atRisk + param.keyStartCol;
      
      if (param.verbose > 1) {
        Rprintf("  r: " SIZE_T_FMT ", c: " SIZE_T_FMT " ", row_atRisk + 1, dataCol_atRisk + 1);
        printObs(data, state.xt + row_atRisk * data.nCol);
        Rprintf(" [%.2f]", risk[row_atRisk]);
      }
      
      // randomly pick a row that matches on all but that column to NA out
      bool matchPossible = getNotAtRiskProbs(data, param, state, originallyAtRisk, row_atRisk, col_atRisk, probs_t);
      
      if (!matchPossible) {
        ++numFailures;
        if (param.verbose > 1) Rprintf(" - no match\n");
        continue;
      }
      
      size_t row_toNa = rng_drawFromDiscreteDistribution(probs_t, data.nRow);
      
      if (param.verbose > 1) {
        Rprintf(" - r: " SIZE_T_FMT " ", row_toNa + 1);
        printObs(data, state.xt + row_toNa * data.nCol);
        Rprintf("\n");
      }
      
      // na both out
      state.decrementFreqTable(data, state.xt + row_atRisk * data.nCol, 0, 0, 1, false);
      state.decrementFreqTable(data, state.xt + row_toNa   * data.nCol, 0, 0, 1, false);
      
      state.xt[dataCol_atRisk + row_atRisk * data.nCol] = NA_LEVEL;
      state.xt[dataCol_atRisk + row_toNa   * data.nCol] = NA_LEVEL;
      
      state.incrementFreqTable(data, state.xt + row_atRisk * data.nCol, 0, 0, 1, false);
      state.incrementFreqTable(data, state.xt + row_toNa   * data.nCol, 0, 0, 1, false);
      
      state.minRisk = calculateRisk(data, state, risk);
      
      ++numSuppressions;
    } while (state.minRisk < param.threshold && iter < MAX_RANDOM_INIT_ITERATIONS);
    
    bool result = iter != MAX_RANDOM_INIT_ITERATIONS || numFailures != MAX_RANDOM_INIT_ITERATIONS;
    
    if (param.verbose > 0) {
      if (result == true) {
        if (param.verbose == 1) Rprintf(", "); else Rprintf("  min risk ");
        Rprintf("at end: %.2f\n", state.minRisk);
        Rprintf("  iters: " SIZE_T_FMT ", suppressions: " SIZE_T_FMT ", failures: " SIZE_T_FMT "\n", iter, numSuppressions, numFailures);
      } else {
        if (param.verbose == 1) Rprintf(",  no solution found\n");
        else Rprintf("  at end: no solution found\n");
      }
    }
    
    state.objective = getObjective(data, param, state.xt, state.minRisk);
    
    delete [] originallyAtRisk;
    delete [] probs_t;
    delete [] risk;
    
    return result;
  }
  
  // Rows at risk are those with risk below the threshold and at least one column that is not
  // NA in the original data - this allows us to select a column that has already been NA'd out
  // once and find a new observation to pair with it.
  
  // Because replacing a value with an NA can considerably lower the risk associated with an
  // obs, we only allow the selection of a row with NAs in it if it was originally at risk
  void getAtRiskProbs(const Data& data, const MCMCParam& param, const State& state,
                      const bool* originallyAtRisk, const double* risk, double* probs_t) {
    size_t numProbs = param.numKeyCols * data.nRow;
    
    double total = 0.0;
    for (size_t row = 0; row < data.nRow; ++row) {
      if (getRowAtRisk(data, param, row, risk[row]) == false) {
        for (size_t col = 0; col < param.numKeyCols; ++col)
          probs_t[col + row * param.numKeyCols] = 0.0;
        continue;
      }
      
      bool rowCurrentlyHasNAs = false;
      if (!originallyAtRisk[row]) {
        for (size_t col = 0; col < param.numKeyCols; ++col) {
          if (state.xt[col + row * data.nCol + param.keyStartCol] == NA_LEVEL) {
            rowCurrentlyHasNAs = true;
            break;
          }
        }
      }
      
      for (size_t col = 0; col < param.numKeyCols; ++col) {
        if (rowCurrentlyHasNAs || data.xt[col + row * data.nCol + param.keyStartCol] == NA_LEVEL) {
          probs_t[col + row * param.numKeyCols] = 0.0;
        } else {
          probs_t[col + row * param.numKeyCols] = param.theta_inv[col];
          total += probs_t[col + row * param.numKeyCols];
        }
      }
    }
    if (total > 0.0) for (size_t i = 0; i < numProbs; ++i) probs_t[i] /= total;
  }
  
  bool getNotAtRiskProbs(const Data& data, const MCMCParam& param, const State& state,
                         const bool* originallyAtRisk, size_t row_atRisk, size_t col_atRisk,
                         double* probs)
  {
    // of those rows not at risk and matching the at-risk row in all but the one column, randomly pick one if one exists
    double total = 0.0;
    const unsigned char* xt_atRisk = data.xt + row_atRisk * data.nCol;
    size_t dataCol_atRisk = col_atRisk + param.keyStartCol;
    
    // first try and match those with no-NAs present
    for (size_t row = 0; row < data.nRow; ++row) {
      // if originally at risk, exclude for now
      if (originallyAtRisk[row]) {
        probs[row] = 0.0;
        continue;
      }
      
      // check that row matches row at risk in all but selected one
      const unsigned char* xt_i = state.xt + row * data.nCol;
      bool rowMatches = true;
      for (size_t col = param.keyStartCol; col < data.nCol; ++col) {
        if (col == dataCol_atRisk) continue;
        if (xt_atRisk[col] != xt_i[col]) {
          rowMatches = false;
          break;
        }
      }
      // check (if applicable) that row can be suppressed
      if (rowMatches && param.suppressValues != NULL)
       rowMatches &= data.xt[row * data.nCol] != NA_LEVEL && param.suppressValues[data.xt[row * data.nCol]];
      
      if (!rowMatches) {
        probs[row] = 0.0;
      } else {
        probs[row] = 1.0;
        total += 1.0;
      }
    }
        
    if (total != 0.0) {
      for (size_t row = 0; row < data.nRow; ++row) probs[row] /= total;
      return true;
    }
    
    // try again but match rows potentially with NAs
    for (size_t row = 0; row < data.nRow; ++row) {
      if (originallyAtRisk[row]) continue;
      
      const unsigned char* xt_i = state.xt + row * data.nCol;
      bool rowMatches = true, rowIsAllNA = true;
      for (size_t col = param.keyStartCol; col < data.nCol; ++col) {
        if (col == dataCol_atRisk) continue;
        if (xt_atRisk[col] != xt_i[col] && xt_i[col] != NA_LEVEL) {
          rowMatches = false;
          break;
        }
        rowIsAllNA &= xt_i[col] == NA_LEVEL;
      }
      rowMatches &= !rowIsAllNA;
      
      if (rowMatches && param.suppressValues != NULL)
       rowMatches &= data.xt[row * data.nCol] != NA_LEVEL && param.suppressValues[data.xt[row * data.nCol]];
      
      if (!rowMatches) {
        probs[row] = 0.0;
      } else {
        probs[row] = 1.0;
        total += 1.0;
      }
    }
        
    if (total != 0.0) {
      for (size_t row = 0; row < data.nRow; ++row) probs[row] /= total;
      return true;
    }
    
    // no row matches all but the one column we selected and is also not at risk
    // we might still gain by NAing out another at risk row so long as it is not identical to the
    // one selected
    for (size_t row = 0; row < data.nRow; ++row) {
      if (!originallyAtRisk[row] || row == row_atRisk) continue;
      
      const unsigned char* xt_i = state.xt + row * data.nCol;
      bool rowMatches = true;
      for (size_t col = param.keyStartCol; col < data.nCol; ++col) {
        // bad if match in selected col or differ in other cols
        if ((col == dataCol_atRisk && xt_atRisk[col] == xt_i[col]) ||
            (xt_atRisk[col] != xt_i[col]))
        {
          rowMatches = false;
          break;
        }
      }
      
      // check (if applicable) that row can be suppressed
      if (rowMatches && param.suppressValues != NULL)
       rowMatches &= data.xt[row * data.nCol] != NA_LEVEL && param.suppressValues[data.xt[row * data.nCol]];
      
      // total was 0, just need to handle case when they now match
      if (rowMatches) {
        probs[row] = 1.0;
        total += 1.0;
      }
    }
    
    if (total != 0.0) {
      for (size_t row = 0; row < data.nRow; ++row) probs[row] /= total;
      return true;
    }
        
    // try again but allow for matches with NA
    for (size_t row = 0; row < data.nRow; ++row) {
      if (!originallyAtRisk[row] || row == row_atRisk) continue;
      
      const unsigned char* xt_i = state.xt + row * data.nCol;
      bool rowMatches = true, rowIsAllNA = true;
      for (size_t col = param.keyStartCol; col < data.nCol; ++col) {
        // bad if match in selected col or differ in other cols
        if ((col == dataCol_atRisk && xt_atRisk[col] == xt_i[col]) ||
            (xt_atRisk[col] != xt_i[col] && xt_i[col] != NA_LEVEL))
        {
          rowMatches = false;
          break;
        }
        rowIsAllNA &= xt_i[col] == NA_LEVEL;
      }
      rowMatches &= !rowIsAllNA;
      
      // check (if applicable) that row can be suppressed
      if (rowMatches && param.suppressValues != NULL)
       rowMatches &= data.xt[row * data.nCol] != NA_LEVEL && param.suppressValues[data.xt[row * data.nCol]];
      
      // total was 0, just need to handle case when they now match
      if (rowMatches) {
        probs[row] = 1.0;
        total += 1.0;
      }
    }
    
    if (total == 0.0) return false;
    
    for (size_t row = 0; row < data.nRow; ++row) probs[row] /= total;
    return true;
  }
  
  struct MCMCScratch {
    size_t numProbs;
    double* cellProbs_t;
    double* rowProbs;
  };
  
  double rowSwapStep(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& curr, MCMCScratch& scratch, State& prop);
  double colSwapStep(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& curr, MCMCScratch& scratch, State& prop);
  double naRevStep(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& curr, MCMCScratch& scratch, State& prop);
  
  State* mcmcSuppression(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& initState)
  {
    State* result = new State(data);
    
    State curr(data);
    State prop(data);
    
    curr.copyFrom(data, initState);
    
    curr.minRisk = calculateRisk(data, curr, NULL);
    curr.objective = getObjective(data, param, curr.xt, curr.minRisk);
    
    MCMCScratch scratch;
    
    scratch.numProbs     = param.numKeyCols * data.nRow;
    scratch.cellProbs_t  = new double[scratch.numProbs];
    scratch.rowProbs     = new double[data.nRow];
    
    double r, ratio;
   
    if (param.verbose > 0) Rprintf("mcmc:\n  min risk at start: " SIZE_T_FMT "\n", curr.minRisk);
    
    int numDigits = 0;
    for (size_t temp = param.nSamp; temp > 0; temp /= 10) ++numDigits;
    
    for (size_t i = 0; i < param.nSamp; ++i) {
      if (param.verbose > 1) Rprintf("  %-*lu: ", numDigits, i + 1);
      
      prop.copyFrom(data, curr);
      
      r = unif_rand();
      
      if (r <= param.rowSwapProb) {
        ratio = rowSwapStep(data, param, calculateRisk, curr, scratch, prop);
      } else if (r <= param.rowSwapProb + param.colSwapProb) {
        ratio = colSwapStep(data, param, calculateRisk, curr, scratch, prop);
      } else {
        ratio = naRevStep(data, param, calculateRisk, curr, scratch, prop);
      }
                  
      if (-Rf_rexp(1.0) < ratio) {
        if (param.verbose > 1) Rprintf(" - accepted, min risk: " SIZE_T_FMT "\n", prop.minRisk);
        curr.copyFrom(data, prop);
        
        if ((i >= param.nBurn && curr.objective > result->objective && curr.minRisk >= param.threshold)) {
          if (param.verbose > 0) {
            if (param.verbose == 1)
              Rprintf("  %-*lu: ", numDigits, i + 1);
            else
              Rprintf("  %*s: ", numDigits, "");
            Rprintf("updating state from ");
            result->print(data, param);
            Rprintf(" to ");
            curr.print(data, param);
            Rprintf("\n");
          }
          
          result->copyFrom(data, curr);
        }
      } else {
       if (param.verbose > 1) Rprintf(" - rejected\n");
      }
      
      if (param.verbose == 1 && (i + 1) % 100 == 0) Rprintf("  iter: " SIZE_T_FMT "\n", i + 1);
    }
    if (param.verbose > 0) {
      if (result->objective == -HUGE_VAL)
        Rprintf("  no solution found\n");
      else
        Rprintf("  min risk at end: " SIZE_T_FMT "\n", result->minRisk);
    }
    
    delete [] scratch.cellProbs_t;
    delete [] scratch.rowProbs;
    
    if (result->objective == -HUGE_VAL) {
      delete result;
      result = NULL;
    }
    return result;
  }
  
  // puts positive probability on every NA row/col that isn't NA in original data
  bool getCurrentNAProbs(const Data& data, const MCMCParam& param, const State& state, MCMCScratch& scratch);
  // puts positive probability on every non-NA row/col that isn't NA in original data
  bool getCurrentNonNAProbs(const Data& data, const MCMCParam& param, const State& state, MCMCScratch& scratch);
  // puts positive probability on every row/col that isn't NA in original data
  // bool getRandomProbs(const Data& data, const Param& param, const State& state, MCMCScratch& scratch);
  
  bool getRowSwapProbs(const Data& data, const MCMCParam& param, const State& state, MCMCScratch& scratch, size_t targetRow, size_t targetDataCol);
  
  // among rows/cols with NAs, pick one and then shuffle that NA to another valid row
  double rowSwapStep(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& curr, MCMCScratch& scratch, State& prop)
  {
    if (param.verbose > 1) Rprintf("rs ");
    
    if (getCurrentNAProbs(data, param, curr, scratch) == false) {
      if (param.verbose > 1) Rprintf("- none good");
      return -HUGE_VAL;
    }
    
    size_t index     = rng_drawFromDiscreteDistribution(scratch.cellProbs_t, scratch.numProbs);
    size_t sourceRow = index / param.numKeyCols;
    size_t col       = index % param.numKeyCols;
    size_t dataCol   = col + param.keyStartCol;
    
    if (getRowSwapProbs(data, param, curr, scratch, sourceRow, dataCol) == false) {
      if (param.verbose > 1) Rprintf("- none good");
      return -HUGE_VAL;
    }
    
    size_t targetRow = rng_drawFromDiscreteDistribution(scratch.rowProbs, data.nRow);
    
    prop.decrementFreqTable(data, prop.xt + sourceRow * data.nCol, 0, 0, 1, false);
    prop.decrementFreqTable(data, prop.xt + targetRow * data.nCol, 0, 0, 1, false);
    
    prop.xt[dataCol + sourceRow * data.nCol] = data.xt[dataCol + sourceRow * data.nCol];
    prop.xt[dataCol + targetRow * data.nCol] = NA_LEVEL;
    
    prop.incrementFreqTable(data, prop.xt + sourceRow * data.nCol, 0, 0, 1, false);
    prop.incrementFreqTable(data, prop.xt + targetRow * data.nCol, 0, 0, 1, false);
    
    prop.minRisk = calculateRisk(data, prop, NULL);
    prop.objective = getObjective(data, param, prop.xt, prop.minRisk);
    
    double ratio = prop.objective - curr.objective;
    
    if (param.verbose > 1)
      Rprintf("%.3f / %.3f = %.3f",
              std::exp(prop.objective), std::exp(curr.objective), std::exp(ratio));
    
    return ratio;
  }
  
  // among rows/cols with NAs, pick one and then shuffle that NA to another valid col
  double colSwapStep(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& curr, MCMCScratch& scratch, State& prop)
  {
    if (param.verbose > 1) Rprintf("cs ");
    
    if (getCurrentNAProbs(data, param, curr, scratch) == false) {
      if (param.verbose > 1) Rprintf("- none good");
      return -HUGE_VAL;
    }
    
    size_t index     = rng_drawFromDiscreteDistribution(scratch.cellProbs_t, scratch.numProbs);
    size_t row       = index / param.numKeyCols;
    size_t sourceCol = index % param.numKeyCols;
    
    double* colProbs = new double[param.numKeyCols];
    
    double total = 0.0;
    for (size_t col = 0; col < param.numKeyCols; ++col) {
      if (col != sourceCol && curr.xt[col + row * data.nCol + param.keyStartCol] != NA_LEVEL) {
        colProbs[col] = 1.0;
        total += 1.0;
      } else {
        colProbs[col] = 0.0;
      }
    }
    if (total == 0.0) {
      if (param.verbose > 1) Rprintf("- none good");
      delete [] colProbs;
      return -HUGE_VAL;
    }
    
    for (size_t col = 0; col < param.numKeyCols; ++col) colProbs[col] /= total;
    
    size_t targetCol = rng_drawFromDiscreteDistribution(colProbs, param.numKeyCols);
        
    prop.decrementFreqTable(data, prop.xt + row * data.nCol, 0, 0, 1, false);
    
    prop.xt[sourceCol + row * data.nCol + param.keyStartCol] = data.xt[sourceCol + row * data.nCol + param.keyStartCol];
    prop.xt[targetCol + row * data.nCol + param.keyStartCol] = NA_LEVEL;
    
    prop.incrementFreqTable(data, prop.xt + row * data.nCol, 0, 0, 1, false);
    
    prop.minRisk = calculateRisk(data, prop, NULL);
    prop.objective = getObjective(data, param, prop.xt, prop.minRisk);
    
    double ratio = prop.objective - curr.objective;
    
    if (param.verbose > 1)
      Rprintf("%.3f / %.3f = %.3f",
              std::exp(prop.objective), std::exp(curr.objective), std::exp(ratio));
    
    delete [] colProbs;
    
    return ratio;
  }
  
  double naRevStep(const Data& data, const MCMCParam& param, RiskFunction& calculateRisk, const State& curr, MCMCScratch& scratch, State& prop)
  {
    double ratio;
    if (unif_rand() < param.naProb) {
      if (param.verbose > 1) Rprintf("na ");
      
      if (getCurrentNonNAProbs(data, param, curr, scratch) == false) {
        if (param.verbose > 1) Rprintf("- none good");
        return -HUGE_VAL;
      }
      
      size_t index = rng_drawFromDiscreteDistribution(scratch.cellProbs_t, scratch.numProbs);
      size_t row     = index / param.numKeyCols;
      size_t col     = index % param.numKeyCols;
      size_t dataCol = col + param.keyStartCol;
      
      double propProb = scratch.cellProbs_t[index];
      
      prop.decrementFreqTable(data, prop.xt + row * data.nCol, 0, 0, 1, false);
      prop.xt[dataCol + row * data.nCol] = NA_LEVEL;
      prop.incrementFreqTable(data, prop.xt + row * data.nCol, 0, 0, 1, false);
      
      prop.minRisk = calculateRisk(data, prop, NULL);
      prop.objective = getObjective(data, param, prop.xt, prop.minRisk);
      
      getCurrentNAProbs(data, param, prop, scratch);
      double currProb = scratch.cellProbs_t[index];
      
      ratio = (currProb == 0.0 ? HUGE_VAL :
                (prop.objective + std::log(currProb) + std::log(1.0 - param.naProb)) -
                (curr.objective + std::log(propProb) + std::log(param.naProb)));
      
      if (param.verbose > 1)
        Rprintf("(%.2f * %.4f * %.2f) / (%.2f * %.4f * %.2f)) = %.3f",
                std::exp(prop.objective), currProb, 1.0 - param.naProb,
                std::exp(curr.objective), propProb, param.naProb,
                currProb == 0.0 ? INFINITY : std::exp(ratio));
    } else {
      if (param.verbose > 1) Rprintf("nv ");
      
      if (getCurrentNAProbs(data, param, curr, scratch) == false) {
        if (param.verbose > 1) Rprintf("- none good");
        return -HUGE_VAL;
      }
        
      size_t index = rng_drawFromDiscreteDistribution(scratch.cellProbs_t, scratch.numProbs);
      size_t row     = index / param.numKeyCols;
      size_t col     = index % param.numKeyCols;
      size_t dataCol = col + param.keyStartCol;
        
      double propProb = scratch.cellProbs_t[index];
      
      prop.decrementFreqTable(data, prop.xt + row * data.nCol, 0, 0, 1, false);
      prop.xt[dataCol + row * data.nCol] = data.xt[dataCol + row * data.nCol];
      prop.incrementFreqTable(data, prop.xt + row * data.nCol, 0, 0, 1, false);
      
      prop.minRisk = calculateRisk(data, prop, NULL);
      prop.objective = getObjective(data, param, prop.xt, prop.minRisk);
      
      getCurrentNonNAProbs(data, param, prop, scratch);
      double currProb = scratch.cellProbs_t[index];
      
      ratio = (currProb == 0.0 ? HUGE_VAL :
                (prop.objective + std::log(currProb) + std::log(param.naProb)) -
                (curr.objective + std::log(propProb) + std::log(1.0 - param.naProb)));
      
      if (param.verbose > 1)
        Rprintf("(%.2f * %.4f * %.2f) / (%.2f * %.4f * %.2f)) = %.3f",
                std::exp(prop.objective), currProb, param.naProb,
                std::exp(curr.objective), propProb, 1.0 - param.naProb,
                currProb == 0.0 ? INFINITY : std::exp(ratio));
    }
    return ratio;
  }
  
  bool getRowSwapProbs(const Data& data, const MCMCParam& param, const State& state, MCMCScratch& scratch, size_t targetRow, size_t targetDataCol)
  {
    double total = 0.0;
    for (size_t row = 0; row < data.nRow; ++row) {
      if (state.xt[targetDataCol + row * data.nCol] != NA_LEVEL && row != targetRow) {
        scratch.rowProbs[row] = 1.0;
        total += 1.0;
      } else {
        scratch.rowProbs[row] = 0.0;
      }
    }
    
    if (total == 0.0) return false;
    
    for (size_t i = 0; i < data.nRow; ++i) scratch.rowProbs[i] /= total;
    return true;
  } 
  
  bool getCurrentNAProbs(const Data& data, const MCMCParam& param, const State& state, MCMCScratch& scratch)
  {
    double total = 0.0;
    for (size_t row = 0; row < data.nRow; ++row) {
      for (size_t col = 0; col < param.numKeyCols; ++col) {
        if (state.xt[col + row * data.nCol + param.keyStartCol] == NA_LEVEL &&
            data.xt [col + row * data.nCol + param.keyStartCol] != NA_LEVEL)
        {
          scratch.cellProbs_t[col + row * param.numKeyCols] = param.theta_inv[col];
          total += scratch.cellProbs_t[col + row * param.numKeyCols];
        } else {
          scratch.cellProbs_t[col + row * param.numKeyCols] = 0.0;
        }
      }
    }
    
    if (total == 0.0) return false;
    
    for (size_t i = 0; i < scratch.numProbs; ++i) scratch.cellProbs_t[i] /= total;
    return true;
  }
  
  bool getCurrentNonNAProbs(const Data& data, const MCMCParam& param, const State& state, MCMCScratch& scratch)
  {
    double total = 0.0;
    for (size_t row = 0; row < data.nRow; ++row) {
      for (size_t col = 0; col < param.numKeyCols; ++col) {
        if (state.xt[col + row * data.nCol + param.keyStartCol] != NA_LEVEL &&
            data.xt [col + row * data.nCol + param.keyStartCol] != NA_LEVEL)
        {
          scratch.cellProbs_t[col + row * param.numKeyCols] = param.theta_inv[col];
          total += scratch.cellProbs_t[col + row * param.numKeyCols];
        } else {
          scratch.cellProbs_t[col + row * param.numKeyCols] = 0.0;
        }
      }
    }
    
    if (total == 0.0) return false;
    
    for (size_t i = 0; i < scratch.numProbs; ++i) scratch.cellProbs_t[i] /= total;
    return true;
  }
    
  void pruneNAs(const Data& data, const Param& param, RiskFunction& calculateRisk, State& state)
  {
    State temp(data);
    temp.copyFrom(data, state);
    
    size_t numIndices = param.numKeyCols * data.nRow;
    
    size_t* indices = new size_t[numIndices];
    for (size_t index = 0; index < numIndices; ++index) indices[index] = index;
    
    rng_permuteIndexArray(indices, numIndices);
    
    for (size_t index = 0; index < numIndices; ++index) {
      size_t row = indices[index] / param.numKeyCols;
      size_t col = indices[index] % param.numKeyCols;
      size_t keyCol = col + param.keyStartCol;
      
      if (state.xt[keyCol + row * data.nCol] == NA_LEVEL && data.xt[keyCol + row * data.nCol] != NA_LEVEL) {
        temp.decrementFreqTable(data, temp.xt + row * data.nCol, 0, 0, 1, false);
        
        temp.xt[keyCol + row * data.nCol] = data.xt[keyCol + row * data.nCol];
        
        temp.incrementFreqTable(data, temp.xt + row * data.nCol, 0, 0, 1, false);
         
        temp.minRisk = calculateRisk(data, temp, NULL);
        
        if (temp.minRisk >= param.threshold) {
          state.copyFrom(data, temp);
        } else {
          temp.copyFrom(data, state);
        }
      }
    }
    
    delete [] indices;
  }
  
  // 2 * (1 - gamma) * -log(mean(naTerm)) + log(kTerm)
  // mean(naTerm) is weighted average of number of NAs in data
  // riskTerm is a gamma density
  double getObjective(const Data& data, const MCMCParam& param, const unsigned char* xt, double minRisk)
  {
    double naTerm = 0.0;
    size_t dataStartCol = param.riskType != rsupp::RTYPE_COUNT ? 1 : 0;
    size_t numProbCols = data.nCol - dataStartCol;
    for (size_t row = 0; row < data.nRow; ++row) {
      for (size_t col = 0; col < numProbCols; ++col) if (xt[col + row * data.nCol + dataStartCol] == NA_LEVEL) naTerm += param.theta[col]; 
    }
    naTerm /= static_cast<double>(data.nRow);
    
    double riskTerm = (param.threshold >= 1.0 ?
     (param.alpha - 1.0) * std::log(minRisk) - param.beta * minRisk :
     param.alpha * std::log(minRisk) + param.beta * std::log(1.0 - minRisk));
    
    return param.gamma * riskTerm + 2.0 * (1.0 - param.gamma) * -std::log(naTerm);
  }
}
namespace rsupp {
  void printObs(const Data& data, const unsigned char* x_i)
  {
    Rprintf("(");
    if (data.levelNames == NULL) {
      if (x_i[0] == NA_LEVEL) Rprintf("NA"); else Rprintf("%hu", x_i[0]);
      
      for (size_t col = 1; col < data.nCol; ++col)
        if (x_i[col] == NA_LEVEL) Rprintf("NA"); else Rprintf(", %hu", x_i[col]);
      
    } else {
      if (x_i[0] == NA_LEVEL) Rprintf("NA"); else if (data.levelNames[0] != NULL) Rprintf("%s", data.levelNames[0][x_i[0]]);
      else Rprintf("%hu", x_i[0]);
      
      for (size_t col = 1; col < data.nCol; ++col) {
        if (x_i[col] == NA_LEVEL) Rprintf(", NA"); else if (data.levelNames[col] != NULL) Rprintf(", %s", data.levelNames[col][x_i[col]]);
        else Rprintf(", %hu", x_i[col]);
      }
    }
    Rprintf(")");
  }
}

extern "C" {

#define DEF_FUNC(_N_, _F_, _A_) { _N_, reinterpret_cast<DL_FUNC>(&_F_), _A_ }
  
  static R_CallMethodDef R_callMethods[] = {
    DEF_FUNC("localSuppression", localSuppression, 4),
    DEF_FUNC("calcRisk", calcRisk, 2),
    DEF_FUNC("getAtRiskSubset", getAtRiskSubset, 3),
    { NULL, NULL, 0 }
  };
  
#undef DEF_FUNC
  
  void R_init_rsupp(DllInfo* info)
  {
    R_registerRoutines(info, NULL, R_callMethods, NULL, NULL);
    R_useDynamicSymbols(info, static_cast<Rboolean>(FALSE));
  }
  
}
