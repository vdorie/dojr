#include "param.hpp"

#include <cstring> // std::strcmp, std::memcpy

#include "rc.h"

#include "data.hpp"
#include "definitions.hpp"
#include "riskFunction.hpp"
#include "state.hpp"

namespace rsupp {
  bool* getSuppressValues(const Data& fullData, RiskFunction& calculateRisk);
  
  Param::Param(const Data& data, RiskFunction* divRiskFunction, double _threshold, uint8_t _verbose) : 
    threshold(_threshold), riskType(RTYPE_INVALID), suppressValues(NULL), verbose(_verbose),
    numKeyCols(INVALID_EXTENT), keyStartCol(INVALID_EXTENT)
  {
    if (divRiskFunction == NULL) {
      riskType = rsupp::RTYPE_COUNT;
    } else if (threshold >= 1.0) {
      riskType = rsupp::RTYPE_DIVERSITY;
    } else {
      riskType = rsupp::RTYPE_PERCENT;
      
      suppressValues = getSuppressValues(data, *divRiskFunction);
      if (suppressValues == NULL)
        throw "could not find values to suppress for threshold < 1.0";
    }
    
    keyStartCol = riskType != RTYPE_COUNT ? 1 : 0;
    numKeyCols = data.nCol - keyStartCol;
  }
  
  Param::~Param() {
    if (suppressValues != NULL) {
      delete [] suppressValues;
      suppressValues = NULL;
    }
  }
  
  MCMCParam::MCMCParam(const Data& data, RiskFunction* divRiskFunction, SEXP paramExpr) :
    Param(data, divRiskFunction, -1.0, 0), alpha(-1.0), beta(-1.0), gamma(-1.0),
    theta(NULL), theta_inv(NULL),
    rowSwapProb(-1.0), colSwapProb(-1.0), naProb(-1.0),
    nBurn(INVALID_EXTENT), nSamp(INVALID_EXTENT)
  {
    if (!Rf_isVector(paramExpr)) throw "params argument must be a named list";
    
    SEXP paramNames = rc_getNames(paramExpr);
    if (paramNames == R_NilValue) throw "params argument must have names";
    
    double* theta = NULL;
    for (size_t i = 0; i < rc_getLength(paramExpr); ++i) {
      SEXP param_i = VECTOR_ELT(paramExpr, i);
      const char* param_name_i = CHAR(STRING_ELT(paramNames, i));
      
      if (std::strcmp(param_name_i, "risk.k") == 0) {
        if (!Rf_isReal(param_i)) throw "risk.k parameter must be real type";
        if (rc_getLength(param_i) != 1) throw "risk.k parameter must be of length 1";
        if (REAL(param_i)[0] <= 0.0) throw "risk.k parameter must be non-negative";
        
        threshold = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "alpha") == 0) {
        if (!Rf_isReal(param_i)) throw "alpha parameter must be double type";
        if (rc_getLength(param_i) != 1) throw "alpha parameter must be of length 1";
        if (REAL(param_i)[0] <= 1.0) throw "alpha parameter must be a greater than 1";
        
        alpha = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "gamma") == 0) {
        if (!Rf_isReal(param_i)) throw "gamma parameter must be double type";
        if (rc_getLength(param_i) != 1) throw "gamma parameter must be of length 1";
        if (REAL(param_i)[0] < 0.0 || REAL(param_i)[0] > 1.0) throw "gamma parameter must be in [0,1]";
        
        gamma = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "theta") == 0) {
        if (!Rf_isReal(param_i)) throw "theta parameter must be double type";
        if (rc_getLength(param_i) != numKeyCols) throw "theta parameter must be of length equal to number of key variables";
        for (size_t i = 0; i < numKeyCols; ++i) if (REAL(param_i)[i] < 0.0) throw "theta parameter must be positive";
        
        theta = REAL(param_i);
      } else if (std::strcmp(param_name_i, "rowSwap.prob") == 0) {
        if (!Rf_isReal(param_i)) throw "rowSwap.prob parameter must be double type";
        if (rc_getLength(param_i) != 1) throw "rowSwap.prob parameter must be of length 1";
        if (REAL(param_i)[0] < 0.0 || REAL(param_i)[0] > 1.0) throw "rowSwap.prob must be in [0, 1]";
        
        rowSwapProb = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "colSwap.prob") == 0) {
        if (!Rf_isReal(param_i)) throw "colSwap.prob parameter must be double type";
        if (rc_getLength(param_i) != 1) throw "colSwap.prob parameter must be of length 1";
        if (REAL(param_i)[0] < 0.0 || REAL(param_i)[0] > 1.0) throw "colSwap.prob must be in [0, 1]";
        
        colSwapProb = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "na.prob") == 0) {
        if (!Rf_isReal(param_i)) throw "na.prob parameter must be double type";
        if (rc_getLength(param_i) != 1) throw "na.prob parameter must be of length 1";
        if (REAL(param_i)[0] < 0.0 || REAL(param_i)[0] > 1.0) throw "na.prob must be in [0, 1]";
        
        naProb = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "n.burn") == 0) {
        if (!Rf_isInteger(param_i)) throw "nBurn parameter must be integer type";
        if (rc_getLength(param_i) != 1) throw "nBurn parameter must be of length 1";
        if (INTEGER(param_i)[0] < 0) throw "nBurn parameter must be a non-negative integer";
        
        nBurn = static_cast<size_t>(INTEGER(param_i)[0]);
      } else if (std::strcmp(param_name_i, "n.samp") == 0) {
        if (!Rf_isInteger(param_i)) throw "nSamp parameter must be integer type";
        if (rc_getLength(param_i) != 1) throw "nSamp parameter must be of length 1";
        if (INTEGER(param_i)[0] < 0) throw "nSamp parameter must be a non-negative integer";
        
        nSamp = static_cast<size_t>(INTEGER(param_i)[0]);
      } else if (std::strcmp(param_name_i, "verbose") == 0) {
        if (!Rf_isInteger(param_i)) throw "verbose parameter must be integer type";
        if (rc_getLength(param_i) != 1) throw "verbose parameter must be of length 1";
        if (INTEGER(param_i) < 0) throw "verbose parameter must be a non-negative integer";
        
        verbose = static_cast<uint8_t>(INTEGER(param_i)[0]);
      } else {
        throw "unrecognized parameter";
      }
    }
    
    if (threshold == -1.0) throw "risk.k parameter unset";
    if (alpha == -1.0) throw "alpha parameter unset";
    if (gamma == -1.0) throw "gamma parameter unset";
    if (theta == NULL) throw "theta parameter unset";
    if (rowSwapProb == -1.0) throw "rowSwap.prob parameter unset";
    if (colSwapProb == -1.0) throw "colSwap.prob parameter unset";
    if (naProb == -1.0) throw "na.prob parameter unset";
    if (nBurn == INVALID_EXTENT) throw "n.burn parameter unset";
    if (nSamp == INVALID_EXTENT) throw "n.samp parameter unset";
    
    
    if (nSamp < nBurn) throw "n.samp must be greater than or equal to n.burn";
    if (threshold > data.nRow) throw "threshold is greater than the number of rows in the data";
    if (rowSwapProb + colSwapProb > 1.0) throw "rowSwap + colSwap probabilities must be less than or equal to 1";
        
    if (threshold >= 1.0) {
      beta = (alpha - 1.0) / threshold;
    } else {
      beta = alpha / threshold - alpha;
    }
    
    this->theta = new double[numKeyCols];
    std::memcpy(this->theta, const_cast<const double*>(theta), sizeof(double) * numKeyCols);
    this->theta_inv = new double[numKeyCols];
    
    double thetaTotal = 0.0;
    // normalize theta as a penalty weight so that the sum across a row = num cols
    for (size_t i = 0; i < numKeyCols; ++i)
      if (R_finite(theta[i])) thetaTotal += theta[i];
    for (size_t i = 0; i < numKeyCols; ++i)
      if (R_finite(theta[i])) this->theta[i] *= static_cast<double>(numKeyCols) / thetaTotal;
    
    thetaTotal = 0.0;
    // normalize theta_inv as a probability
    for (size_t i = 0; i < numKeyCols; ++i) {
      if (R_finite(theta[i])) {
        this->theta_inv[i] = 1.0 / this->theta[i];
        thetaTotal += this->theta_inv[i];
      } else {
        this->theta_inv[i] = 0.0;
      }
    }
    for (size_t i = 0; i < numKeyCols; ++i) this->theta_inv[i] /= thetaTotal;
  }
    
  MCMCParam::~MCMCParam() {
    if (theta != NULL) {
      delete [] theta;
      theta = NULL;
    }
    if (theta_inv != NULL) {
      delete [] theta_inv;
      theta_inv = NULL;
    }
  }
  
  bool* getSuppressValues(const Data& fullData, RiskFunction& calculateRisk)
  {
    size_t subsetLength = 1;
    size_t subsetIndices = 0;
    
    // subset down to first row
    Data data(fullData, &subsetIndices, subsetLength);
    State state(data);
    
    // just need temp values
    for (size_t col = 0; col < data.nCol; ++col)
      if (state.xt[col] == NA_LEVEL) state.xt[col] = 0;
    
    
    double risk;
    bool* result = new bool[data.nLev[0]];
    bool suppressAny = false;
    
    // fiddle with diversity column
    for (unsigned char i = 0; i < data.nLev[0]; ++i) {
      state.xt[0] = i;
      state.incrementFreqTable(data, state.xt, 0, 0, 1, false);
      calculateRisk(data, state, &risk);
      state.decrementFreqTable(data, state.xt, 0, 0, 1, false);
      
      result[i] = risk > 0.0;
      suppressAny |= result[i];
    }
    
    if (!suppressAny) {
      delete [] result;
      return NULL;
    }
    
    return result;
  }
}
