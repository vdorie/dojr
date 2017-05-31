#include "param.hpp"

#include <cstring> // std::strcmp, std::memcpy

#include "rc.h"

#include "data.hpp"
#include "definitions.hpp"
#include "riskFunction.hpp"
#include "state.hpp"

namespace rsupp {
  bool* getSuppressValues(const Data& fullData, RiskFunction& calculateRisk);
  
  Param::Param(const Data& data, RiskFunction* divRiskFunction, SEXP paramExpr) :
    threshold(-1.0), alpha(-1.0), beta(-1.0), gamma(-1.0), theta(NULL), theta_inv(NULL),
    rowSwapProb(-1.0), colSwapProb(-1.0), naProb(-1.0),
    nBurn(INVALID_EXTENT), nSamp(INVALID_EXTENT), verbose(0), skipRandomInit(false),
    riskType(RTYPE_INVALID), suppressValues(NULL)
  {
    if (!Rf_isVector(paramExpr)) Rf_error("params argument must be a named list");
    
    SEXP paramNames = rc_getNames(paramExpr);
    if (paramNames == R_NilValue) Rf_error("params argument must have names");
    
    double* theta = NULL;
    for (size_t i = 0; i < rc_getLength(paramExpr); ++i) {
      SEXP param_i = VECTOR_ELT(paramExpr, i);
      const char* param_name_i = CHAR(STRING_ELT(paramNames, i));
      
      if (std::strcmp(param_name_i, "risk.k") == 0) {
        if (!Rf_isReal(param_i)) Rf_error("risk.k parameter must be real type");
        if (rc_getLength(param_i) != 1) Rf_error("risk.k parameter must be of length 1");
        if (REAL(param_i)[0] <= 0.0) Rf_error("risk.k parameter must be non-negative");
        
        threshold = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "alpha") == 0) {
        if (!Rf_isReal(param_i)) Rf_error("alpha parameter must be double type");
        if (rc_getLength(param_i) != 1) Rf_error("alpha parameter must be of length 1");
        if (REAL(param_i)[0] <= 1.0) Rf_error("alpha parameter must be a greater than 1");
        
        alpha = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "gamma") == 0) {
        if (!Rf_isReal(param_i)) Rf_error("gamma parameter must be double type");
        if (rc_getLength(param_i) != 1) Rf_error("gamma parameter must be of length 1");
        if (REAL(param_i)[0] < 0.0 || REAL(param_i)[0] > 1.0) Rf_error("gamma parameter must be in [0,1]");
        
        gamma = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "theta") == 0) {
        if (!Rf_isReal(param_i)) Rf_error("theta parameter must be double type");
        if (rc_getLength(param_i) != data.nCol) Rf_error("theta parameter must be of length equal to number of columns in x");
        for (size_t i = 0; i < data.nCol; ++i) if (REAL(param_i)[i] < 0.0) Rf_error("theta parameter must be positive");
        
        theta = REAL(param_i);
      } else if (std::strcmp(param_name_i, "rowSwap.prob") == 0) {
        if (!Rf_isReal(param_i)) Rf_error("rowSwap.prob parameter must be double type");
        if (rc_getLength(param_i) != 1) Rf_error("rowSwap.prob parameter must be of length 1");
        if (REAL(param_i)[0] < 0.0 || REAL(param_i)[0] > 1.0) Rf_error("rowSwap.prob must be in [0, 1]");
        
        rowSwapProb = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "colSwap.prob") == 0) {
        if (!Rf_isReal(param_i)) Rf_error("colSwap.prob parameter must be double type");
        if (rc_getLength(param_i) != 1) Rf_error("colSwap.prob parameter must be of length 1");
        if (REAL(param_i)[0] < 0.0 || REAL(param_i)[0] > 1.0) Rf_error("colSwap.prob must be in [0, 1]");
        
        colSwapProb = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "na.prob") == 0) {
        if (!Rf_isReal(param_i)) Rf_error("na.prob parameter must be double type");
        if (rc_getLength(param_i) != 1) Rf_error("na.prob parameter must be of length 1");
        if (REAL(param_i)[0] < 0.0 || REAL(param_i)[0] > 1.0) Rf_error("na.prob must be in [0, 1]");
        
        naProb = REAL(param_i)[0];
      } else if (std::strcmp(param_name_i, "n.burn") == 0) {
        if (!Rf_isInteger(param_i)) Rf_error("nBurn parameter must be integer type");
        if (rc_getLength(param_i) != 1) Rf_error("nBurn parameter must be of length 1");
        if (INTEGER(param_i)[0] < 0) Rf_error("nBurn parameter must be a non-negative integer");
        
        nBurn = static_cast<size_t>(INTEGER(param_i)[0]);
      } else if (std::strcmp(param_name_i, "n.samp") == 0) {
        if (!Rf_isInteger(param_i)) Rf_error("nSamp parameter must be integer type");
        if (rc_getLength(param_i) != 1) Rf_error("nSamp parameter must be of length 1");
        if (INTEGER(param_i)[0] < 0) Rf_error("nSamp parameter must be a non-negative integer");
        
        nSamp = static_cast<size_t>(INTEGER(param_i)[0]);
      } else if (std::strcmp(param_name_i, "verbose") == 0) {
        if (!Rf_isInteger(param_i)) Rf_error("verbose parameter must be integer type");
        if (rc_getLength(param_i) != 1) Rf_error("verbose parameter must be of length 1");
        if (INTEGER(param_i) < 0) Rf_error("verbose parameter must be a non-negative integer");
        
        verbose = static_cast<uint8_t>(INTEGER(param_i)[0]);
      } else if (std::strcmp(param_name_i, "skip.rinit") == 0) {
        if (!Rf_isLogical(param_i)) Rf_error("skip.rinit parameter must be logical type");
        if (rc_getLength(param_i) != 1) Rf_error("skip.rinit parameter must be of length 1");
        
        skipRandomInit = static_cast<bool>(INTEGER(param_i)[0]);
      } else {
        Rf_error("unrecognized parameter '%s'", param_name_i);
      }
    }
    
    if (threshold == -1.0) Rf_error("risk.k parameter unset");
    if (alpha == -1.0) Rf_error("alpha parameter unset");
    if (gamma == -1.0) Rf_error("gamma parameter unset");
    if (theta == NULL) Rf_error("theta parameter unset");
    if (rowSwapProb == -1.0) Rf_error("rowSwap.prob parameter unset");
    if (colSwapProb == -1.0) Rf_error("colSwap.prob parameter unset");
    if (naProb == -1.0) Rf_error("na.prob parameter unset");
    if (nBurn == INVALID_EXTENT) Rf_error("n.burn parameter unset");
    if (nSamp == INVALID_EXTENT) Rf_error("n.samp parameter unset");
    
    
    if (nSamp < nBurn) Rf_error("n.samp must be greater than or equal to n.burn");
    if (threshold > data.nRow) Rf_error("threshold is greater than the number of rows in the data");
    if (rowSwapProb + colSwapProb > 1.0) Rf_error("rowSwap + colSwap probabilities must be less than or equal to 1");
    
    if (divRiskFunction == NULL) {
      riskType = rsupp::RTYPE_COUNT;
    } else if (threshold >= 1.0) {
      riskType = rsupp::RTYPE_DIVERSITY;
    } else {
      riskType = rsupp::RTYPE_PERCENT;
      
      suppressValues = getSuppressValues(data, *divRiskFunction);
      if (suppressValues == NULL)
        Rf_error("could not find values to suppress for threshold < 1.0");
    }
    
    if (threshold >= 1.0) {
      beta = (alpha - 1.0) / threshold;
    } else {
      beta = alpha / threshold - alpha;
    }
    
    this->theta = new double[data.nCol];
    std::memcpy(this->theta, const_cast<const double*>(theta), sizeof(double) * data.nCol);
    this->theta_inv = new double[data.nCol];
    
    double thetaTotal = 0.0;
    // normalize theta as a penalty weight so that the sum across a row = num cols
    for (size_t i = 0; i < data.nCol; ++i)
      if (R_finite(theta[i])) thetaTotal += theta[i];
    for (size_t i = 0; i < data.nCol; ++i)
      if (R_finite(theta[i])) this->theta[i] *= static_cast<double>(data.nCol) / thetaTotal;
    
    thetaTotal = 0.0;
    // normalize theta_inv as a probability
    for (size_t i = 0; i < data.nCol; ++i) {
      if (R_finite(theta[i])) {
        this->theta_inv[i] = 1.0 / this->theta[i];
        thetaTotal += this->theta_inv[i];
      } else {
        this->theta_inv[i] = 0.0;
      }
    }
    for (size_t i = 0; i < data.nCol; ++i) this->theta_inv[i] /= thetaTotal;
  }
    
  Param::~Param() {
    if (theta != NULL) {
      delete [] theta;
      theta = NULL;
    }
    if (theta_inv != NULL) {
      delete [] theta_inv;
      theta_inv = NULL;
    }
    if (suppressValues != NULL) {
      delete [] suppressValues;
      suppressValues = NULL;
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
      
      result[i] = risk < 1.0;
      suppressAny |= result[i];
    }
    
    if (!suppressAny) {
      delete [] result;
      return NULL;
    }
    
    return result;
  }
}
