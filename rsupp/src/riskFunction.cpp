#include "riskFunction.hpp"

#include "rc.h"

#define R_NO_REMAP
#include <Rinternals.h>
#undef R_NO_REMAP

#include "data.hpp"
#include "definitions.hpp"
#include "state.hpp"

using std::size_t;

namespace rsupp {
  double KRiskFunction::operator()(const Data& data, const State& state, double* risk) {
    double minK = static_cast<double>(data.nRow);
    
    const unsigned char* xt = state.xt;
    for (size_t row = 0; row < data.nRow; ++row) {
      double k_i = state.getKFromTable(data, xt, 0, 0, 1);
            
      if (k_i < minK) minK = k_i;
      
      xt += data.nCol;
      
      if (risk != NULL) risk[row] = k_i;
    }
    
    return minK;
  }

  DivRiskFunction::DivRiskFunction(const Data& data, SEXP riskFunction) {
    if (rc_getLength(riskFunction) != 2) throw "length of riskMeasure for functions must be 2";
    
    SEXP function = VECTOR_ELT(riskFunction, 0);
    environment = VECTOR_ELT(riskFunction, 1);
    
    if (!Rf_isFunction(function)) throw "first element of list for function riskMeasure must be a function";
    if (!Rf_isEnvironment(environment)) throw "second element of list for function riskMeasure must be an environment";
    
    SEXP freq = PROTECT(rc_newInteger(data.nLev[0]));
    SEXP freqNames = PROTECT(rc_newCharacter(data.nLev[0]));
    for (size_t i = 0; i < data.nLev[0]; ++i)
      SET_STRING_ELT(freqNames, i, Rf_mkChar(data.levelNames[0][i]));
    rc_setNames(freq, freqNames);
    
    closure = PROTECT(Rf_lang2(function, freq));
    
    freq_int = INTEGER(freq);
    numFrequencies = data.nLev[0];
  }
  
  DivRiskFunction::~DivRiskFunction() {
    UNPROTECT(3);
  }
    
  double DivRiskFunction::operator()(const Data& data, const State& state, double* risk)
  {
    const unsigned char* xt = state.xt;
    double minRisk = static_cast<double>(data.nRow);
    
    for (size_t row = 0; row < data.nRow; ++row) {
      double risk_i = state.getDivFromTable(data, xt, 1, 0, data.nLev[0], *this);
      if (risk_i < minRisk) minRisk = risk_i;
      
      xt += data.nCol;
      
      if (risk != NULL) risk[row] = risk_i;
    }
    
    return minRisk;
  }
  
  double DivRiskFunction::operator()(const size_t* naFreq, const size_t* ccFreq) {
    for (size_t i = 0; i < numFrequencies; ++i) freq_int[i] = static_cast<int>(ccFreq[i] > 0 ? naFreq[i] : 0);
    
    SEXP result = Rf_eval(closure, environment);
    if (Rf_isInteger(result)) return static_cast<double>(INTEGER(result)[0]);
     
    return REAL(result)[0];
  }
}
