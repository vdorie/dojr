#include "riskFunction.hpp"

#include <cstring> // std::memcpy
#include <cmath>   // HUGE_VAL

#include "rc.h"

#define R_NO_REMAP
#include <Rinternals.h>
#undef R_NO_REMAP

#include "data.hpp"
#include "definitions.hpp"
#include "state.hpp"

using std::size_t;

namespace rsupp {
  double KRiskFunction::operator()(const Data& data, const State& state, double* risk)
  {
    double minK = static_cast<double>(data.nRow);
    
    const unsigned char* xt = state.xt;
    for (size_t row = 0; row < data.nRow; ++row) {
      double k_i = state.getKFromTable(data, xt);
            
      if (k_i < minK) minK = k_i;
      
      xt += data.nCol;
      
      if (risk != NULL) risk[row] = k_i;
    }
    
    return minK;
  }

  DivRiskFunction::DivRiskFunction(const Data& data, SEXP riskFunction, bool _naRiskWithin) : naRiskWithin(_naRiskWithin) {
    if (rc_getLength(riskFunction) != 2) throw "length of riskMeasure for functions must be 2";
    
    SEXP function = VECTOR_ELT(riskFunction, 0);
    environment = VECTOR_ELT(riskFunction, 1);
    
    if (!Rf_isFunction(function)) throw "first element of list for function riskMeasure must be a function";
    if (!Rf_isEnvironment(environment)) throw "second element of list for function riskMeasure must be an environment";
    
    SEXP freq = PROTECT(rc_newInteger(data.nLev[0] + 1));
    SEXP freqNames = PROTECT(rc_newCharacter(data.nLev[0] + 1));
    for (size_t i = 0; i < data.nLev[0]; ++i)
      SET_STRING_ELT(freqNames, i, Rf_mkChar(data.levelNames[0][i]));
    SET_STRING_ELT(freqNames, data.nLev[0], Rf_mkChar("NA"));
    rc_setNames(freq, freqNames);
    
    closure = PROTECT(Rf_lang2(function, freq));
    
    freq_int = INTEGER(freq);
    numFrequencies = data.nLev[0] + 1;
  }
  
  DivRiskFunction::~DivRiskFunction() {
    UNPROTECT(3);
  }
  
  extern void printObs(const Data& data, const unsigned char* x_i);
  // extern bool printDebugCase;

  double DivRiskFunction::operator()(const Data& data, const State& state, double* risk)
  {
    const unsigned char* xt = state.xt;
    double minRisk = HUGE_VAL;
   
    if (!naRiskWithin) {
      for (size_t row = 0; row < data.nRow; ++row) {
        double risk_i = state.getDivFromTable(data, xt, *this);
        if (risk_i < minRisk) minRisk = risk_i;
        
        xt += data.nCol;
        
        if (risk != NULL) risk[row] = risk_i;
      }
    } else {
      State temp(data);
      temp.copyFrom(data, state);
      unsigned char* x_i = new unsigned char[data.nCol];
      double* risks = new double[data.nCol];
      
      for (size_t row = 0; row < data.nRow; ++row) {
        bool anyNA = false, allNA = true;
        for (size_t col = 1; col < data.nCol; ++col) {
          if (xt[col] == data.nLev[col]) {
            anyNA = true;
          } else {
            allNA = false;
          }
        }
        
        if (!anyNA) {
          double risk_i = temp.getDivFromTable(data, xt, *this);
          if (risk_i < minRisk) minRisk = risk_i;
          
          xt += data.nCol;
          
          if (risk != NULL) risk[row] = risk_i;
          
          continue;
        }
        
        if (allNA) {
          xt += data.nCol;
          
          if (risk != NULL) risk[row] = 1.0;
          
          continue;
        }
        
        // printDebugCase = row == 0;
        // go through possible completions of the obs and calc risks for each
        std::memcpy(x_i, xt, data.nCol * sizeof(unsigned char));
        for (size_t i = 0; i < data.nCol; ++i) risks[i] = HUGE_VAL;
        temp.calculateRiskForCompletion(data, *this, x_i, 1, risks);
        
        /* if (printDebugCase) {
          Rprintf("at end for obs "); printObs(data, x_i);
          Rprintf("\n  risks: %.2f", risks[0]);
          for (size_t i = 1; i < data.nCol; ++i) Rprintf(" %.2f", risks[i]);
          Rprintf("\n");
        } */
        
        double risk_i = risks[0];
        for (size_t i = 1; i < data.nCol; ++i) if (risks[i] < HUGE_VAL) risk_i = risks[i];
        
        if (risk_i < minRisk) minRisk = risk_i;
          
        xt += data.nCol;
          
        if (risk != NULL) risk[row] = risk_i;
      }
      
      delete [] risks;
      delete [] x_i;
    }
    
    return minRisk;
  }
  
  double DivRiskFunction::operator()(const size_t* naFreq) {
    for (size_t i = 0;  i < numFrequencies; ++i) freq_int[i] = static_cast<int>(naFreq[i]);
    
    SEXP result = Rf_eval(closure, environment);
    
    if (Rf_isInteger(result)) return static_cast<double>(INTEGER(result)[0]);
    return REAL(result)[0];
  }
}

