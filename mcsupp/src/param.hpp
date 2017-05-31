#ifndef MCSUPP_PARAM_HPP
#define MCSUPP_PARAM_HPP

#include <cstddef> // size_t
#include <stdint.h> // uint8_t; not C++ until C++11

#define R_NO_REMAP
#include <Rinternals.h>  // SEXP
#undef R_NO_REMAP

namespace mcsupp {
  struct Data;
  struct RiskFunction;
  
  typedef enum {
    RTYPE_COUNT,
    RTYPE_DIVERSITY,
    RTYPE_PERCENT,
    RTYPE_INVALID
  } risk_t;
  
  struct Param {
    double threshold;
    
    double alpha; // shape of gamma term on risk
    double beta;  // rate of gamma term on risk
    double gamma; // weight of gamma term on risk, 1 - gamma is weight on NA loss term
    
    double* theta; // weights for each NA component
    double* theta_inv;
    
    // mcmc transition probs
    double rowSwapProb;
    double colSwapProb;
    double naProb;
    
    std::size_t nBurn;
    std::size_t nSamp;
    
    uint8_t verbose;
    bool skipRandomInit;
    
    risk_t riskType;
    bool* suppressValues;
    
    Param(const Data& data, RiskFunction* divRiskFunction, SEXP param);
    ~Param();
  };
}

#endif

