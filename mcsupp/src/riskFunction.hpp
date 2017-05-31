#ifndef MCSUPP_RISK_FUNCTION_HPP
#define MCSUPP_RISK_FUNCTION_HPP

#include <cstddef> // size_t

#define R_NO_REMAP
#include <Rinternals.h>  // SEXP
#undef R_NO_REMAP

namespace mcsupp {
  struct Data;
  struct State;
  
  struct RiskFunction {
    virtual double operator()(const Data& data, const State& state, double* risk) = 0;
    virtual ~RiskFunction() { };
  };
  
  struct KRiskFunction : RiskFunction {
    KRiskFunction() { };
    double operator()(const Data& data, const State& state, double* risk);
    ~KRiskFunction() { };
  };
  
  // wraps an R function
  struct DivRiskFunction : RiskFunction {
    SEXP closure;
    SEXP environment;
    
    int* freq_int;
    std::size_t numFrequencies;
    
    // pulls names from x
    DivRiskFunction(const Data& data, SEXP x, SEXP riskFunction);
    double operator()(const Data& data, const State& state, double* risk);
    ~DivRiskFunction();
    
    // works on individual cell of freq table by putting naFreq when ccFreq > 0 into freq_int
    // and evaluating the closure (in the environment)
    double operator()(const std::size_t* naFreq, const std::size_t* ccFreq);
  };
}

#endif

