#ifndef RSUPP_RISK_FUNCTION_HPP
#define RSUPP_RISK_FUNCTION_HPP

#include <cstddef> // size_t

#define R_NO_REMAP
#include <Rinternals.h>  // SEXP
#undef R_NO_REMAP

namespace rsupp {
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
    bool naRiskWithin;
    
    // pulls names from x
    DivRiskFunction(const Data& data, SEXP riskFunction, bool naRiskWithin);
    double operator()(const Data& data, const State& state, double* risk);
    ~DivRiskFunction();
    
    // works on individual cell of freq table by putting naFreq into freq_int
    // and evaluating the closure (in the environment)
    double operator()(const std::size_t* naFreq);
  };
}

#endif

