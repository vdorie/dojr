#ifndef RSUPP_STATE_HPP
#define RSUPP_STATE_HPP

#include <cstddef> // size_t

namespace rsupp {
  struct Data;
  struct DivRiskFunction;
  struct MCMCParam;
  
  struct State {
    unsigned char* xt;
    // frequency counts in total cross-tab
    std::size_t* naCount; // na
    std::size_t* ccCount; // complete cases
    std::size_t** mCounts; // marginal counts
    
    double minRisk;
    double objective;
    
    std::size_t nCol;
    
    State(const Data& data);
    ~State();
    void copyFrom(const Data& data, const State& other);
    
    // uses internal xt and fills in table with counts for each key
    void calculateFreqTable(const Data& data);
    
    
    // for obs pointed to at x_i, recursively go through columns and increment
    // naCount/ccCount; typically called as:
    //   state.incrementFreqTable(data, x_i, 0, 0, 1);
    void incrementFreqTable(const Data& data, const unsigned char* x_i,
                            std::size_t currCol, std::size_t offset, std::size_t stride,
                            bool anyNA);
    void decrementFreqTable(const Data& data, const unsigned char* x_i,
                            std::size_t currCol, std::size_t offset, std::size_t stride,
                            bool anyNA);
    
    // for obs pointed to at x_i, recursively dig through table and find matching counts
    //   if there the observation is a complete case, report all that match in anyway
    //   if the observation has NAs, find the minimum of complete cases with which it matches
    //   if no complete cases are found, report the maximum of all with which it could be entagled
    double getKFromTable(const Data& data, const unsigned char* x_i) const;
    double getDivFromTable(const Data& data, const unsigned char* x_i, DivRiskFunction& calculateRisk) const;
    
    // recursive function
    void getKFromTable(const Data& data, const unsigned char* x_i,
                         std::size_t currCol, std::size_t offset, std::size_t stride,
                         bool& hasCompleteCase, bool hasMarignalCase, double& ccMin, double& mMin, double& naMin) const;
    void getDivFromTable(const Data& data, const unsigned char* x_i, DivRiskFunction& calculateRisk,
                         std::size_t currCol, std::size_t offset, std::size_t stride,
                         bool& hasCompleteCase, bool hasMarginalCase, double& ccMin, double& mMin, double& naMin) const;
    
    void print(const Data& data, const MCMCParam& param);
  };
}

#endif

