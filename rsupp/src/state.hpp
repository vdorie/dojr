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
    
    double minRisk;
    double objective;
    
    State(const Data& data);
    ~State();
    void copyFrom(const Data& data, const State& other);
    
    // uses internal xt and fills in table with counts for each key
    void calculateFreqTable(const Data& data);
    
    
    // for obs pointed to at x_i, recursively go through columns and increment
    // naCount/ccCount; typically called as:
    //   state.incrementFreqTable(data, x_i, 0, 0, 1, false);
    void incrementFreqTable(const Data& data, const unsigned char* x_i,
                            std::size_t currCol, std::size_t offset, std::size_t stride,
                            bool anyNA);
    void decrementFreqTable(const Data& data, const unsigned char* x_i,
                            std::size_t currCol, std::size_t offset, std::size_t stride,
                            bool anyNA);
    
    // for obs pointed to at x_i, recursively dig through table and find
    // naCount when ccCount > 0; typically called as:
    //   state.getKFromTable(data, x_i, 0, 0, 1)
    double getKFromTable(const Data& data, const unsigned char* x_i,
                         std::size_t currCol, std::size_t offset, std::size_t stride) const;
    double getDivFromTable(const Data& data, const unsigned char* x_i,
                           std::size_t currCol, std::size_t offset, std::size_t stride,
                           DivRiskFunction& calculateRisk) const;
    
    void print(const Data& data, const MCMCParam& param);
  };
}

#endif

