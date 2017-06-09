#include "state.hpp"

#include <cstring> // std::memcpy
#include <cmath>   // HUGE_VAL

#include "rc.h"

#define R_NO_REMAP
#include <R_ext/Print.h> // Rprintf
#undef R_NO_REMAP

#include "data.hpp"
#include "definitions.hpp"
#include "param.hpp"
#include "riskFunction.hpp"

using std::size_t;

namespace rsupp {
  State::State(const Data& data) :
    xt(new unsigned char[data.nRow * data.nCol]),
    naCount(new size_t[data.tableSize]),
    ccCount(new size_t[data.tableSize]),
    mCounts(new size_t*[data.nCol]),
    minRisk(HUGE_VAL),
    objective(-HUGE_VAL),
    nCol(data.nCol)
  {
    std::memcpy(xt, data.xt, data.nRow * data.nCol * sizeof(unsigned char));
    for (size_t i = 0; i < data.tableSize; ++i) {
      naCount[i] = 0;
      ccCount[i] = 0;
    }
    
    for (size_t col = 0; col < data.nCol; ++col) {
      mCounts[col] = new size_t[data.nLev[col]];
      for (size_t i = 0; i < data.nLev[col]; ++i) mCounts[col][i] = 0;
    }
  }
  
  State::~State() {
    if (xt != NULL) {
      delete [] xt;
      xt = NULL;
    }
    if (naCount != NULL) {
      delete [] naCount;
      naCount = NULL;
    }
    if (ccCount != NULL) {
      delete [] ccCount;
      ccCount = NULL;
    }
    if (mCounts != NULL) {
      for (size_t col = 0; col < nCol; ++col)
        if (mCounts[col] != NULL) delete [] mCounts[col];
      delete [] mCounts;
      mCounts = NULL;
    }
  }
  
  void State::copyFrom(const Data& data, const State& other) {
    std::memcpy(xt, other.xt, data.nRow * data.nCol * sizeof(unsigned char));
    std::memcpy(naCount, other.naCount, data.tableSize * sizeof(size_t));
    std::memcpy(ccCount, other.ccCount, data.tableSize * sizeof(size_t));
    for (size_t col = 0; col < data.nCol; ++col)
      std::memcpy(mCounts[col], other.mCounts[col], data.nLev[col] * sizeof(size_t));
    minRisk = other.minRisk;
    objective = other.objective;
  }
  
  void State::calculateFreqTable(const Data& data) {
    const unsigned char* xt = const_cast<const unsigned char*>(this->xt);
    for (size_t row = 0; row < data.nRow; ++row) {
      incrementFreqTable(data, xt, 0, 0, 1, false);
      xt += data.nCol;
    }
  }
  
  void State::incrementFreqTable(const Data& data, const unsigned char* x_i,
                                 size_t currCol, size_t offset, size_t stride, bool anyNA)
  {
    if (currCol == data.nCol - 1) {
      if (x_i[currCol] != NA_LEVEL) {
        ++naCount[offset + x_i[currCol] * stride];
        if (!anyNA) ++ccCount[offset + x_i[currCol] * stride];
        mCounts[currCol][x_i[currCol]] += 1;
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          ++naCount[offset + i * stride];
        }
      }
    } else {
      if (x_i[currCol] != NA_LEVEL) {
        incrementFreqTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * data.nLev[currCol], anyNA);
        mCounts[currCol][x_i[currCol]] += 1;
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i)
          incrementFreqTable(data, x_i, currCol + 1, offset + i * stride, stride * data.nLev[currCol], true);
      }
    }
  }
  
  void State::decrementFreqTable(const Data& data, const unsigned char* x_i,
                                 size_t currCol, size_t offset, size_t stride, bool anyNA)
  {
    if (currCol == data.nCol - 1) {
      if (x_i[currCol] != NA_LEVEL) {
        --naCount[offset + x_i[currCol] * stride];
        if (!anyNA) --ccCount[offset + x_i[currCol] * stride];
        mCounts[currCol][x_i[currCol]] -= 1;
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          --naCount[offset + i * stride];
        }
      }
    } else {
      if (x_i[currCol] != NA_LEVEL) {
        decrementFreqTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * data.nLev[currCol], anyNA);
        mCounts[currCol][x_i[currCol]] -= 1;
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i)
          decrementFreqTable(data, x_i, currCol + 1, offset + i * stride, stride * data.nLev[currCol], true);
      }
    }
  }
  
  extern void printObs(const Data& data, const unsigned char* x);
  
  double State::getKFromTable(const Data& data, const unsigned char* x_i) const {
    bool hasCompleteCase = false;
    double naMin = HUGE_VAL, ccMin = HUGE_VAL;
    
    /* Rprintf("for obs ");
    printObs(data, x_i);
    Rprintf("\n"); */
    getKFromTable(data, x_i, 0, 0, 1, hasCompleteCase, true, ccMin, naMin);
    // Rprintf("  cc %s, ccMin %.2f, naMax %.2f\n", hasCompleteCase ? "true" : "false", ccMin, naMin);
    
    return hasCompleteCase ? ccMin : naMin;
  }
  
  void State::getKFromTable(const Data& data, const unsigned char* x_i,
                            size_t currCol, size_t offset, size_t stride,
                            bool& hasCompleteCase, bool hasMarginalCase, double& ccMin, double& naMin) const
  {
    if (currCol == data.nCol - 1) {
      if (x_i[currCol] != NA_LEVEL) {
        hasCompleteCase |= ccCount[offset + x_i[currCol] * stride] > 0;
        
        double risk_i = static_cast<double>(naCount[offset + x_i[currCol] * stride]);
        
        // Rprintf("    %s %.2f\n", data.levelNames[data.nCol - 1][x_i[currCol]], risk_i);
        
        if (hasMarginalCase)
          naMin = risk_i < naMin ? risk_i : naMin;
        if (ccCount[offset + x_i[currCol] * stride] > 0)
          ccMin = risk_i < ccMin ? risk_i : ccMin;
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          hasCompleteCase |= ccCount[offset + i * stride] > 0;
          
          double risk_i = static_cast<double>(naCount[offset + i * stride]);
          // Rprintf("    %s %.2f\n", data.levelNames[data.nCol - 1][i], risk_i);
          if (hasMarginalCase && mCounts[currCol][i] > 0)
            naMin = risk_i < naMin ? risk_i : naMin;
          if (ccCount[offset + i * stride] > 0)
            ccMin = risk_i < ccMin ? risk_i : ccMin;
        }
      }
    } else {
      if (x_i[currCol] != NA_LEVEL) {
        getKFromTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * data.nLev[currCol],
                      hasCompleteCase, true, ccMin, naMin);
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          getKFromTable(data, x_i, currCol + 1, offset + i * stride, stride * data.nLev[currCol],
                        hasCompleteCase, hasMarginalCase && mCounts[currCol][i] > 0, ccMin, naMin);
        }
      }
    }
  }
  
  double State::getDivFromTable(const Data& data, const unsigned char* x_i, DivRiskFunction& calculateRisk) const {
    bool hasCompleteCase = false;
    double naMin = HUGE_VAL, ccMin = HUGE_VAL;
    
    getDivFromTable(data, x_i, calculateRisk, 1, 0, data.nLev[0], hasCompleteCase, true, ccMin, naMin);
    
    return hasCompleteCase ? ccMin : naMin;
  }
  
  void State::getDivFromTable(const Data& data, const unsigned char* x_i, DivRiskFunction& calculateRisk,
                              size_t currCol, size_t offset, size_t stride, 
                              bool& hasCompleteCase, bool hasMarginalCase, double& ccMin, double& naMin) const
  {
    if (currCol == data.nCol - 1) {
      if (x_i[currCol] != NA_LEVEL) {
        hasCompleteCase |= ccCount[offset + x_i[currCol] * stride] > 0;
        
        double risk_i = calculateRisk(naCount + offset + x_i[currCol] * stride);
        
        if (hasMarginalCase)
          naMin = risk_i < naMin ? risk_i : naMin;
        if (ccCount[offset + x_i[currCol] * stride] > 0)
          ccMin = risk_i < ccMin ? risk_i : ccMin;
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          hasCompleteCase |= ccCount[offset + i * stride] > 0;
          
          double risk_i = calculateRisk(naCount + offset + i * stride);
          
          if (hasMarginalCase && mCounts[currCol][i] > 0)
            naMin = risk_i < naMin ? risk_i : naMin;
          if (ccCount[offset + i * stride] > 0)
            ccMin = risk_i < ccMin ? risk_i : ccMin;
        }
      }
    } else {
      if (x_i[currCol] != NA_LEVEL) {
        getDivFromTable(data, x_i, calculateRisk,
                        currCol + 1, offset + x_i[currCol] * stride, stride * data.nLev[currCol],
                        hasCompleteCase, true, ccMin, naMin);
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          getDivFromTable(data, x_i, calculateRisk,
                          currCol + 1, offset + i * stride, stride * data.nLev[currCol],
                          hasCompleteCase, hasMarginalCase && mCounts[currCol][i] > 0, ccMin, naMin);
        }
      }
    }
  }
  
  void State::print(const Data& data, const MCMCParam& param) {
    double naTerm = 0.0;
    
    for (size_t row = 0; row < data.nRow; ++row) {
      for (size_t col = 0; col < data.nCol; ++col) if (xt[col + row * data.nCol] == NA_LEVEL) naTerm += param.theta[col]; 
    }
    naTerm /= static_cast<double>(data.nRow);
   
    Rprintf("(o = %.2f, k = %0.2f, n = %.2f)", objective, minRisk, naTerm);
  }
}