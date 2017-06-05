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
    minRisk(static_cast<double>(data.nRow)),
    objective(-HUGE_VAL)
  {
    std::memcpy(xt, data.xt, data.nRow * data.nCol * sizeof(unsigned char));
    for (size_t i = 0; i < data.tableSize; ++i) {
      naCount[i] = 0;
      ccCount[i] = 0;
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
  }
  
  void State::copyFrom(const Data& data, const State& other) {
    std::memcpy(xt, other.xt, data.nRow * data.nCol * sizeof(unsigned char));
    std::memcpy(naCount, other.naCount, data.tableSize * sizeof(size_t));
    std::memcpy(ccCount, other.ccCount, data.tableSize * sizeof(size_t));
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
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          ++naCount[offset + i * stride];
        }
      }
    } else {
      if (x_i[currCol] != NA_LEVEL)
        incrementFreqTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * data.nLev[currCol], anyNA);
      else {
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
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          --naCount[offset + i * stride];
        }
      }
    } else {
      if (x_i[currCol] != NA_LEVEL)
        decrementFreqTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * data.nLev[currCol], anyNA);
      else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i)
          decrementFreqTable(data, x_i, currCol + 1, offset + i * stride, stride * data.nLev[currCol], true);
      }
    }
  }
  
  double State::getKFromTable(const Data& data, const unsigned char* x_i,
                              size_t currCol, size_t offset, size_t stride) const
  {
    if (currCol == data.nCol - 1) {
      if (x_i[currCol] != NA_LEVEL) {
        return ccCount[offset + x_i[currCol] * stride] > 0 ? naCount[offset + x_i[currCol] * stride] : data.nRow;
      } else {
        double minK = static_cast<double>(data.nRow);
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          double k_i = static_cast<double>(ccCount[offset + i * stride] > 0 ? naCount[offset + i * stride] : data.nRow);
          if (k_i < minK) minK = k_i;
        }
        return minK;
      }
    } else {
      if (x_i[currCol] != NA_LEVEL) return getKFromTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * data.nLev[currCol]);
      else {
        double minK = static_cast<double>(data.nRow);
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          double k_i = getKFromTable(data, x_i, currCol + 1, offset + i * stride, stride * data.nLev[currCol]);
          if (k_i < minK) minK = k_i;
        }
        return minK;
      }
    }
  }
  
  extern void printObs(const Data& data, const unsigned char*x);
  
  double State::getDivFromTable(const Data& data, const unsigned char* x_i,
                                size_t currCol, size_t offset, size_t stride, 
                                DivRiskFunction& calculateRisk) const
  {
    if (currCol == data.nCol - 1) {
      if (x_i[currCol] != NA_LEVEL) {
        return calculateRisk(naCount + offset + x_i[currCol] * stride);
      } else {
        double minRisk = static_cast<double>(data.nRow);
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          double risk_i = calculateRisk(naCount + offset + i * stride);
          if (risk_i < minRisk) minRisk = risk_i;
        }
        return minRisk;
      }
    } else {
      if (x_i[currCol] != NA_LEVEL) return getDivFromTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * data.nLev[currCol], calculateRisk);
      else {
        double minRisk = static_cast<double>(data.nRow);
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          double risk_i = getDivFromTable(data, x_i, currCol + 1, offset + i * stride, stride * data.nLev[currCol], calculateRisk);
          if (risk_i < minRisk) minRisk = risk_i;
        }
        return minRisk;
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
