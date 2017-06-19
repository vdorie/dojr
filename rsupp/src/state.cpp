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
  extern void printObs(const Data& data, const unsigned char* x);
  
  State::State(const Data& data) :
    xt(new unsigned char[data.nRow * data.nCol]),
    freqTable(new size_t[data.tableSize]),
    minRisk(HUGE_VAL),
    objective(-HUGE_VAL),
    nCol(data.nCol)
  {
    std::memcpy(xt, data.xt, data.nRow * data.nCol * sizeof(unsigned char));
    for (size_t i = 0; i < data.tableSize; ++i)
      freqTable[i] = 0;
  }
  
  State::~State() {
    if (xt != NULL) {
      delete [] xt;
      xt = NULL;
    }
    if (freqTable != NULL) {
      delete [] freqTable;
      freqTable = NULL;
    }
  }
  
  void State::copyFrom(const Data& data, const State& other) {
    std::memcpy(xt, other.xt, data.nRow * data.nCol * sizeof(unsigned char));
    std::memcpy(freqTable, other.freqTable, data.tableSize * sizeof(size_t));
    minRisk = other.minRisk;
    objective = other.objective;
  }
  
  void State::calculateFreqTable(const Data& data) {
    const unsigned char* xt = const_cast<const unsigned char*>(this->xt);
    for (size_t row = 0; row < data.nRow; ++row) {
      incrementFreqTable(data, xt);
      xt += data.nCol;
    }
  }
  
  void State::incrementFreqTable(const Data& data, const unsigned char* x_i)
  {
    incrementFreqTable(data, x_i, 0, 0, 1);
  }
  
  void State::decrementFreqTable(const Data& data, const unsigned char* x_i)
  {
    decrementFreqTable(data, x_i, 0, 0, 1);
  }
  
  void State::incrementFreqTable(const Data& data, const unsigned char* x_i,
                                 size_t currCol, size_t offset, size_t stride)
  {
    if (currCol == data.nCol - 1) {
      ++freqTable[offset + x_i[currCol] * stride];
    } else {
      incrementFreqTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * (data.nLev[currCol] + 1));
    }
  }
  
  void State::decrementFreqTable(const Data& data, const unsigned char* x_i,
                                 size_t currCol, size_t offset, size_t stride)
  {
    if (currCol == data.nCol - 1) {
      --freqTable[offset + x_i[currCol] * stride];
    } else {
      decrementFreqTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * (data.nLev[currCol] + 1));
    }
  }
  
  double State::getKFromTable(const Data& data, const unsigned char* x_i) const {
    return getKFromTable(data, x_i, 0, 0, 1);
  }
  
  double State::getKFromTable(const Data& data, const unsigned char* x_i,
                              size_t currCol, size_t offset, size_t stride) const
  {
    double result;
    if (currCol == data.nCol - 1) {
      // everybody gets NAs
      result = freqTable[offset + data.nLev[currCol] * stride];
      if (x_i[currCol] != data.nLev[currCol]) {
        // if obs is not NA
        result += freqTable[offset + x_i[currCol] * stride];
      } else {
        // cycle through all other types
        for (size_t i = 0; i < data.nLev[currCol]; ++i)
          result += freqTable[offset + i * stride];
      }
    } else {
      // NAs
      result = getKFromTable(data, x_i, currCol + 1, offset + data.nLev[currCol] * stride, stride * (data.nLev[currCol] + 1));
      if (x_i[currCol] != data.nLev[currCol]) {
        result += getKFromTable(data, x_i, currCol + 1, offset + x_i[currCol] * stride, stride * (data.nLev[currCol] + 1));
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i) {
          result += getKFromTable(data, x_i, currCol + 1, offset + i * stride, stride * (data.nLev[currCol] + 1));
        }
      }
    }
    return result;
  }
    
  double State::getDivFromTable(const Data& data, const unsigned char* x_i, DivRiskFunction& calculateRisk) const {
    size_t* counts = new size_t[data.nLev[0] + 1];
    for (size_t i = 0; i < (data.nLev[0] + 1); ++i) counts[i] = 0;
    
    getCountsFromTable(data, x_i, calculateRisk, 1, 0, data.nLev[0] + 1, counts);
    
    double result = calculateRisk(counts);
    delete [] counts;
    return result;
  }
}
namespace {
  inline void addColumn(const size_t* freqTable, size_t length, size_t* counts) {
    for (size_t i = 0; i < length; ++i)
      counts[i] += freqTable[i];
  }
}
namespace rsupp {
  void State::getCountsFromTable(const Data& data, const unsigned char* x_i, DivRiskFunction& calculateRisk,
                                 size_t currCol, size_t offset, size_t stride, size_t* counts) const
  {
    if (currCol == data.nCol - 1) {
      // everybody gets NAs
      addColumn(freqTable + offset + data.nLev[currCol] * stride, data.nLev[0] + 1, counts);
      if (x_i[currCol] != data.nLev[currCol]) {
        // if obs is not NA
        addColumn(freqTable + offset + x_i[currCol] * stride, data.nLev[0] + 1, counts);
      } else {
        // cycle through all other types
        for (size_t i = 0; i < data.nLev[currCol]; ++i)
          addColumn(freqTable + offset + i * stride, data.nLev[0] + 1, counts);
      }
    } else {
      getCountsFromTable(data, x_i, calculateRisk, currCol + 1, offset + data.nLev[currCol] * stride, stride * (data.nLev[currCol] + 1), counts);
      if (x_i[currCol] != data.nLev[currCol]) {
        getCountsFromTable(data, x_i, calculateRisk, currCol + 1, offset + x_i[currCol] * stride, stride * (data.nLev[currCol] + 1), counts);
      } else {
        for (size_t i = 0; i < data.nLev[currCol]; ++i)
          getCountsFromTable(data, x_i, calculateRisk, currCol + 1, offset + i * stride, stride * (data.nLev[currCol] + 1), counts);
      }
    }
  }
}
namespace {
  using rsupp::Data;
  
  inline bool anyNonzeroCounts(const Data& data, const size_t* counts) {
    for (size_t i = 0; i < (data.nLev[0] + 1); ++i) 
      if (counts[i] > 0) return true;
    return false;
  }
  
  struct CountAccumulator {
    size_t* counts;
    size_t numNonNA;
    
    CountAccumulator(const Data& data) : counts(new size_t[data.nLev[0] + 1]), numNonNA(0) {
      for (size_t i = 0; i < (data.nLev[0] + 1); ++i) counts[i] = 0;
    }
    ~CountAccumulator() {
      if (counts != NULL) {
        delete [] counts;
        counts = NULL;
      }
    }
    
    void addCounts(const Data& data, const size_t* counts) {
      for (size_t i = 0; i < data.nLev[0] + 1; ++i)
        this->counts[i] += counts[i];
    }
    void addCounts(const Data& data, const size_t* counts, size_t numNonNA) {
      addCounts(data, counts);
      if (numNonNA > this->numNonNA && anyNonzeroCounts(data, counts))
        this->numNonNA = numNonNA;
    }
    void reset(const Data& data) {
      for (size_t i = 0; i < data.nLev[0] + 1; ++i)
        counts[i] = 0;
      numNonNA = 0;
    }
  };
  
  using rsupp::State;
  void getCompleteCaseCountsFromTable(const Data& data, const State& state, const unsigned char* x_i,
                                      size_t currCol, size_t offset, size_t stride, CountAccumulator& accumulator,
                                      size_t numNonNA) 
  {
    if (currCol == data.nCol - 1) {
      accumulator.addCounts(data, state.freqTable + offset + x_i[currCol] * stride,
                                           numNonNA + 1);
      accumulator.addCounts(data, state.freqTable + offset + data.nLev[currCol] * stride,
                                           numNonNA);
    } else {
      getCompleteCaseCountsFromTable(data, state, x_i, currCol + 1, offset + x_i[currCol] * stride,
                                     stride * (data.nLev[currCol] + 1), accumulator,
                                     numNonNA + 1);
      getCompleteCaseCountsFromTable(data, state, x_i, currCol + 1, offset + data.nLev[currCol] * stride,
                                     stride * (data.nLev[currCol] + 1), accumulator,
                                     numNonNA);
    }
  }
}

namespace rsupp {
  
  // bool printDebugCase = false;
  
  void State::calculateRiskForCompletion(const Data& data, DivRiskFunction& calculateRisk,
                                         unsigned char* x_i, size_t currCol, double* risks)
  {
    if (currCol == data.nCol - 1) {
      CountAccumulator accumulator(data);
      if (x_i[currCol] != data.nLev[currCol]) {
        // if (printDebugCase) { Rprintf("  getting complete case count for "); printObs(data, x_i); Rprintf("\n"); }
        getCompleteCaseCountsFromTable(data, *this, x_i, 1, 0, data.nLev[0] + 1, accumulator, 0);
        
        double risk_i = calculateRisk(accumulator.counts);
        // if (printDebugCase) { Rprintf("  risk %.2f, numNonNA %lu\n", risk_i, accumulator.numNonNA); }
        if (risk_i < risks[accumulator.numNonNA]) risks[accumulator.numNonNA] = risk_i;
      } else {
        for (unsigned char i = 0; i < data.nLev[currCol]; ++i) {
          x_i[currCol] = i;
          
          // if (printDebugCase) { Rprintf("  getting complete case count for "); printObs(data, x_i); Rprintf("\n"); }
          getCompleteCaseCountsFromTable(data, *this, x_i, 1, 0, data.nLev[0] + 1, accumulator, 0);
          
          double risk_i = calculateRisk(accumulator.counts);
          // if (printDebugCase) { Rprintf("  risk %.2f, numNonNA %lu\n", risk_i, accumulator.numNonNA); }
          if (risk_i < risks[accumulator.numNonNA]) risks[accumulator.numNonNA] = risk_i;
          
          if (i < data.nLev[currCol] - 1) accumulator.reset(data);
        }
        x_i[currCol] = data.nLev[currCol];
      }
    } else {
      if (x_i[currCol] != data.nLev[currCol]) {
        calculateRiskForCompletion(data, calculateRisk, x_i, currCol + 1, risks);
      } else {
        for (unsigned char i = 0; i < data.nLev[currCol]; ++i) {
          x_i[currCol] = i;
          calculateRiskForCompletion(data, calculateRisk, x_i, currCol + 1, risks);
        }
        x_i[currCol] = data.nLev[currCol];
      }
    }
  }

  void State::print(const Data& data, const MCMCParam& param) {
    double naTerm = 0.0;
    
    for (size_t row = 0; row < data.nRow; ++row) {
      for (size_t col = 0; col < data.nCol; ++col) if (xt[col + row * data.nCol] == data.nLev[col]) naTerm += param.theta[col]; 
    }
    naTerm /= static_cast<double>(data.nRow);
   
    Rprintf("(o = %.2f, k = %0.2f, n = %.2f)", objective, minRisk, naTerm);
  }
}
