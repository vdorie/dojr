#ifndef RSUPP_DATA_HPP
#define RSUPP_DATA_HPP

#include <cstddef> // size_t

#define R_NO_REMAP
#include <Rinternals.h>  // SEXP
#undef R_NO_REMAP

namespace rsupp {
  struct Data {
    const unsigned char* xt;
    std::size_t nRow;
    std::size_t nCol;
    const std::size_t* nLev;
    std::size_t tableSize;
    
    const char* const* colNames;
    const char* const* const* levelNames;
    
    Data(SEXP x);
    Data(const Data& data, const std::size_t* subsetIndices, std::size_t subsetLength);
    ~Data();
  };
}

#endif

