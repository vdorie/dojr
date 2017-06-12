#include "data.hpp"

#include <climits> // UCHAR_MAX
#include <cstring> // std::strcmp, memcpy

#include "definitions.hpp"
#include "rc.h"

using std::size_t;

namespace rsupp {
  Data::Data(SEXP x) :
    xt(NULL), nRow(INVALID_EXTENT), nCol(INVALID_EXTENT), nLev(NULL), tableSize(INVALID_EXTENT), colNames(NULL), levelNames(NULL)
  {
    SEXP classExpr = rc_getClass(x);
    if (std::strcmp(CHAR(STRING_ELT(classExpr, 0)), "data.frame") != 0) Rf_error("x argument must be of 'data.frame' class");
    
    nCol = rc_getLength(x);
    if (nCol == 0) Rf_error("x argument cannot be an empty data frame");
    
    nRow = rc_getLength(VECTOR_ELT(x, 0));
    if (nRow == 0) Rf_error("x argument cannot be an empty data frame");
    
    SEXP namesExpr = rc_getNames(x);
    const char** colNames = NULL;
    if (namesExpr != R_NilValue) {
      colNames = new const char*[nCol];
      for (size_t col = 0; col < nCol; ++col)
        colNames[col] = CHAR(STRING_ELT(namesExpr, col));
    }
    
    unsigned char* xt = new unsigned char[nRow * nCol];
    size_t* nLev = new size_t[nCol];
    
    for (size_t col = 0; col < nCol; ++col) {
      SEXP x_j = VECTOR_ELT(x, col);
      if (!Rf_isFactor(x_j)) {
        delete [] xt; delete [] nLev;
        if (colNames != NULL) {
          const char* colName = colNames[col];
          delete [] colNames;
          Rf_error("column '%s' of x not of factor type", colName);
        } else {
          Rf_error("column '" SIZE_T_FMT " of x not of factor type", col + 1);
        }
      }
      
      nLev[col] = rc_getLength(rc_getLevels(x_j));
      
      if (nLev[col] > UCHAR_MAX - 1) {
        delete [] xt; delete [] nLev;
        if (colNames != NULL) {
          const char* colName = colNames[col];
          delete [] colNames;
          Rf_error("column '%s' of x has more levels than maximum (" SIZE_T_FMT " > %hu)", colName, nLev[col], UCHAR_MAX - 1);
        } else {
          Rf_error("column " SIZE_T_FMT " of x has more levels than maximum (" SIZE_T_FMT " > %hu)", col + 1, nLev[col], UCHAR_MAX - 1);
        }
      }
      
      int* x_j_int = INTEGER(x_j);
      
      for (size_t row = 0; row < nRow; ++row)
        xt[col + row * nCol] = x_j_int[row] == R_NaInt ? nLev[col] : x_j_int[row] - 1;
    }
    
    const char*** levelNames = new const char**[nCol];
    
    tableSize = 1; 
    for (size_t col = 0; col < nCol; ++ col) {
      tableSize *= (nLev[col] + 1);
      
      SEXP levelsExpr = rc_getLevels(VECTOR_ELT(x, col));
      if (levelsExpr == R_NilValue) {
        levelNames[col] = NULL;
      } else {
        levelNames[col] = new const char*[nLev[col]];
        for (size_t i = 0; i < nLev[col]; ++i)
          levelNames[col][i] = CHAR(STRING_ELT(levelsExpr, i));
      }
    }
    
    this->xt = xt;
    this->nLev = nLev;
    this->colNames = colNames;
    this->levelNames = levelNames;
  }
  
  Data::Data(const Data& other, const size_t* subsetIndices, size_t subsetLength) :
    xt(NULL), nRow(subsetLength), nCol(other.nCol), nLev(NULL), tableSize(other.tableSize),
    colNames(NULL), levelNames(NULL)
  {
    unsigned char* xt = new unsigned char[nRow * nCol];
    size_t* nLev = new size_t[nCol];
    const char** colNames = NULL;
    const char*** levelNames = NULL;
    
    for (size_t row = 0; row < nRow; ++row) {
      size_t otherRow = subsetIndices[row];
      std::memcpy(xt + row * nCol, other.xt + otherRow * nCol, nCol * sizeof(unsigned char));
    }
    for (size_t col = 0; col < nCol; ++col)
      nLev[col] = other.nLev[col];
    
    if (other.colNames != NULL) {
      colNames = new const char*[nCol];
      std::memcpy(colNames, other.colNames, nCol * sizeof(const char*));
    }
    if (other.levelNames != NULL) {
      levelNames = new const char**[nCol];
      for (size_t col = 0; col < nCol; ++col) {
        if (other.levelNames[col] != NULL) {
          levelNames[col] = new const char*[nLev[col]];
          std::memcpy(levelNames[col], other.levelNames[col], nLev[col] * sizeof(const char*));
        } else {
          levelNames[col] = NULL;
        }
      }
    }
   
    this->xt = xt;
    this->nLev = nLev;
    this->colNames = colNames;
    this->levelNames = levelNames;
  }
  
  Data::~Data() {
    if (xt != NULL) {
      delete [] xt;
      xt = NULL;
    }
    if (nLev != NULL) {
      delete [] nLev;
      nLev = NULL;
    }
    if (colNames != NULL) {
      delete [] colNames;
      colNames = NULL;
    }
    if (levelNames != NULL) {
      for (size_t col = 0; col < nCol; ++col)
        if (levelNames[col] != NULL) delete [] levelNames[col];
      delete [] levelNames;
      levelNames = NULL;
    }
  }
}
