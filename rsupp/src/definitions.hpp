#ifndef RSUPP_DEFINITIONS_HPP
#define RSUPP_DEFINITIONS_HPP

#define NA_LEVEL (UCHAR_MAX - 1)
#define INVALID_EXTENT static_cast<size_t>(-1)

#ifdef _WIN32
#  define SIZE_T_FMT "%Iu"
#else
#  define SIZE_T_FMT "%zu"
#endif

#endif

