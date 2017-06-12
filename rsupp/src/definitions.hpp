#ifndef RSUPP_DEFINITIONS_HPP
#define RSUPP_DEFINITIONS_HPP

#define INVALID_EXTENT static_cast<size_t>(-1)

#ifdef _WIN32
#  define SIZE_T_FMT "%Iu"
#else
#  define SIZE_T_FMT "%zu"
#endif

/*
void* operator new (size_t size, const char* filename, int line);
void* operator new[] (size_t size, const char* filename, int line);

#define new new(__FILE__, __LINE__)
*/

#endif

