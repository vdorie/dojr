#ifndef RANDOM_H
#define RANDOM_H

#ifdef __cplusplus
#  include <cstddef>
#  define rng_size_t std::size_t
#else
#  include <stddef.h>
#  define rng_size_t size_t
#endif

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif


#ifdef __cplusplus
#  define RNG_DISCRETE_DRAW_FAILURE static_cast<rng_size_t>(-1)
#else
#  define RNG_DISCRETE_DRAW_FAILURE ((rng_size_t) -1)
#endif
rng_size_t rng_drawFromDiscreteDistribution(const double* probabilities, rng_size_t length);

void rng_permuteIndexArray(rng_size_t* indices, rng_size_t length);
  
// random in [min, min + 1, ..., max - 1, max)
int64_t rng_simulateIntegerUniformInRange(int64_t min_inclusive, int64_t max_exclusive);
uint64_t rng_simulateUnsignedIntegerUniformInRange(uint64_t min_inclusive, uint64_t max_exclusive);

#ifdef __cplusplus
}
#endif

#endif

