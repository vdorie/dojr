#include "random.h"

#include <math.h> // fabs

#define R_NO_REMAP
#include <R_ext/Random.h> // unif_rand
#undef R_NO_REMAP

size_t rng_drawFromDiscreteDistribution(const double* probabilities, size_t length)
{
  if (length == 0) return RNG_DISCRETE_DRAW_FAILURE;
  
  double u = unif_rand();
  
  size_t result = 0;
  double sum = probabilities[0];
  
  while (sum < u && result < length - 1) {
    sum += probabilities[++result];
  }
  
  if (result == length - 1 && sum < u) return RNG_DISCRETE_DRAW_FAILURE;
  return result;
}

void rng_permuteIndexArray(size_t* indices, size_t length)
{
  if (length == 0) return;
    
  size_t temp, swapPos;
  for (size_t i = 0; i < length - 1; ++i) {
    swapPos = (size_t) rng_simulateUnsignedIntegerUniformInRange(i, length);
    
    temp = indices[i];
    indices[i] = indices[swapPos];
    indices[swapPos] = temp;
  }
}

// random in [min, min + 1, ..., max - 1, max)
int64_t rng_simulateIntegerUniformInRange(int64_t min_inclusive, int64_t max_exclusive)
{
  double range = fabs((double) (max_exclusive - min_inclusive));
  int64_t actualMin = (min_inclusive < max_exclusive ? min_inclusive : max_exclusive);
  
  double u = unif_rand();
  
  return actualMin + (int64_t) (u * range);
}

uint64_t rng_simulateUnsignedIntegerUniformInRange(uint64_t min_inclusive, uint64_t max_exclusive)
{
  uint64_t actualMin, actualMax;
  if (min_inclusive < max_exclusive) {
    actualMin = min_inclusive;
    actualMax = max_exclusive;
  } else {
    actualMin = max_exclusive;
    actualMax = min_inclusive;
  }
  
  double range = (double) (actualMax - actualMin);
  
  double u = unif_rand();
  
  return actualMin + (uint64_t) (u * range);
}

