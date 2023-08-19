#include <stdlib.h>
#include "HsFFI.h"
#include "Lib_stub.h"

typedef HsStablePtr Generator;

Generator generator_lake_new(void)
{
  return newGenerator();
}

unsigned short generator_lake_get_size(Generator generator)
{
  return getSize(generator);
}

unsigned long generator_lake_get_count(Generator generator)
{
  return getCount(generator);
}

char *generator_lake_get_value(Generator generator)
{
  return getValue(generator);
}

Generator generator_lake_next_value(Generator generator)
{
  return nextValue(generator);
}

Generator generator_lake_next_size(Generator generator)
{
  return nextSize(generator);
}

void generator_lake_free(Generator generator)
{
  return freeGenerator(generator);
}

void generator_lake_init(void)
{
  hs_init(NULL, NULL);
}

void generator_lake_exit(void)
{
  hs_exit();
}
