#include <stdio.h>
#include "tv_spike_intf.h"
#include "tv_spike.h"

void tv_init(void)
{
  fprintf(stderr, "%s()\n", __func__);
}

void tv_load_elf(const char *filename)
{
  fprintf(stderr, "%s(%s)\n", __func__, filename);
}

void tv_set_verbose(int enable)
{
  fprintf(stderr, "%s(%d)\n", __func__, enable);
}

void tv_reset(void)
{
  fprintf(stderr, "%s()\n", __func__);
}

void tv_step(void)
{
  fprintf(stderr, "%s()\n", __func__);
}

int tv_is_done(void)
{
  fprintf(stderr, "%s()\n", __func__);
  return 0;
}

int tv_check_priv(uint8_t priv)
{
  fprintf(stderr, "%s(%d)\n", __func__, priv);
  return 0;
}

int tv_check_pc(uint64_t val)
{
  fprintf(stderr, "%s(%0" PRIx64 ")\n", __func__, val);
  return 0;
}

int tv_check_gpr(size_t regno, uint64_t val)
{
  fprintf(stderr, "%s(%ld, %0" PRIx64 ")\n", __func__, regno, val);
  return 0;
}

int tv_check_csr(size_t regno, uint64_t val)
{
  fprintf(stderr, "%s(%ld, %0" PRIx64 ")\n", __func__, regno, val);
  return 0;
}
