#ifndef _TV_SPIKE_H_
#define _TV_SPIKE_H_

#include <stdlib.h>
#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

void tv_init(void);

void tv_load_elf(const char *filename);

void tv_set_verbose(int enable);

void tv_reset(void);

void tv_step(void);

int  tv_is_done(void);

int  tv_check_priv(uint8_t prv);

int  tv_check_pc(uint64_t val);

int  tv_check_gpr(size_t regno, uint64_t val);

int  tv_check_csr(size_t regno, uint64_t val);

#ifdef __cplusplus
}
#endif

#endif /* _TV_SPIKE_H_ */
