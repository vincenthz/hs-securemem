#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define SCRUBVAR 0xa5a5a5a5a5a5a5a5ULL

void finalizer_scrub8(void *ptr)
{
	uint64_t *sz_ptr = (uint64_t *) ptr;
	sz_ptr[0] = SCRUBVAR;
}

void finalizer_scrub16(void *ptr)
{
	uint64_t *sz_ptr = (uint64_t *) ptr;
	sz_ptr[0] = SCRUBVAR;
	sz_ptr[1] = SCRUBVAR;
}

void finalizer_scrub24(void *ptr)
{
	uint64_t *sz_ptr = (uint64_t *) ptr;
	sz_ptr[0] = SCRUBVAR;
	sz_ptr[1] = SCRUBVAR;
	sz_ptr[2] = SCRUBVAR;
}

void finalizer_scrub32(void *ptr)
{
	uint64_t *sz_ptr = (uint64_t *) ptr;
	sz_ptr[0] = SCRUBVAR;
	sz_ptr[1] = SCRUBVAR;
	sz_ptr[2] = SCRUBVAR;
	sz_ptr[3] = SCRUBVAR;
}

void finalizer_scrub64(void *ptr)
{
	uint64_t *sz_ptr = (uint64_t *) ptr;
	sz_ptr[0] = SCRUBVAR;
	sz_ptr[1] = SCRUBVAR;
	sz_ptr[2] = SCRUBVAR;
	sz_ptr[3] = SCRUBVAR;
	sz_ptr[4] = SCRUBVAR;
	sz_ptr[5] = SCRUBVAR;
	sz_ptr[6] = SCRUBVAR;
	sz_ptr[7] = SCRUBVAR;
}

void finalizer_scrub128(void *ptr)
{
	uint64_t *sz_ptr = (uint64_t *) ptr;
	int i;

	for (i = 0; i < 16; i++)
		sz_ptr[i] = SCRUBVAR;
}

void finalizer_scrub256(void *ptr)
{
	uint64_t *sz_ptr = (uint64_t *) ptr;
	int i;

	for (i = 0; i < 32; i++)
		sz_ptr[i] = SCRUBVAR;
}

void finalizer_scrubvar(uint32_t sz, void *ptr)
{
	memset(ptr, 0xa5, sz);
}

int constant_memeq16(uint8_t *p1, uint8_t *p2)
{
}

int compare_eq(uint32_t size, uint8_t *p1, uint8_t *p2)
{
	uint32_t i;
	int acc = 1;

	for (i = 0; i < size / 8; i++)
		acc &= (p1[i] == p2[i]);
	return acc;
}

#define COMPARE_LOOP(n) \
	uint64_t *sp1, *sp2; \
	int i, acc; \
	sp1 = (uint64_t *) p1; sp2 = (uint64_t *) p2; \
	acc = 1; \
	for (i = 0; i < n; i++) \
		acc &= (sp1[i] == sp2[i]); \
	return acc; \

int compare_eq8(uint8_t *p1, uint8_t *p2)
{
	uint64_t *sp1, *sp2;
	sp1 = (uint64_t *) p1; sp2 = (uint64_t *) p2;
	return (p1[0] == p2[0]);
}

int compare_eq16(uint8_t *p1, uint8_t *p2)
{
	uint64_t *sp1, *sp2;
	sp1 = (uint64_t *) p1; sp2 = (uint64_t *) p2;
	return ((p1[0] == p2[0]) & (p1[1] == p2[1])) == 1;
}

int compare_eq24(uint8_t *p1, uint8_t *p2)
{
	COMPARE_LOOP(3);
}

int compare_eq32(uint8_t *p1, uint8_t *p2)
{
	COMPARE_LOOP(4);
}

int compare_eq64(uint8_t *p1, uint8_t *p2)
{
	COMPARE_LOOP(8);
}

int compare_eq128(uint8_t *p1, uint8_t *p2)
{
	COMPARE_LOOP(16);
}

int compare_eq256(uint8_t *p1, uint8_t *p2)
{
	COMPARE_LOOP(32);
}
