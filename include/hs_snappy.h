#ifndef _hs_snappy_h
#define _hs_snappy_h

#include <stddef.h>

#ifdef __cplusplus
extern "C" 
{
#endif

size_t _hsnappy_MaxCompressedLength(size_t);

void _hsnappy_RawCompress(const char *input, size_t input_length,
			  char *compressed, size_t *compressed_length);

bool _hsnappy_GetUncompressedLength(const char *compressed,
				    size_t compressed_length,
				    size_t *result);

bool _hsnappy_RawUncompress(const char *compressed, size_t compressed_length,
			    char *uncompressed);

#ifdef __cplusplus
}
#endif

#endif /* _hs_snappy_h */
