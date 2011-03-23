#include "hs_snappy.h"
#include "snappy.h"

size_t _hsnappy_MaxCompressedLength(size_t n)
{
  return snappy::MaxCompressedLength(n);
}

void _hsnappy_RawCompress(const char *input, size_t input_length,
			  char *compressed, size_t *compressed_length)
{
  snappy::RawCompress(input, input_length, compressed, compressed_length);
}

bool _hsnappy_GetUncompressedLength(const char *compressed,
				    size_t compressed_length,
				    size_t *result)
{
  return snappy::GetUncompressedLength(compressed, compressed_length, result);
}

bool _hsnappy_RawUncompress(const char *compressed, size_t compressed_length,
			    char *uncompressed)
{
  return snappy::RawUncompress(compressed, compressed_length, uncompressed);
}
