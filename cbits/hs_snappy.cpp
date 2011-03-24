#include "hs_snappy.h"
#include "snappy.h"
#include "snappy-sinksource.h"

using namespace snappy;

size_t _hsnappy_MaxCompressedLength(size_t n)
{
  return MaxCompressedLength(n);
}

void _hsnappy_RawCompress(const char *input, size_t input_length,
			  char *compressed, size_t *compressed_length)
{
  RawCompress(input, input_length, compressed, compressed_length);
}

int _hsnappy_GetUncompressedLength(const char *compressed,
				   size_t compressed_length,
				   size_t *result)
{
  return GetUncompressedLength(compressed, compressed_length, result);
}

int _hsnappy_RawUncompress(const char *compressed, size_t compressed_length,
			   char *uncompressed)
{
  return RawUncompress(compressed, compressed_length, uncompressed);
}

class BSSource : public Source 
{
public:
  BSSource(BS *chunks, size_t nchunks, size_t left)
    : chunks_(chunks), nchunks_(nchunks), cur_(chunks), left_(left) { }
  
  size_t Available() const { return left_; }
  
  const char *Peek(size_t *len) {
    *len = cur_->len;
    return cur_->ptr;
  }

  void Skip(size_t n) {
    left_ -= n;
    while (n >= cur_->len) {
      n -= cur_->len;
      cur_++;
    }
    if (n > 0) {
      cur_->len -= n;
      cur_->ptr += n;
    }
  }

private:
  BS *chunks_;
  const int nchunks_;
  BS *cur_;
  size_t left_;
};
  
void _hsnappy_CompressChunks(BS *chunks, size_t nchunks, size_t length,
			     char *compressed, size_t *compressed_length)
{
  BSSource reader(chunks, nchunks, length);
  UncheckedByteArraySink writer(compressed);

  Compress(&reader, &writer);

  *compressed_length = writer.CurrentDestination() - compressed;
}
