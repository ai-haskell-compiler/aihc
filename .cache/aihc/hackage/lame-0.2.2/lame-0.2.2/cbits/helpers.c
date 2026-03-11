#include "helpers.h"

int id3tag_set_textinfo_utf16_wrapped
  ( lame_global_flags *gfp
  , const char *id
  , const uint16_t *raw_text
  , int len)
{
  uint16_t *text = alloca(sizeof(uint16_t) * (len + 2));
  int i;
  *(text + 0) = 0xfeff; /* BOM */
  for (i = 0; i < len; i++)
    {
      *(text + i + 1) = *(raw_text + i);
    }
  *(text + len + 1) = 0;
  return id3tag_set_textinfo_utf16(gfp, id, text);
}

int id3tag_set_comment_utf16_wrapped
  ( lame_global_flags *gfp
  , const uint16_t *raw_text
  , int len )
{
  uint16_t *text = alloca(sizeof(uint16_t) * (len + 2));
  int i;
  *(text + 0) = 0xfeff; /* BOM */
  for (i = 0; i < len; i++)
    {
      *(text + i + 1) = *(raw_text + i);
    }
  *(text + len + 1) = 0;
  return id3tag_set_comment_utf16(gfp, 0, 0, text);
}

static unsigned round_to_bytes(unsigned bits)
{
  return (bits + (bits % 8)) / 8;
}

int lame_encoding_helper
  ( lame_global_flags *gfp
  , uint64_t data_offset
  , uint64_t data_size
  , uint16_t sample_format
  , uint16_t bits_per_sample
  , const char * ifile_name
  , const char * ofile_name )
{
  unsigned channels = lame_get_num_channels(gfp);
  unsigned msw = round_to_bytes(bits_per_sample);
  unsigned block_align = channels * msw;
  uint64_t samples_to_process = data_size / block_align;
  uint64_t read_size = 0xffff; /* we need a bit more here because of pictures in metadata */
  void *buffer = malloc(read_size * block_align);
  size_t mp3buffer_size = 1.3 * read_size + 7200;
  void *mp3buffer = malloc(mp3buffer_size);
  int imp3, status;
  FILE *ifile = fopen(ifile_name, "r");
  FILE *ofile = fopen(ofile_name, "w");
  uint64_t samples;

  if ((data_size % block_align) ||
      bits_per_sample <= 8      ||
      (sample_format == 0 && bits_per_sample > 16))
    {
      free(buffer);
      fclose(ifile);
      fclose(ofile);
      return -1;
    }

  /* move position indicator to beginning of audio data */
  fseek(ifile, data_offset, SEEK_SET);

  while (samples_to_process)
    {
      /* The reading happens by blocks. Every block has read_size samples in
       * it (multi-channel samples, that is). Since we will be using the
       * value returned by fread as indicator of whether something went
       * wrong or not, we need to know beforehand how many samples we need
       * to read. */
      samples = read_size <= samples_to_process ? read_size : samples_to_process;

      /* Read the samples into the “raw” buffer, fail by returning false if
       * number of read samples differs from the expected. */
      if (fread(buffer, block_align, samples, ifile) != samples)
        {
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

      switch (sample_format)
        {
        case LAME_HASKELL_INT: /* samples are integers */
          imp3 = lame_encode_buffer_interleaved
            (gfp, buffer, samples, mp3buffer, mp3buffer_size);
          break;
        case LAME_HASKELL_FLOAT: /* samples are 32 bit floats */
          imp3 = lame_encode_buffer_interleaved_ieee_float
            (gfp, buffer, samples, mp3buffer, mp3buffer_size);
          break;
        case LAME_HASKELL_DOUBLE: /* samples are 64 bit floats (doubles) */
          imp3 = lame_encode_buffer_interleaved_ieee_double
            (gfp, buffer, samples, mp3buffer, mp3buffer_size);
          break;
        default: /* got something else? you're screwed */
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

      if (imp3 < 0)
        {
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

      status = (int) fwrite(mp3buffer, 1, imp3, ofile);

      if (status != imp3)
        {
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

      samples_to_process -= samples;
    }

  if (lame_get_nogap_total(gfp) > 0)
    imp3 = lame_encode_flush_nogap(gfp, mp3buffer, mp3buffer_size);
  else
    imp3 = lame_encode_flush(gfp, mp3buffer, mp3buffer_size);

      if (imp3 < 0)
        {
          free(buffer);
          fclose(ifile);
          fclose(ofile);
          return -1;
        }

  status = (int) fwrite(mp3buffer, 1, imp3, ofile);

  free(buffer);
  fclose(ifile);
  fclose(ofile);

  return (status == imp3) ? 0 : -1;
}
