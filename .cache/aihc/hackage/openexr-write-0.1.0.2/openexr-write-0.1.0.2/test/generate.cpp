/* PURPOSE: generate test files using ILM's OpenEXR library (i.e., the standard) */


#include <OpenEXR/ImfChannelList.h>
#include <OpenEXR/ImfOutputFile.h>




static void
generate(char const *const filepath, Imf::Compression const compression)
{
        float const r_pixels[] = {1.0f};
        float const g_pixels[] = {0.0f};
        float const b_pixels[] = {0.0f};
        int const width = 1;
        int const height = 1;

        Imf::Header header(width, height, 1, Imath::V2f(0, 0), 1, Imf::INCREASING_Y, compression);
        header.channels().insert("B", Imf::Channel(Imf::FLOAT));
        header.channels().insert("G", Imf::Channel(Imf::FLOAT));
        header.channels().insert("R", Imf::Channel(Imf::FLOAT));

        Imf::OutputFile file(filepath, header);

        Imf::FrameBuffer fb;
        fb.insert("B", Imf::Slice(Imf::FLOAT, (char *)b_pixels, sizeof *b_pixels, sizeof *b_pixels*width));
        fb.insert("G", Imf::Slice(Imf::FLOAT, (char *)g_pixels, sizeof *g_pixels, sizeof *g_pixels*width));
        fb.insert("R", Imf::Slice(Imf::FLOAT, (char *)r_pixels, sizeof *r_pixels, sizeof *r_pixels*width));

        file.setFrameBuffer(fb);
        file.writePixels(height);
}




int
main(void)
{
        generate("images/red_1x1_no_compression.exr", Imf::NO_COMPRESSION);
        generate("images/red_1x1_zips_compression.exr", Imf::ZIPS_COMPRESSION);
        generate("images/red_1x1_zip_compression.exr", Imf::ZIP_COMPRESSION);
}
