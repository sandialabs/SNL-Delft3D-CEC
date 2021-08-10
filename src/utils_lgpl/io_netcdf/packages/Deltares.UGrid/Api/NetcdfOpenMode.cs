namespace Deltares.UGrid.Api
{
    public enum NetcdfOpenMode
    {
        nf90_nowrite = 0,
        nf90_write = 1,
        nf90_clobber = 0,
        nf90_noclobber = 4,
        nf90_fill = 0,
        nf90_nofill = 256,
        nf90_64bit_offset = 512,
        nf90_lock = 1024,
        nf90_share = 2048
    }
}