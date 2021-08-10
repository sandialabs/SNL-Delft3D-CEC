using System.ComponentModel;

namespace Deltares.UGrid.Api
{
    public enum GridLocationType
    {
        [Description("Unknown")]
        UG_LOC_NONE = 0,
        [Description("Node")]
        UG_LOC_NODE = 1,
        [Description("Edge")]
        UG_LOC_EDGE = 2,
        [Description("Cell")]
        UG_LOC_FACE = 4,
        [Description("Volume")]
        UG_LOC_VOL = 8,
        [Description("All")]
        UG_LOC_ALL2D = UG_LOC_NODE + UG_LOC_EDGE + UG_LOC_FACE
    }
}