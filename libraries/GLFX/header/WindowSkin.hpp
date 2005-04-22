enum wsSections {
    wsTopLeft = 0,
    wsTop = 1,
    wsTopRight = 2,
    wsRight = 3,
    wsBottomRight = 4,
    wsBottom = 5,
    wsBottomLeft = 6,
    wsLeft = 7,
    wsMiddle = 8
};

enum wsSectionFlags {
    sfTopLeft = 1,
    sfTop = 2,
    sfTopRight = 4,
    sfRight = 8,
    sfBottomRight = 16,
    sfBottom = 32,
    sfBottomLeft = 64,
    sfLeft = 128,
    sfMiddle = 256,
    sfEdges = 127,
    sfAll = 511
};

struct WindowSkinParam {
    int *pImages;
    int Alpha;
    Pixel MaskColor;
    Pixel CornerColors[4];
    int EdgeOffsets[4];
    Pixel TintColors[9];
    Byte BackgroundMode;
    Byte EdgeMode;
    Byte RenderMode;
};