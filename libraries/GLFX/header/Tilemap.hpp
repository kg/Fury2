struct CameraParam {
    int pImage;
    FX::Rectangle Rectangle;
    float Alpha;
    int ViewportX;
    int ViewportY;
};

struct TilemapLayerParam {
    short *pData;
    int Alpha;
    int X1;
    int Y1;
    int X2;
    int Y2;
    int Width;
    int Height;
    int pTileset;
    int MaskedTile;
    int Effect;
    Byte WrapX;
    Byte WrapY;
    short *pAnimationMap;
    Pixel TintColor;
};
