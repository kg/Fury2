struct CameraParam {
    int *pRenderTargets;
    int RenderTargetCount;
    FX::Rectangle Rectangle;
    float Alpha;
    int ViewportX;
    int ViewportY;

    inline int pImage() {
      if (RenderTargetCount >= 1) return pRenderTargets[0];
      return 0;
    }
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
    Byte RenderTarget;
    Byte Reserved;
    short *pAnimationMap;
    Pixel TintColor;
};
