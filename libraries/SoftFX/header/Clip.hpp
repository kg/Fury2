/*
SoftFX (Software graphics manipulation library)
Copyright (C) 2003 Kevin Gadd

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

Export int Clip2D_Simple(Rectangle *Rect, Image *Dest, Image *Source, int &X, int &Y, int &CX, int &CY);
Export int Clip2D_SimpleRect(Rectangle *Rect, Image *Dest, Image *Source, Rectangle *DestRect, int &CX, int &CY);
Export int Clip2D_SimpleRectWrap(Rectangle *Rect, Image *Dest, Image *Source, Rectangle *DestRect);
Export int Clip2D_PairedRect(Rectangle *Rect, Rectangle *RectS, Image *Dest, Image *Source, Rectangle *DestRect, Rectangle *SourceRect, int *CropOutput);

Export int Clip2D_PairToRect(Rectangle *Dest, Rectangle *Source, Rectangle *Clip);

Export int ClipRectangle_Rect(Rectangle *Rect, Rectangle *Clip);
Export int ClipRectangle_Image(Rectangle *Rect, Image *Image);
Export int ClipRectangle_ImageClipRect(Rectangle *Rect, Image *Image);
int ClipRectangle_ImageClipRect(Rectangle *Rect, Image *Image, int *CropOutput);

extern bool enableClipping;