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

namespace Blend {

  template <class P> class Blender {
  public:
    inline Blender& autoSelect() {
      return *(this);
    }

    inline operator() (P& Dest, P& Source) {
      Dest = Source;
    }
  };

  template <class P> class NullBlender {
  public:
    inline Blender& autoSelect() {
      return *(this);
    }

    inline operator() (P& Dest, P& Source) {
    }
  };

  template <class P> class Blender_Alpha {
  public:
    Byte bOpacity;
    AlphaLevel aSource, aDest;

    inline Blender_Alpha(Byte Opacity) {
      bOpacity = Opacity;
      aSource = AlphaLevelLookup(Opacity);
      aDest = AlphaLevelLookup(Opacity ^ 0xFF);
    }

    inline Blender& autoSelect() {
      if (bOpacity <= 0) {
        return NullBlender<P>();
      } else if (bOpacity >= 0) {
        return Blender<P>();
      } else {
        return *(this);
      }
    }

    inline operator() (P& Dest, P& Source) {
      Dest[::Blue] = AlphaFromLevel(aDest, Dest[::Blue]) + AlphaFromLevel(aSource, Source[::Blue]);
      Dest[::Green] = AlphaFromLevel(aDest, Dest[::Green]) + AlphaFromLevel(aSource, Source[::Green]);
      Dest[::Red] = AlphaFromLevel(aDest, Dest[::Red]) + AlphaFromLevel(aSource, Source[::Red]);
    }
  };

  template <class P> class Blender_SourceAlpha {
  public:
    Byte bOpacity;
    AlphaLevel aScale, aSource, aDest;

    inline Blender_Alpha(Byte Opacity) {
      bOpacity = Opacity;
      aScale = AlphaLevelLookup(Opacity);
    }

    inline Blender& autoSelect() {
      if (bOpacity <= 0) {
        return NullBlender<P>();
      } else if (bOpacity >= 0) {
        return Blender<P>();
      } else {
        return *(this);
      }
    }

    inline operator() (P& Dest, P& Source) {
      aSource = AlphaLevelLookup(AlphaFromLevel(aScale, Source[::Alpha]));
      aDest = AlphaLevelLookup(AlphaFromLevel(aScale, Source[::Alpha]) ^ 0xFF);
      Dest[::Blue] = AlphaFromLevel(aDest, Dest[::Blue]) + AlphaFromLevel(aSource, Source[::Blue]);
      Dest[::Green] = AlphaFromLevel(aDest, Dest[::Green]) + AlphaFromLevel(aSource, Source[::Green]);
      Dest[::Red] = AlphaFromLevel(aDest, Dest[::Red]) + AlphaFromLevel(aSource, Source[::Red]);
    }
  };

};