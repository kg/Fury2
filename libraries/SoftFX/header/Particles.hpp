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

enum ParticleRenderModes {
    Pixel,
    AntiAliasPixel,
    Line,
    AntiAliasLine,
    GradientLine,
    Graphic
};

struct ParticleGraphicParam {
    int FrameCount;
    Image **pFrames;
    float XCenter, YCenter;
    DoubleWord MatteColor;
    int BlitMode;
    int Alpha;
    int Frame;
};

struct ParticleType {
    Byte DecayMode;
    Byte XVDecayMode;
    Byte YVDecayMode;
    Byte AVDecayMode;
    Byte SVDecayMode;
    float XVDecay;
    float YVDecay;
    float AVDecay;
    float SVDecay;
    Byte RenderMode;
    ParticleGraphicParam *Graphic;
    FLine *Line;
    DoubleWord Color1;
    DoubleWord Color2;
};

struct Particle {
    float X, Y, A, S;
    float XV, YV, AV, SV;
    Byte Type, Active;
    Particle *pNext;
};

struct ParticleEngine {
    int Alpha;
    ParticleType *ParticleTypes;
    int BufferCount;
    Particle *ParticleBuffer;
    Particle *FirstParticle;
};