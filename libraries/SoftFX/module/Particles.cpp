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

#include "../header/SoftFX Main.hpp"
#include "../header/MersenneTwister.h"
#include "../header/Particles.hpp"
#include "../header/Polygon.hpp"
#include "../header/Filters.hpp"
#include "../header/Blitters.hpp"
#include "../header/Resample.hpp"

#define decayUpdate(p, t, id) \
  if (t.id##VDecayMode != pdmNone) { \
    switch(t.id##VDecayMode) { \
      case pdmAdd: \
        p->id##V += t.id##VDecay; \
        break; \
      case pdmMultiply: \
        p->id##V *= t.id##VDecay; \
        break; \
      case pdmExponent: \
        p->id##V = pow(p->id##V, t.id##VDecay); \
        break; \
      case pdmRandomAdd: \
        p->id##V += t.id##VDecay * engine.RNG.rand(); \
        break; \
      case pdmRandomMultiply: \
        if (t.id##VDecay >= 0.0f) { \
          p->id##V *= pow((double)t.id##VDecay, (double)engine.RNG.rand()); \
        } else { \
          p->id##V *= 1.0f + (abs(t.id##VDecay) * engine.RNG.rand()); \
        } \
        break; \
    } \
  }

#define decayUpdateEx(p, w, id) \
  if (this->id##VDecayMode != pdmNone) { \
    switch(this->id##VDecayMode) { \
      case pdmAdd: \
        p.id##V += this->id##VDecay * w; \
        break; \
      case pdmMultiply: \
        p.id##V *= pow(this->id##VDecay, w); \
        break; \
      case pdmExponent: \
        p.id##V = (pow(p.id##V, this->id##VDecay) * w) + (p.id##V * (1 - w)); \
        break; \
      case pdmRandomAdd: \
        p.id##V += this->id##VDecay * w * engine.RNG.rand(); \
        break; \
      case pdmRandomMultiply: \
        if (this->id##VDecay >= 0.0f) { \
          p.id##V *= pow((double)pow(this->id##VDecay, w), (double)engine.RNG.rand()); \
        } else { \
          p.id##V *= 1.0f + (abs((double)pow(this->id##VDecay, w)) * engine.RNG.rand()); \
        } \
        break; \
    } \
  }

RenderFunction* getRenderer(int id) {
  switch (id) {
    case 1:
      return RenderFunction_SourceAlpha;
    case 2:
      return RenderFunction_Additive;
    case 3:
      return RenderFunction_Subtractive;
    case 4:
      return RenderFunction_Merge;
    case 5:
      return RenderFunction_Shadow;
    case 6:
      return RenderFunction_Screen;
    case 7:
      return RenderFunction_Additive_SourceAlpha;
    case 8:
      return RenderFunction_Subtractive_SourceAlpha;
    case 9:
      return RenderFunction_Font_SourceAlpha;
    default:
      return 0;
  }
}

void ParticleEngine::spawn(Particle& particle, ParticleList& list) {
  this->ParticleCounts[particle.Type] = this->ParticleCounts[particle.Type] + 1;
  if (this->ParticleCounts[particle.Type] > list.size()) {
    list.push_back(particle);
  } else {
    list[this->ParticleCounts[particle.Type] - 1] = particle;
  }
}

void ParticleEngine::spawn(Particle& particle) {
  if (particle.Type < 0) return;
  if (particle.Type >= this->Particles.size()) return;
  this->spawn(particle, this->Particles[particle.Type]);
}

void ParticleEngine::render(ParticleCamera& camera) {
  ParticleList::iterator iter;
  ParticleListList::iterator listiter;
  ParticleTypeList::iterator typeiter;

  for (int r = 0; r < camera.RenderTargetCount; r++) {
    Rectangle clip;
    clip = camera.Rectangle;
    camera.pRenderTargets[r]->clipRectangle(&clip);
    camera.pRenderTargets[r]->setClipRectangle(&clip);
  }

  int t = 0;
  typeiter = this->Types.begin();
  listiter = this->Particles.begin();
  while (typeiter != this->Types.end()) {
    int i = this->ParticleCounts[t];
    if (i > listiter->size()) i = listiter->size();
    iter = listiter->begin();
    while (i--) {
      iter->render(*this, **typeiter, camera);
      ++iter;
    }
    ++listiter;
    ++typeiter;
    t++;
  }

  for (int r = 0; r < camera.RenderTargetCount; r++) {
    camera.pRenderTargets[r]->setClipRectangle(camera.pRenderTargets[r]->getRectangle());
  }
}

void ParticleEngine::tick() {
  ParticleList::iterator iter;
  ParticleListList::iterator listiter;
  ParticleCountList::iterator countiter;
  ParticleTypeList::iterator typeiter;
  ParticleModifierList::iterator modifier;
  ParticleGeneratorList::iterator generator;
  bool kill = false;

  generator = this->Generators.begin();
  while (generator != this->Generators.end()) {
    (*generator)->tick(*this);
    ++generator;
  }

  typeiter = this->Types.begin();
  listiter = this->Particles.begin();
  countiter = this->ParticleCounts.begin();
  while (typeiter != this->Types.end()) {
    iter = listiter->begin();
    int i = 0;
    int c = *countiter;
    while (i < c) {
      kill = false;
      modifier = this->Modifiers.begin();
      while (modifier != this->Modifiers.end()) {
        bool r = ((iter->Type == (*modifier)->RequireType) || ((*modifier)->RequireType == -1));
        bool e = ((iter->Type != (*modifier)->ExcludeType));
        if (e && r)
          (*modifier)->tick(*this, *iter);
        modifier++;
      }
      iter->tick(*this, **typeiter);
      kill = (((*typeiter)->LBehavior == plbRemove) && (iter->L <= ParticleMinimumL));
      if (kill) {
        Particle temp = (*listiter)[c - 1];
        (*listiter)[c - 1] = *iter;
        *iter = temp;
        c--;
      } else {
        ++iter;
        i++;
      }
    }
    *countiter = c;
    ++countiter;
    ++listiter;
    ++typeiter;
  }
}

#define generateProperty(id) \
  p.id = this->New##id + (this->Random##id * (engine.RNG.rand(2.0f) - 1.0f));
#define generatePropertyP(id) \
  p.id = this->New##id + (this->Random##id * (engine.RNG.rand()));

    /*
      int Type;
      int GenerateRate;
      float GenerateDelay;
      float CurrentDelay;
      float NewX, NewY, NewL, NewA;
      float NewXV, NewYV, NewLV, NewAV;
      float RandomX, RandomY, RandomL, RandomA;
      float RandomXV, RandomYV, RandomLV, RandomAV;
    */

void ParticleGenerator::tick(ParticleEngine& engine) {
  this->CurrentDelay += 1.0f;
  if (this->CurrentDelay > this->GenerateDelay) {
    this->CurrentDelay -= this->GenerateDelay;
  } else {
    return;
  }
  Particle p;
  p.Type = this->Type;
  p.Frame = 0;
  int t = ClipValue(this->Type, 0, engine.Types.size() - 1);
  ParticleListList::iterator list = engine.Particles.begin();
  while (t--) {
    ++list;
  }
  int i = this->GenerateRate;
  float r = this->Rotation * Radian;
  float ri = this->GenerateRotation * Radian;
  while (i--) {
    generateProperty(X);
    generateProperty(Y);
    generatePropertyP(A);
    generatePropertyP(L);
    generateProperty(XV);
    generateProperty(YV);
    generatePropertyP(AV);
    generatePropertyP(LV);
    float xd = sin(r);
    float yd = -cos(r);
    p.X += (xd * this->NewR);
    p.Y += (yd * this->NewR);
    p.XV += (xd * this->NewRV);
    p.YV += (yd * this->NewRV);
    engine.spawn(p, *list);
    r += ri;
  }
}

void ParticleModifier::tick(ParticleEngine& engine, Particle& particle) {
  if (particle.X < this->Area.X1) return;
  if (particle.Y < this->Area.Y1) return;
  if (particle.X > this->Area.X2) return;
  if (particle.Y > this->Area.Y2) return;
  float xd = abs(particle.X - this->X);
  float yd = abs(particle.Y - this->Y);
  float distance = sqrt((xd * xd) + (yd * yd));
  float weight;
  if (this->RangeScale > 0) {
    weight = (this->Range - distance) / (this->RangeScale);
    if (weight > 1) weight = 1;
    if (weight < 0) weight = 0;
  } else if (this->RangeScale < 0) {
    weight = (distance) / abs(this->RangeScale);
    if (weight > 1) weight = 1;
    if (weight < 0) weight = 0;
  } else {
    weight = 1;
  }
  decayUpdateEx(particle, weight, X);
  decayUpdateEx(particle, weight, Y);
  decayUpdateEx(particle, weight, A);
  decayUpdateEx(particle, weight, L);
  if (this->Attraction != 0) {
    float a = AngleBetween(FPoint(particle.X, particle.Y), FPoint(this->X, this->Y)) * Radian;
    float s = sqrt((particle.XV * particle.XV) + (particle.YV * particle.YV));
    if ((s >= this->MaxAttraction) && (this->MaxAttraction > 0)) {
    } else {
      float xd = this->Attraction * weight * sin(a);
      float yd = this->Attraction * weight * -cos(a);
      particle.XV += xd;
      particle.YV += yd;
    }
  }
}

void Particle::tick(ParticleEngine &engine, ParticleType &type) {
  decayUpdate(this, type, X);
  decayUpdate(this, type, Y);
  decayUpdate(this, type, A);
  decayUpdate(this, type, L);
  this->X += this->XV;
  this->Y += this->YV;
  this->A += this->AV;
  this->L += this->LV;
  if (type.Graphic) {
    this->Frame += type.Graphic->FrameIncrement;
    switch (type.Graphic->LoopMode) {
      case 0:
        if (this->Frame < 0) this->Frame = 0;
        if (this->Frame > type.Graphic->FrameCount - 1) this->Frame = type.Graphic->FrameCount - 1;
        break;
      case 1:
        if (this->Frame < 0) this->Frame += type.Graphic->FrameCount - 1;
        if (this->Frame > type.Graphic->FrameCount - 1) this->Frame -= type.Graphic->FrameCount - 1;
        break;
      case 2:
        // not implemented
        break;
    }
  } else {
    this->Frame = 0;
  }
}

void Particle::render(ParticleEngine &engine, ParticleType &type, ParticleCamera& camera) {
  float x = this->X - camera.ViewportX + camera.Rectangle.Left;
  float y = this->Y - camera.ViewportY + camera.Rectangle.Top;
  int f;
  if (type.RenderMode == prtGraphic)
    f = floor(this->Frame);
  Pixel c = type.Color1;
  if (type.LColorMode != plcNone) {
    switch (type.LColorMode) {
      case plcFade:
        c = ScaleAlpha(c, ClipByte(this->L * 255.0f));
        break;
      case plcInterpolate:
        float l = this->L;
        if (l > 1) l = 1;
        if (l < 0) l = 0;
        c = Pixel(type.Color1, type.Color2, 1 - l);
        break;
    }
  }
  Image *surface;
  Rectangle rctLine;
  Rectangle rctDest;
  Stroke stroke;
  StrokePoint strokePoints[2];
  RenderFunction *renderer;
  Image *pImage;
  surface = camera.pRenderTargets[ClipValue(type.RenderTarget, 0, camera.RenderTargetCount)];
  switch (type.RenderType) {
    default:
      break;
    case prtPixel:
      surface->setPixel(floor(x), floor(y), c);
      break;
    case prtAntiAliasPixel:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      surface->setPixelAA(x, y, c);
      break;
    case prtLine:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      rctLine.setValuesAbsolute(floor(x), floor(y), floor(x + this->XV), floor(y + this->YV));
      FilterSimple_Line(surface, &rctLine, c);
      break;
    case prtAntiAliasLine:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      FilterSimple_Line_AA(surface, x, y, x + this->XV, y + this->YV, c);
      break;
    case prtStroke:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      renderer = getRenderer(type.RenderMode);
      stroke.PointCount = 2;
      stroke.Loop = 0;
      stroke.Softness = type.Softness;
      stroke.Points = strokePoints;
      strokePoints[0].X = x;
      strokePoints[0].Y = y;
      strokePoints[0].Color = c.V;
      strokePoints[0].Thickness = type.Thickness;
      strokePoints[1].X = x + this->XV;
      strokePoints[1].Y = y + this->YV;
      strokePoints[1].Color = c.V;
      strokePoints[1].Thickness = type.Thickness;
      FilterSimple_RenderStroke(surface, &stroke, renderer, 0);
    case prtGraphic:
      if (type.Graphic) {
        int a = type.Graphic->Alpha;
        float s = 1.0f;
        float r = 0;
        switch (type.AMode) {
          case pamFadeAndScale:
            s *= this->A;
            if (s < 0) s = 0;
            a = ClipByte(a * camera.Alpha * this->A);
            c = ScaleAlpha(c, a);
            break;
          case pamFade:
            a = ClipByte(a * camera.Alpha * this->A);
            c = ScaleAlpha(c, a);
            break;
          case pamRotate:
            r = this->A * 360.0f;
            break;
        }
        pImage = type.Graphic->pFrames[(int)floor(this->Frame)];
        if (pImage) {
          if ((s == 1.0f) && (r == 0.0f)) {
            rctDest.Left = ceil(x - type.Graphic->XCenter);
            rctDest.Top = ceil(y - type.Graphic->YCenter);
            rctDest.Width = pImage->Width;
            rctDest.Height = pImage->Height;
            switch (type.RenderMode) {
              default:
                BlitSimple_Normal_Tint_Opacity(surface, pImage, &rctDest, 0, 0, c, a);
                break;
              case 1:
                BlitSimple_SourceAlpha_Tint_Opacity(surface, pImage, &rctDest, 0, 0, c, a);
                break;
              case 2:
                BlitSimple_Additive_Opacity(surface, pImage, &rctDest, 0, 0, a);
                break;
              case 3:
                BlitSimple_Subtractive_Opacity(surface, pImage, &rctDest, 0, 0, a);
                break;
              case 4:
                BlitSimple_Merge_Opacity(surface, pImage, &rctDest, 0, 0, a);
                break;
              case 5:
                break;
              case 6:
                BlitSimple_Screen_Opacity(surface, pImage, &rctDest, 0, 0, a);
                break;
              case 7:
                BlitSimple_Additive_SourceAlpha_Opacity(surface, pImage, &rctDest, 0, 0, a);
                break;
              case 8:
                BlitSimple_Subtractive_SourceAlpha_Opacity(surface, pImage, &rctDest, 0, 0, a);
                break;
              case 9:
                BlitSimple_Font_SourceAlpha_RGB_Opacity(surface, pImage, &rctDest, 0, 0, c, 255);
                break;
            }
          } else {
            renderer = getRenderer(type.RenderMode);
            float w = pImage->Width;
            float h = pImage->Height;
            float px, py;
            TexturedPolygon poly;
            poly.Allocate(4);
            r *= Radian;
            s /= 2;
            w *= s; h *= s;
            poly.Empty();
            px = -w; py = -h;
            RotatePoint(px, py, r);
            px += x; py += y;
            poly.Append(TexturedVertex(px, py, 0, 0));
            px = w; py = -h;
            RotatePoint(px, py, r);
            px += x; py += y;
            poly.Append(TexturedVertex(px, py, pImage->Width - 1, 0));
            px = w; py = h;
            RotatePoint(px, py, r);
            px += x; py += y;
            poly.Append(TexturedVertex(px, py, pImage->Width - 1, pImage->Height - 1));
            px = -w; py = h;
            RotatePoint(px, py, r);
            px += x; py += y;
            poly.Append(TexturedVertex(px, py, 0, pImage->Height - 1));
            FilterSimple_ConvexPolygon_Textured(surface, pImage, &poly, DefaultSampleFunction, renderer, c.V);
          }
        }
      }
      break;
  }
}

Export ParticleEngine* CreateParticleEngine() {
  return new ParticleEngine();
}

Export int DestroyParticleEngine(ParticleEngine *Engine) {
  if (Engine) {
    delete Engine;
    return Success;
  }
  return Failure;
}

Export int RemoveParticleModifier(ParticleEngine *Engine, ParticleModifier *Modifier) {
  if (!Engine) return Failure;
  if (!Modifier) return Failure;
  ParticleModifierList::iterator iter = Engine->Modifiers.begin();
  while (iter != Engine->Modifiers.end()) {
    if (*iter == Modifier) {
      Engine->Modifiers.erase(iter);
      return Success;
    }
    ++iter;
  }
  return Trivial_Success;
}

Export int AddParticleModifier(ParticleEngine *Engine, ParticleModifier *NewModifier) {
  if (!Engine) return Failure;
  if (!NewModifier) return Failure;
  RemoveParticleModifier(Engine, NewModifier);
  Engine->Modifiers.push_back(NewModifier);
  return Success;
}

Export int RemoveParticleGenerator(ParticleEngine *Engine, ParticleGenerator *Generator) {
  if (!Engine) return Failure;
  if (!Generator) return Failure;
  ParticleGeneratorList::iterator iter = Engine->Generators.begin();
  while (iter != Engine->Generators.end()) {
    if (*iter == Generator) {
      Engine->Generators.erase(iter);
      return Success;
    }
    ++iter;
  }
  return Trivial_Success;
}

Export int AddParticleGenerator(ParticleEngine *Engine, ParticleGenerator *NewGenerator) {
  if (!Engine) return Failure;
  if (!NewGenerator) return Failure;
  RemoveParticleGenerator(Engine, NewGenerator);
  Engine->Generators.push_back(NewGenerator);
  return Success;
}

Export int RemoveParticleType(ParticleEngine *Engine, ParticleType *Type) {
  if (!Engine) return Failure;
  if (!Type) return Failure;
  ParticleTypeList::iterator iter = Engine->Types.begin();
  ParticleListList::iterator liter = Engine->Particles.begin();
  ParticleCountList::iterator citer = Engine->ParticleCounts.begin();
  while (iter != Engine->Types.end()) {
    if (*iter == Type) {
      Engine->Types.erase(iter);
      Engine->Particles.erase(liter);
      Engine->ParticleCounts.erase(citer);
      return Success;
    }
    ++iter;
    ++liter;
    ++citer;
  }
  return Trivial_Success;
}

Export int AddParticleType(ParticleEngine *Engine, ParticleType *NewType) {
  if (!Engine) return Failure;
  if (!NewType) return Failure;
  RemoveParticleType(Engine, NewType);
  Engine->Types.push_back(NewType);
  Engine->Particles.push_back(ParticleList());
  Engine->ParticleCounts.push_back(0);
  return Success;
}

Export int SpawnParticle(ParticleEngine *Engine, Particle *NewParticle) {
  if (!Engine) return Failure;
  if (!NewParticle) return Failure;
  Engine->spawn(*NewParticle);
  return Success;
}

Export int EmptyParticleEngine(ParticleEngine *Engine) {
  if (!Engine) return Failure;
  ParticleListList::iterator liter = Engine->Particles.begin();
  ParticleCountList::iterator citer = Engine->ParticleCounts.begin();
  while (liter != Engine->Particles.end()) {
    liter->clear();
    *citer = 0;
    ++liter;
    ++citer;
  }
  return Success;
}

Export int UpdateParticleEngine(ParticleEngine *Engine) {
  if (!Engine) return Failure;
  Engine->tick();
  return Success;
}

Export int RenderParticleEngine(ParticleEngine *Engine, ParticleCamera *Camera) {
  if (!Engine) return Failure;
  if (!Camera) return Failure;
  Engine->render(*Camera);
  return Success;
}

Export int GetParticleCount(ParticleEngine *Engine, int Type) {
  if (!Engine) return Failure;
  ParticleCountList::iterator citer = Engine->ParticleCounts.begin();
  int i = 0, c = 0;
  while (citer != Engine->ParticleCounts.end()) {
    if ((i == Type) || (Type < 0))
      c += *citer;
    ++citer;
  }
  return c;
}