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
#include "../header/Filters.hpp"
#include "../header/Blitters.hpp"
#include "../header/Resample.hpp"

inline float xpow(float v, float exp) {
  if (exp == 1.0f) return v;
  if (exp == 0.0f) return 1.0f;
  if (exp == 2.0f) return v * v;
  return pow(v, exp);
}

#define decayUpdate(p, t, id) \
  if (t.id##VDecayMode != pdmNone) { \
    switch(t.id##VDecayMode) { \
      case pdmSet: \
        p->id##V = (t.id##VDecay * _One(state.elapsed)) + (p->id##V * (1 - _One(state.elapsed))); \
        break; \
      case pdmAdd: \
        p->id##V += t.id##VDecay * state.elapsed; \
        break; \
      case pdmMultiply: \
        p->id##V *= xpow((float)t.id##VDecay, (float)state.elapsed); \
        break; \
      case pdmExponent: \
        p->id##V = xpow(p->id##V, xpow((float)t.id##VDecay, (float)state.elapsed)); \
        break; \
      case pdmRandomAdd: \
        p->id##V += t.id##VDecay * engine.RNG.rand() * state.elapsed; \
        break; \
      case pdmRandomMultiply: \
        if (t.id##VDecay >= 0.0f) { \
          p->id##V *= xpow((float)xpow((float)t.id##VDecay, (float)engine.RNG.rand()), state.elapsed); \
        } else { \
          p->id##V *= xpow((float)(1.0f + (abs(t.id##VDecay) * engine.RNG.rand())), state.elapsed); \
        } \
        break; \
    } \
  }

#define decayUpdateEx(p, w, id) \
  if (this->id##VDecayMode != pdmNone) { \
    switch(this->id##VDecayMode) { \
      case pdmSet: \
        p.id##V = (this->id##VDecay * _One(w)) + (p.id##V * (1-_One(w))); \
        break; \
      case pdmAdd: \
        p.id##V += this->id##VDecay * w; \
        break; \
      case pdmMultiply: \
        p.id##V *= xpow(this->id##VDecay, w); \
        break; \
      case pdmExponent: \
        p.id##V = xpow(p.id##V, xpow(this->id##VDecay, w)); \
        break; \
      case pdmRandomAdd: \
        p.id##V += this->id##VDecay * w * engine.RNG.rand(); \
        break; \
      case pdmRandomMultiply: \
        if (this->id##VDecay >= 0.0f) { \
          p.id##V *= xpow((double)xpow(this->id##VDecay, w), (double)engine.RNG.rand()); \
        } else { \
          p.id##V *= 1.0f + (abs((double)xpow(this->id##VDecay, w)) * engine.RNG.rand()); \
        } \
        break; \
    } \
  }

#define propUpdate(p, t, id) \
  if (t.id##Mode != pdmNone) { \
    switch(t.id##Mode) { \
      case pdmSet: \
        p->id = (p->id##V * _One(state.elapsed)) + (p->id * (1 - _One(state.elapsed))); \
        break; \
      case pdmAdd: \
        p->id += p->id##V * state.elapsed; \
        break; \
      case pdmMultiply: \
        p->id *= xpow((float)p->id##V, (float)state.elapsed); \
        break; \
      case pdmExponent: \
        p->id = xpow(p->id##V, xpow((float)p->id##V, (float)state.elapsed)); \
        break; \
      case pdmRandomAdd: \
        p->id += p->id##V * engine.RNG.rand() * state.elapsed; \
        break; \
      case pdmRandomMultiply: \
        if (t.id##Mode >= 0.0f) { \
          p->id *= xpow((float)xpow((float)p->id##V, (float)engine.RNG.rand()), state.elapsed); \
        } else { \
          p->id *= xpow((float)(1.0f + (abs(p->id##V) * engine.RNG.rand())), state.elapsed); \
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
  if (!particle.inside(this->Size)) return;
  int c = this->ParticleCounts[particle.Type];
  if (c >= list.size()) {
    list.push_back(particle);
  } else {
    list[c] = particle;
  }
  c++;
  this->ParticleCounts[particle.Type] = c;
}

void ParticleEngine::prespawn(ParticleList& list, int newcount) {
  if (list.capacity() < newcount) {
    list.reserve(newcount);
  }
}

inline void ParticleEngine::spawn(Particle& particle) {
  if (particle.Type < 0) return;
  if (particle.Type >= this->Particles.size()) return;
  this->spawn(particle, this->Particles[particle.Type]);
}

void ParticleEngine::render(ParticleCamera& camera) {
  ParticleEngineState state = ParticleEngineState(*this, camera);
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
    state.poly.Allocate(4);
    state.type = (*typeiter);
    state.renderTarget = camera.pRenderTargets[ClipValue(state.type->RenderTarget, 0, camera.RenderTargetCount)];
    state.renderer = getRenderer(state.type->RenderMode);
    state.theta = 0;
    state.distance = 0;
    if (state.type->RenderType == prtGraphic) {
      if (state.type->Graphic) {
        Image* f = state.type->Graphic->pFrames[0];
        float w = f->Width / 2, h = f->Height / 2;
        state.theta = atan2(h, w);
        state.distance = sqrt((w * w) + (h * h));
      }
    }
    while (i--) {
      state.particle = &(*iter);
      iter->render(state);
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

void ParticleEngine::tick(float elapsed) {
  ParticleEngineState state = ParticleEngineState(*this, elapsed);
  ParticleList::iterator iter;
  ParticleListList::iterator listiter;
  ParticleCountList::iterator countiter;
  ParticleTypeList::iterator typeiter;
  ParticleModifierList::iterator modifier;
  ParticleGeneratorList::iterator generator;
  bool kill = false;

  generator = this->Generators.begin();
  while (generator != this->Generators.end()) {
    (*generator)->tick(state);
    ++generator;
  }

  int t = 0;
  typeiter = this->Types.begin();
  listiter = this->Particles.begin();
  countiter = this->ParticleCounts.begin();
  while (typeiter != this->Types.end()) {
    state.type = (*typeiter);
    state.typeList = &(*listiter);
    int i = 0;
    int c = *countiter;
    iter = listiter->begin();
    modifier = this->Modifiers.begin();
    while (modifier != this->Modifiers.end()) {
      bool r = ((t == (*modifier)->RequireType) || ((*modifier)->RequireType == -1));
      bool e = ((t != (*modifier)->ExcludeType));
      if (e && r) {
        iter = listiter->begin();
        i = 0;
        c = *countiter;
        while (i < c) {
          state.particle = &(*iter);
          (*modifier)->tick(state);
          ++iter;
          i++;
        }
      }
      ++modifier;
    }
    iter = listiter->begin();
    i = 0;
    c = *countiter;
    while (i < c) {
      state.particle = &(*iter);
      kill = false;
      kill = iter->tick(state);
      if (kill) {
        if (state.type->DieCallback) state.type->DieCallback(this, state.type, state.particle, state.type->UserData);
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
    t++;
  }
}

#define generateProperty(id) \
  p.id = this->New##id + (this->Random##id * (engine.RNG.rand(2.0f) - 1.0f));
#define generatePropertyP(id) \
  p.id = this->New##id + (this->Random##id * (engine.RNG.rand()));

void ParticleGenerator::tick(ParticleEngineState& state) {
  ParticleEngine& engine = *(state.engine);
  float d = this->GenerateDelay;
  if (d == 0) d = 1;
  d = 1 / d;
  if (this->CurrentDelay < 0) this->CurrentDelay = 0;
  this->CurrentDelay += state.elapsed;
  while (this->CurrentDelay > d) {
    this->CurrentDelay -= d;
    Particle p;
    p.Type = this->Type;
    p.Frame = 0;
    int t = ClipValue(this->Type, 0, engine.Types.size() - 1);
    ParticleListList::iterator list = engine.Particles.begin();
    while (t--) {
      ++list;
    }
    int i = this->GenerateRate;
    engine.prespawn(*list, i + engine.ParticleCounts[ClipValue(this->Type, 0, engine.Types.size() - 1)]);
    while (i--) {
      float ri = (this->GenerateRotation + (this->RandomGenerateRotation * engine.RNG.rand()));
      float r = (this->CurrentRotation * Radian);
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
      float w = this->NewR + (this->RandomR * engine.RNG.rand());
      p.X += (xd * w);
      p.Y += (yd * w);
      w = this->NewRV + (this->RandomRV * engine.RNG.rand());
      p.XV += (xd * w);
      p.YV += (yd * w);
      engine.spawn(p, *list);
      this->CurrentRotation += ri;
    }
  }
}

void ParticleModifier::tick(ParticleEngineState& state) {
  ParticleEngine& engine = *(state.engine);
  Particle& particle = *(state.particle);
  if (particle.X < this->Area.X1) return;
  if (particle.Y < this->Area.Y1) return;
  if (particle.X > this->Area.X2) return;
  if (particle.Y > this->Area.Y2) return;
  float xd = particle.X - this->X;
  float yd = particle.Y - this->Y;
  float distance2 = (xd * xd) + (yd * yd);
  float distance = sqrt(distance2);
  float weight = state.elapsed;
  if (this->Range > 0) {
    weight = (this->Range - distance) / this->Range * (this->RangeScale);
    if (weight > 1) weight = 1;
    if (weight < 0) weight = 0;
    weight *= state.elapsed;
  }
  decayUpdateEx(particle, weight, X);
  decayUpdateEx(particle, weight, Y);
  decayUpdateEx(particle, weight, A);
  decayUpdateEx(particle, weight, L);
  if ((weight != 0) && (this->Attraction != 0) && (distance != 0) && (this->Mass != 0)) {
    weight = engine.gravity(this->Mass, 1.0f, distance2) * weight * this->Attraction;
    particle.XV -= weight * (particle.X - this->X);
    particle.YV -= weight * (particle.Y - this->Y);
  }
}

bool Particle::tick(ParticleEngineState& state) {
  ParticleEngine& engine = *(state.engine);
  ParticleType& type = *(state.type);
  decayUpdate(this, type, X);
  decayUpdate(this, type, Y);
  decayUpdate(this, type, A);
  decayUpdate(this, type, L);
  //propUpdate(this, type, X);
  //propUpdate(this, type, Y);
  //propUpdate(this, type, A);
  //propUpdate(this, type, L);
  this->X += this->XV * state.elapsed;
  this->Y += this->YV * state.elapsed;
  this->A += this->AV * state.elapsed;
  this->L += this->LV * state.elapsed;
  if ((type.LBehavior == plbRemove) && (this->L <= ParticleMinimumL)) return true;
  if (!this->inside(engine.Size)) return true;
  if (type.Graphic) {
    this->Frame += type.Graphic->FrameIncrement * state.elapsed;
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
  return false;
}

void Particle::render(ParticleEngineState& state) {
  ParticleType& type = *(state.type);
  ParticleCamera& camera = *(state.camera);
  float x = this->X - camera.ViewportX + camera.Rectangle.Left;
  float y = this->Y - camera.ViewportY + camera.Rectangle.Top;
  Pixel c = type.Color1, c2 = type.Color2;
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
  Image *surface = state.renderTarget;
  Rectangle rctLine;
  Rectangle rctDest;
  Stroke stroke;
  StrokePoint strokePoints[2];
  RenderFunction *renderer;
  Image *pImage;
  float s;
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
    case prtGradientLine:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          c2 = ScaleAlpha(type.Color2, ClipByte(_One(this->A + this->AV) * camera.Alpha * 255.0f));
          break;
      }
      rctLine.setValuesAbsolute(floor(x), floor(y), floor(x + this->XV), floor(y + this->YV));
      FilterSimple_Line_Gradient(surface, &rctLine, c, c2);
      break;
    case prtAntiAliasGradientLine:
      switch (type.AMode) {
        case pamFadeAndScale:
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          c2 = ScaleAlpha(type.Color2, ClipByte(_One(this->A + this->AV) * camera.Alpha * 255.0f));
          break;
      }
      FilterSimple_Line_Gradient_AA(surface, x, y, x + this->XV, y + this->YV, c, c2);
      break;
    case prtStroke:
      s = 1.0f;
      switch (type.AMode) {
        case pamScale:
          s *= _One(this->A);
          break;
        case pamFadeAndScale:
          s *= _One(this->A);
        case pamFade:
          c = ScaleAlpha(c, ClipByte(this->A * camera.Alpha * 255.0f));
          break;
      }
      renderer = state.renderer;
      stroke.PointCount = 2;
      stroke.Loop = 0;
      stroke.Softness = type.Softness;
      stroke.Points = strokePoints;
      strokePoints[0].X = x;
      strokePoints[0].Y = y;
      strokePoints[0].Color = c.V;
      strokePoints[0].Thickness = type.Thickness * s;
      strokePoints[1].X = x + this->XV;
      strokePoints[1].Y = y + this->YV;
      strokePoints[1].Color = c.V;
      strokePoints[1].Thickness = type.Thickness * _One(this->A + this->AV);
      FilterSimple_RenderStroke(surface, &stroke, renderer, 0);
    case prtGraphic:
      if (type.Graphic) {
        int a = type.Graphic->Alpha;
        s = 1.0f;
        float r = 0;
        switch (type.AMode) {
          case pamFadeAndScale:
            s *= _One(this->A);
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
            renderer = state.renderer;
            float px[4], py[4];
            float w = pImage->Width;
            float h = pImage->Height;
            TexturedPolygon& poly = state.poly;
            poly.Empty();
            r *= Radian;
            s /= 2;
            w *= s; h *= s;
            if (!((state.camera->Rectangle.Left > x) || (state.camera->Rectangle.Top > y) || (state.camera->Rectangle.right() < x) || (state.camera->Rectangle.bottom() < y))) {
              if (state.theta != 0) {
                Rotate4Points(w, h, r, px, py, state.theta);
              } else {
                Rotate4Points(w, h, r, px, py);
              }
              poly.Append(TexturedVertex(px[0] + x, py[0] + y, 0, 0));
              poly.Append(TexturedVertex(px[1] + x, py[1] + y, pImage->Width - 1, 0));
              poly.Append(TexturedVertex(px[2] + x, py[2] + y, pImage->Width - 1, pImage->Height - 1));
              poly.Append(TexturedVertex(px[3] + x, py[3] + y, 0, pImage->Height - 1));
              FilterSimple_ConvexPolygon_Textured(surface, pImage, &poly, DefaultSampleFunction, renderer, c.V);
            }
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

Export int SetParticleEngineSize(ParticleEngine *Engine, FRect *Size) {
  if (!Engine) return Failure;
  if (!Size) return Failure;
  Engine->Size = *Size;
  return Success;
}

Export int UpdateParticleEngine(ParticleEngine *Engine, float ElapsedTime) {
  if (!Engine) return Failure;
  Engine->tick(ElapsedTime);
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