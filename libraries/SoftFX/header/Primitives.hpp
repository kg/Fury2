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

#define DRAWLINE_INIT(Image, Line)                                                         \
    if (ClipLine(Image, &Line)) {                                                          \
                                                                                           \
        Pixel *pCurrent = Image->pointer(Line.Start.X, Line.Start.Y);                      \
                                                                                           \
        int iRowOffset = Image->pointer(0,1) - Image->pointer(0,0);                        \
        int delta[2] = {abs(Line.End.X - Line.Start.X), abs(Line.End.Y - Line.Start.Y)};   \
                                                                                           \
        bool alternate = (delta[1] > delta[0]);                                            \
        if (alternate) _Swap(delta[0], delta[1]);                                          \
                                                                                           \
        int numpixels = delta[0] + 1;                                                      \
        int dinc[2] = {delta[1] * 2, (delta[1] - delta[0]) * 2}, pinc[2];                  \
                                                                                           \
        {                                                                                  \
            int xinc[2] = {1, 1};                                                          \
            int yinc[2] = {1, 1};                                                          \
                                                                                           \
            if (alternate) {                                                               \
                xinc[0] = 0;                                                               \
            } else {                                                                       \
                yinc[0] = 0;                                                               \
            }                                                                              \
                                                                                           \
            if (Line.Start.X > Line.End.X) {                                               \
                xinc[0] = -xinc[0]; xinc[1] = -xinc[1];                                    \
            }                                                                              \
            if (Line.Start.Y > Line.End.Y) {                                               \
                yinc[0] = -yinc[0];    yinc[1] = -yinc[1];                                 \
            }                                                                              \
                                                                                           \
            pinc[0] = xinc[0] + (yinc[0] * iRowOffset);                                    \
            pinc[1] = xinc[1] + (yinc[1] * iRowOffset);                                    \
        }

#define DRAWLINE_BEGIN                                                                     \
        {                                                                                  \
            int d = (2 * delta[1]) - delta[0];                                             \
            int t = 0;                                                                     \
            for (int i=0; i < numpixels; i++) {

#define DRAWLINE_END                                                                       \
                t = (d >= 0);                                                              \
                d += dinc[t];                                                              \
                pCurrent += pinc[t];                                                       \
            }                                                                              \
        }                                                                                  \
    }

#define TRACELINE_INIT(Line)                                                               \
          int delta[2] = {floor(abs((double)Line.End.X - (double)Line.Start.X)),           \
          floor(abs((double)Line.End.Y - (double)Line.Start.Y))};                          \
                                                                                           \
          bool alternate = (delta[1] > delta[0]);                                          \
          if (alternate) _Swap(delta[0], delta[1]);                                        \
                                                                                           \
          int numpixels = delta[0] + 1;                                                    \
          int dinc[2] = {delta[1] * 2, (delta[1] - delta[0]) * 2};                         \
          int x = floor((double)Line.Start.X), y = floor((double)Line.Start.Y);            \
          int xinc[2] = {1, 1};                                                            \
          int yinc[2] = {1, 1};                                                            \
                                                                                           \
          {                                                                                \
                                                                                           \
              if (alternate) {                                                             \
                  xinc[0] = 0;                                                             \
              } else {                                                                     \
                  yinc[0] = 0;                                                             \
              }                                                                            \
                                                                                           \
              if (Line.Start.X > Line.End.X) {                                             \
                  xinc[0] = -xinc[0]; xinc[1] = -xinc[1];                                  \
              }                                                                            \
              if (Line.Start.Y > Line.End.Y) {                                             \
                  yinc[0] = -yinc[0]; yinc[1] = -yinc[1];                                  \
              }                                                                            \
                                                                                           \
          }

#define TRACELINE_BEGIN                                                                    \
          {                                                                                \
              int d = (2 * delta[1]) - delta[0];                                           \
              int t = 0;                                                                   \
              for (int i = 0; i < numpixels; i++) {

#define TRACELINE_END                                                                      \
                  t = (d >= 0);                                                            \
                  d += dinc[t];                                                            \
                  x += xinc[t];                                                            \
                  y += yinc[t];                                                            \
              }                                                                            \
          }

#define TRACELINEFP_INIT(Line)                                                             \
          float delta[2] = {abs(Line.End.X - Line.Start.X), abs(Line.End.Y - Line.Start.Y)};\
                                                                                           \
          bool alternate = (delta[1] > delta[0]);                                          \
          if (alternate) _Swap(delta[0], delta[1]);                                        \
                                                                                           \
          int numpixels = floor(delta[0]) + 1;                                             \
          float x = Line.Start.X, y = Line.Start.Y;                                        \
          float xinc = delta[0] / numpixels;                                               \
          float yinc = delta[1] / numpixels;                                               \

#define TRACELINEFP_BEGIN                                                                  \
          {                                                                                \
              for (int i=0; i < numpixels; i++) {

#define TRACELINEFP_END                                                                    \
                  x += xinc;                                                               \
                  y += yinc;                                                               \
              }                                                                            \
          }