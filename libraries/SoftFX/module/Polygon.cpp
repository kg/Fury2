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

bool FPoint::inside(Rectangle *Area) {
  return (X >= Area->Left) && (Y >= Area->Top) && (X < Area->right()) && (Y < Area->bottom());
}
/*
bool TexturedVertex::inside(Rectangle *Area) {
  return (X >= Area->Left) && (Y >= Area->Top) && (X < Area->right()) && (Y < Area->bottom());
}
bool GradientVertex::inside(Rectangle *Area) {
  return (X >= Area->Left) && (Y >= Area->Top) && (X < Area->right()) && (Y < Area->bottom());
}
*/
bool FPoint::inside(Rectangle *Area, int Edge) {
  switch (Edge) {
  default:
  case 0:
    return (X >= Area->Left);
  case 1:
    return (Y >= Area->Top);
  case 2:
    return (X < Area->right());
  case 3:
    return (Y < Area->bottom());
  }
}
/*
bool TexturedVertex::inside(Rectangle *Area, int Edge) {
  switch (Edge) {
  default:
  case 0:
    return (X >= Area->Left);
  case 1:
    return (Y >= Area->Top);
  case 2:
    return (X < Area->right());
  case 3:
    return (Y < Area->bottom());
  }
}
bool GradientVertex::inside(Rectangle *Area, int Edge) {
  switch (Edge) {
  default:
  case 0:
    return (X >= Area->Left);
  case 1:
    return (Y >= Area->Top);
  case 2:
    return (X < Area->right());
  case 3:
    return (Y < Area->bottom());
  }
}
*/