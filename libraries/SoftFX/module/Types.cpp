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

FLine FRect::edge(int index) {
  FLine ln;
  switch (index) {
    case 0:
      // top
      ln.Start.X = this->X1;
      ln.Start.Y = this->Y1;
      ln.End.X = this->X2;
      ln.End.Y = this->Y1;
      break;
    case 1:
      // right
      ln.Start.X = this->X2;
      ln.Start.Y = this->Y1;
      ln.End.X = this->X2;
      ln.End.Y = this->Y2;
      break;
    case 2:
      // bottom
      ln.Start.X = this->X2;
      ln.Start.Y = this->Y2;
      ln.End.X = this->X1;
      ln.End.Y = this->Y2;
      break;
    case 3:
      // left
      ln.Start.X = this->X1;
      ln.Start.Y = this->Y2;
      ln.End.X = this->X1;
      ln.End.Y = this->Y1;
      break;
  }
  return ln;
}