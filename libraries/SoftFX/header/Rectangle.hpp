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

class Rectangle {
public:
    int Left, Top;
    int Width, Height;

    Rectangle() {
        Left = Top = 0;
        Width = Height = 0;
    }

    Rectangle(int X, int Y, int X2, int Y2) {
        Left = X;
        Top = Y;
        Width = X2 - X;
        Height = Y2 - Y;
    }

    Rectangle(Rectangle *Source) {
        if (Source) {
            this->Left = Source->Left;
            this->Top = Source->Top;
            this->Width = Source->Width;
            this->Height = Source->Height;
        }
    }

    inline bool empty() {
        return !((Width > 0) && (Height > 0));
    }

    inline int right() {
        return Width + Left;
    }

    inline int right_exclusive() {
        if (Width > 0) {
            return Left + (Width - 1);
        } else if (Width < 0) {
            return Left + (Width + 1);
        } else {
            return Left;
        }
    }

    inline void setRight(int value) {
        Width = value - Left;
    }

    inline int bottom() {
        return Height + Top;
    }

    inline int bottom_exclusive() {
        if (Height > 0) {
            return Top + (Height - 1);
        } else if (Height < 0) {
            return Top + (Height + 1);
        } else {
            return Top;
        }
    }

    inline void setBottom(int value) {
        Height = value - Top;
    }

    inline void setValues(int x, int y, int w, int h) {
        Left = x;
        Top = y;
        Width = w;
        Height = h;
    }

    inline void setValuesAbsolute(int x, int y, int x2, int y2) {
        Left = x;
        Top = y;
        Width = x2 - x;
        Height = y2 - y;
    }

    inline void translate(int x, int y) {
        Left += x;
        Top += y;
    }

    inline void normalize() {
        if (Width < 0) {
            Left += Width;
            Width = -Width;
        }
        if (Height < 0) {
            Top += Height;
            Height = -Height;
        }
    }

    inline bool intersect(Rectangle& Rect) {
        if (this->Left > Rect.right()) return false;
        if (this->Top > Rect.bottom()) return false;
        if (this->right() < Rect.Left) return false;
        if (this->bottom() < Rect.Top) return false;
        return true;
    }

    template <class T> inline void getLine(T& start, T& end) {
      start.X = this->Left;
      start.Y = this->Top;
      end.X = this->Left + this->Width + ((this->Width > 0) ? 1 : ((this->Width != 0) ? -1 : 0));
      end.Y = this->Top + this->Height + ((this->Height > 0) ? 1 : ((this->Height != 0) ? -1 : 0));
      return;
    }
};
