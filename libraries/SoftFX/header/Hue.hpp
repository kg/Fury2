const Word Hue_Unit = 600;
const Word Hue_Min = 0;
const Word Hue_Max = (Hue_Unit * 6) - 1;
const Word Saturation_Min = 0;
const Word Saturation_Max = 2550;
const Word Value_Min = 0;
const Word Value_Max = 2550;

class HSVA {
public:
  short Hue;
  short Saturation;
  short Value;
  Byte Alpha;
private:
  Byte Padding;

public:
  HSVA() {
    this->setValuesFast(Hue_Min, Saturation_Min, Value_Min, 0);
  }

  HSVA(Pixel Color) {
    this->setPixel(Color);
  }

  HSVA(int h, int s, int v) {
    this->setValuesFast(h, s, v, 255);
  }

  HSVA(int h, int s, int v, int a) {
    this->setValuesFast(h, s, v, a);
  }

  Pixel getPixel() {
    if (Value <= Value_Min) return Pixel(0, 0, 0, Alpha);
    if ((Value >= Value_Max) && (Saturation <= Saturation_Min)) return Pixel(255, 255, 255, Alpha);
    if (Value > Value_Max) Value = Value_Max;
    if (Saturation > Saturation_Max) Saturation = Saturation_Max;
    int segment, remainder, range, color_range, a, b, c, rb;
    range = Value * 255 / Value_Max;
    if (Saturation <= Saturation_Min) return Pixel(range, range, range, Alpha);
    segment = Hue / Hue_Unit;
    remainder = Hue - (segment * Hue_Unit);
    color_range = (Saturation) * range / Saturation_Max;
    c = (Saturation_Max - Saturation) * range / Saturation_Max;
    b = (remainder * color_range) / Hue_Unit + c;
    rb = color_range - b + c + c;
    a = color_range + c;
    switch (segment) {
      case 0:
        return Pixel(a,b,c,Alpha);
      case 1:
        return Pixel(rb,a,c,Alpha);
      case 2:
        return Pixel(c,a,b,Alpha);
      case 3:
        return Pixel(c,rb,a,Alpha);
      case 4:
        return Pixel(b,c,a,Alpha);
      case 5:
        return Pixel(a,c,rb,Alpha);
      default:
        return Pixel(0,0,0,0);
    }
  }

  void setPixel(Pixel NewColor) {
    int max, min;
    max = _Max(NewColor[::Red], _Max(NewColor[::Green], NewColor[::Blue]));
    min = _Min(NewColor[::Red], _Min(NewColor[::Green], NewColor[::Blue]));
    if (max == min) {
      setValuesFast(Hue_Min, Saturation_Min, min * Value_Max / 255, NewColor[::Alpha]);
      return;
    } else if (max == 0) {
      setValuesFast(Hue_Min, Saturation_Min, Value_Min, NewColor[::Alpha]);
      return;
    }
    int v = max * Value_Max / 255;
    int b = (max - min);
    int s = b * Saturation_Max / max;
    int h, d;
    if (NewColor[::Red] == max) {
      d = NewColor[::Green] - NewColor[::Blue];
      h = d * Hue_Unit / b;
    } else if (NewColor[::Green] == max) {
      d = NewColor[::Blue] - NewColor[::Red];
      h = (d * Hue_Unit / b) + (2 * Hue_Unit);
    } else {
      d = NewColor[::Red] - NewColor[::Green];
      h = (d * Hue_Unit / b) + (4 * Hue_Unit);
    }
    Hue = h;
    Saturation = s;
    Value = v;
    Alpha = NewColor[::Alpha];
//    setValues(h, s, v, NewColor[::Alpha]);
    return;
  }

  inline void setValues(int h, int s, int v, int a) {
    Hue = WrapValue(h, Hue_Min, Hue_Max);
    Saturation = ClipValue(s, Saturation_Min, Saturation_Max);
    Value = ClipValue(v, Value_Min, Value_Max);
    Alpha = ClipByte(a);
  }

  inline void setValuesFast(int h, int s, int v, int a) {
    Hue = WrapValue(h, Hue_Min, Hue_Max);
    Saturation = s;
    Value = v;
    Alpha = a;
  }

  inline void clip() {
    Hue = WrapValue(Hue, Hue_Min, Hue_Max);
    Saturation = ClipValue(Saturation, Saturation_Min, Saturation_Max);
    Value = ClipValue(Value, Value_Min, Value_Max);
    Alpha = ClipByte(Alpha);
  }

  inline void setHue(int h) {
    Hue = WrapValue(h, Hue_Min, Hue_Max);
  }

  inline void setSaturation(int s) {
    Saturation = ClipValue(s, Saturation_Min, Saturation_Max);
  }

  inline void setValue(int v) {
    Value = ClipValue(v, Value_Min, Value_Max);
  }

  inline void setAlpha(int a) {
    Alpha = ClipByte(a);
  }

};

inline Pixel adjustHSV(Pixel Input, int HueOffset, int SaturationOffset, int ValueOffset) {
  int max, min;
  int h, s, v;
  max = _Max(Input[::Red], _Max(Input[::Green], Input[::Blue]));
  min = _Min(Input[::Red], _Min(Input[::Green], Input[::Blue]));
  if (max == min) {
    h = Hue_Min;
    s = Saturation_Min;
    v = (min * Value_Max / 255);
  } else if (max == 0) {
    h = Hue_Min;
    s = Saturation_Min;
    v = Value_Min;
  } else {
    v = max * Value_Max / 255;
    int b = (max - min);
    s = b * Saturation_Max / max;
    int d;
    if (Input[::Red] == max) {
      d = Input[::Green] - Input[::Blue];
      h = d * Hue_Unit / b;
    } else if (Input[::Green] == max) {
      d = Input[::Blue] - Input[::Red];
      h = (d * Hue_Unit / b) + (2 * Hue_Unit);
    } else {
      d = Input[::Red] - Input[::Green];
      h = (d * Hue_Unit / b) + (4 * Hue_Unit);
    }
  }
  h = WrapValue(h + HueOffset, Hue_Min, Hue_Max);
  v += ValueOffset;
  s += SaturationOffset;
  if (v <= Value_Min) return Pixel(0, 0, 0, Input[::Alpha]);
  if ((v >= Value_Max) && (s <= Saturation_Min)) return Pixel(255, 255, 255, Input[::Alpha]);
  if (v > Value_Max) v = Value_Max;
  if (s > Saturation_Max) s = Saturation_Max;
  int segment, remainder, range, color_range, a, b, c, rb;
  range = v * 255 / Value_Max;
  if (s <= Saturation_Min) return Pixel(range, range, range, Input[::Alpha]);
  segment = h / Hue_Unit;
  remainder = h - (segment * Hue_Unit);
  color_range = (s) * range / Saturation_Max;
  c = (Saturation_Max - s) * range / Saturation_Max;
  b = (remainder * color_range) / Hue_Unit + c;
  rb = color_range - b + c + c;
  a = color_range + c;
  switch (segment) {
    case 0:
      return Pixel(a,b,c,Input[::Alpha]);
    case 1:
      return Pixel(rb,a,c,Input[::Alpha]);
    case 2:
      return Pixel(c,a,b,Input[::Alpha]);
    case 3:
      return Pixel(c,rb,a,Input[::Alpha]);
    case 4:
      return Pixel(b,c,a,Input[::Alpha]);
    case 5:
      return Pixel(a,c,rb,Input[::Alpha]);
    default:
      return Pixel(0,0,0,0);
  }
}