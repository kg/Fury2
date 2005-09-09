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
    this->setValues(Hue_Min, Saturation_Min, Value_Min, 0);
  }

  HSVA(Pixel Color) {
    this->setPixel(Color);
  }

  HSVA(int h, int s, int v) {
    this->setValues(h, s, v, 255);
  }

  HSVA(int h, int s, int v, int a) {
    this->setValues(h, s, v, a);
  }

  Pixel getPixel() {
    if (Value == Value_Min) return Pixel(0, 0, 0, Alpha);
    if ((Value == Value_Max) && (Saturation == Saturation_Min)) return Pixel(255, 255, 255, Alpha);
    int segment, remainder, range, color_range, a, b, c, rb;
    segment = Hue / Hue_Unit;
    remainder = Hue - (segment * Hue_Unit);
    range = Value * 255 / Value_Max;
    if (Saturation == Saturation_Min) return Pixel(range, range, range, Alpha);
    color_range = (Saturation) * range / Saturation_Max;
    c = (Saturation_Max - Saturation) * range / Saturation_Max;
    b = (remainder * color_range) / Hue_Unit + c;
    rb = color_range - b + c + c;
    a = color_range + c;
    switch (segment) {
      case 0:
        return Pixel(a,b,c,Alpha);
        break;
      case 1:
        return Pixel(rb,a,c,Alpha);
        break;
      case 2:
        return Pixel(c,a,b,Alpha);
        break;
      case 3:
        return Pixel(c,rb,a,Alpha);
        break;
      case 4:
        return Pixel(b,c,a,Alpha);
        break;
      case 5:
        return Pixel(a,c,rb,Alpha);
        break;
    }
    return Pixel(0,0,0,0);
  }

  void setPixel(Pixel NewColor) {
    int max, min;
    max = _Max(NewColor[::Red], _Max(NewColor[::Green], NewColor[::Blue]));
    min = _Min(NewColor[::Red], _Min(NewColor[::Green], NewColor[::Blue]));
    if (max == min) {
      setValues(Hue_Min, Saturation_Min, min * Value_Max / 255, NewColor[::Alpha]);
      return;
    } else if (max == 0) {
      setValues(Hue_Min, Saturation_Min, Value_Min, NewColor[::Alpha]);
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
    setValues(h, s, v, NewColor[::Alpha]);
    return;
  }

  inline void setValues(int h, int s, int v, int a) {
    Hue = WrapValue(h, Hue_Min, Hue_Max);
    Saturation = ClipValue(s, Saturation_Min, Saturation_Max);
    Value = ClipValue(v, Value_Min, Value_Max);
    Alpha = ClipByte(a);
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

