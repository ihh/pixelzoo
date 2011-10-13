#include "color.h"
#include "util.h"

void ConvertRealHsbToRgb (double H, double S, double B, RGB* rgb) {
  /*
    Source: http://en.wikipedia.org/wiki/HSL_and_HSV#From_HSV
    Only difference is that 0<=H<1 instead of 0<=H<360
  */
  int sextant;
  double C, X, M, Hdash;
  unsigned char c, x, m;
  Assert (H >= 0 && H < 1, "convertHSBtoRGB: H out of range");
  Assert (S >= 0 && S <= 1, "convertHSBtoRGB: S out of range");
  Assert (B >= 0 && B <= 1, "convertHSBtoRGB: B out of range");
  C = B * S;
  Hdash = H * 6;
  sextant = (int) Hdash;
  X = C * (1 - ABS((Hdash - (sextant - (sextant % 2))) - 1));   /* Hdash % 2 = Hdash - (sextant - (sextant % 2)) */
  M = B - C;
  m = (unsigned char) (.5 + M * 255);
  c = m + (unsigned char) (.5 + C * 255);
  x = m + (unsigned char) (.5 + X * 255);
  switch (sextant) {
  case 0:
    rgb->r = c;
    rgb->g = x;
    rgb->b = m;
    break;

  case 1:
    rgb->r = x;
    rgb->g = c;
    rgb->b = m;
    break;

  case 2:
    rgb->r = m;
    rgb->g = c;
    rgb->b = x;
    break;

  case 3:
    rgb->r = m;
    rgb->g = x;
    rgb->b = c;
    break;

  case 4:
    rgb->r = x;
    rgb->g = m;
    rgb->b = c;
    break;

  case 5:
    rgb->r = c;
    rgb->g = m;
    rgb->b = x;
    break;

  default:
    /* should never get here */
    rgb->r = rgb->g = rgb->b = 0xff;
    break;
  }
}

void initializePalette (Palette* palette) {
  int h, s, b;
  double hReal, sReal, bReal;
  RGB *col;
  PaletteIndex pal;

  for (b = 0; b < PaletteBrightnesses; ++b)
    for (s = 0; s < PaletteSaturations; ++s)
      for (h = 0; h < PaletteHues; ++h) {
	pal = ConvertPaletteHsbToPaletteIndex(h,s,b);
	col = &(palette->rgb[pal]);
	ConvertPaletteHsbToRealHsb (h, s, b, hReal, sReal, bReal);
	ConvertRealHsbToRgb (hReal, sReal, bReal, col);
      }
}
