#ifndef COLOR_INCLUDED
#define COLOR_INCLUDED

/* RGB color */
typedef struct RGB {
  unsigned char r, g, b;
} RGB;

/* HSB to RGB:
    0 <= H < 360
    0 <= S <= 1
    0 <= B <= 1
*/
void convertHSBtoRGB (double H, double S, double B, RGB* rgb);

/* Particle color palette */
typedef unsigned short PaletteIndex;

#define PaletteHues             64
#define PaletteSaturations      8
#define PaletteBrightnesses     8

#define PaletteHueMask          0xfc0
#define PaletteSaturationMask   0x038
#define PaletteBrightnessMask   0x007

#define PaletteHueShift         6
#define PaletteSaturationShift  3
#define PaletteBrightnessShift  0

#define PaletteMax              0x0fff
#define PaletteMask             0x0fff
#define PaletteSize             0x1000

/* palette macros */
#define ConvertPaletteHsbToPaletteIndex(PaletteHue,PaletteSaturation,PaletteBrightness) \
  ((PaletteIndex) ((((PaletteHue) % PaletteHues) << PaletteHueShift)	\
		   | (((PaletteSaturation) % PaletteSaturations) << PaletteSaturationShift) \
		   | (((PaletteBrightness) % PaletteBrightnesses) << PaletteBrightnessShift)))

#define ConvertRealHsbToPaletteIndex(RealHue,RealSaturation,RealBrightness) \
  ConvertPaletteHsbToPaletteIndex((int)(.5 + RealHue * PaletteHues),(int)(.5 + RealSaturation * PaletteSaturations),(int)(.5 + RealBrightness * PaletteBrightnesses))

/*
  ColorRule parameterizes the following formula:
  paletteIndex = (((state >> rightShift) & mask) * multiplier) + offset
*/
typedef struct ColorRule {
  PaletteIndex mask, offset;
  int multiplier;
  unsigned char rightShift;
} ColorRule;

/* ColorRule evaluation is simple enough to take place in a macro */
#define evalColorRule(colorRulePtr,state) \
  ((PaletteIndex) ((((((state) >> (colorRulePtr)->rightShift) & (colorRulePtr)->mask) * (colorRulePtr)->multiplier) + (colorRulePtr)->offset) & PaletteMask))

#endif /* COLOR_INCLUDED */
