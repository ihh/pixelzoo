#ifndef COLOR_INCLUDED
#define COLOR_INCLUDED

/* RGB color struct */
#define RGB_max 255
typedef struct RGB {
  unsigned char r, g, b;
} RGB;

/* 24-bit HSB integer */
typedef unsigned long HSB24;

#define HSB24Hues             256
#define HSB24Saturations      256
#define HSB24Brightnesses     256

#define HSB24HueMask          0xff0000
#define HSB24SaturationMask   0x00ff00
#define HSB24BrightnessMask   0x0000ff

#define HSB24HueBits          8
#define HSB24SaturationBits   8
#define HSB24BrightnessBits   8

#define HSB24HueShift         16         /* HSB24SaturationBits + HSB24BrightnessBits */
#define HSB24SaturationShift  8          /* HSB24BrightnessBits */
#define HSB24BrightnessShift  0

#define HSB24Max              0xffffff
#define HSB24Mask             0xffffff
#define HSB24Size             0x1000000

#define HSB24White 0x00ffffff

/* Floating-point (H,S,B) tuple
    0 <= H < 1
    0 <= S <= 1
    0 <= B <= 1
*/
void ConvertRealHsbToRgb (double H, double S, double B, RGB* rgb);

/* 12-bit color palette */

/* palette constants */
#define PaletteHues             64
#define PaletteSaturations      8
#define PaletteBrightnesses     8

#define PaletteHueMask          0xfc0
#define PaletteSaturationMask   0x038
#define PaletteBrightnessMask   0x007

#define PaletteHueBits          6
#define PaletteSaturationBits   3
#define PaletteBrightnessBits   3

#define PaletteHueShift         6        /* PaletteSaturationBits + PaletteBrightnessBits */
#define PaletteSaturationShift  3        /* PaletteBrightnessBits */
#define PaletteBrightnessShift  0

#define PaletteMax              0x0fff
#define PaletteMask             0x0fff
#define PaletteSize             0x1000

#define PaletteBlack            0
#define PaletteWhite            0x0fff

/* palette data structures & types */

typedef struct Palette {
  RGB rgb[PaletteSize];
} Palette;

typedef unsigned short PaletteIndex;

/* palette functions */

void initializePalette (Palette* palette);

/* palette macros */

/* ConvertPaletteHsbToPaletteIndex:
    0 <= PaletteHue < PaletteHues
    0 <= PaletteSaturation < PaletteSaturations
    0 <= PaletteBrightness < PaletteBrightnesses
 */
#define ConvertPaletteHsbToPaletteIndex(PaletteHue,PaletteSaturation,PaletteBrightness) \
  ((PaletteIndex) ((((PaletteHue) % PaletteHues) << PaletteHueShift)	\
		   | (((PaletteSaturation) % PaletteSaturations) << PaletteSaturationShift) \
		   | (((PaletteBrightness) % PaletteBrightnesses) << PaletteBrightnessShift)))

/* ConvertPaletteHsbToRealHsb:
    0 <= PaletteHue < PaletteHues
    0 <= PaletteSaturation < PaletteSaturations
    0 <= PaletteBrightness < PaletteBrightnesses
 */
#define ConvertPaletteHsbToRealHsb(PaletteHue,PaletteSaturation,PaletteBrightness,RealHue,RealSaturation,RealBrightness) \
  (RealHue) = (double) (PaletteHue) / PaletteHues; \
  (RealSaturation) = (double) (PaletteSaturation) / (PaletteSaturations - 1); \
  (RealBrightness) = (PaletteSaturation) == 0 ? ((double) (PaletteBrightness) / (PaletteBrightnesses - 1)) : ((double) (PaletteBrightness + 1) / PaletteBrightnesses);

/* ConvertRealHsbToPaletteIndex:
    0 <= RealHue <= 1
    0 <= RealSaturation <= 1
    0 <= RealBrightness <= 1
 */
#define ConvertRealHsbToPaletteIndex(RealHue,RealSaturation,RealBrightness) \
  ConvertPaletteHsbToPaletteIndex ((int) ((RealHue) * (PaletteHues - 1)), \
				   (int) ((RealSaturation) * (PaletteSaturations - 1)), \
				   (int) ((RealBrightness) * (PaletteBrightnesses - 1)))

/* ConvertHsb24ToPaletteIndex:
    0x000000 <= HSB <= 0xffffff
 */
#define Hsb24ToPaletteHueShift         12  /* HSB24HueShift + HSB24HueBits - PaletteHueShift - PaletteHueBits */
#define Hsb24ToPaletteSaturationShift  10  /* HSB24SaturationShift + HSB24SaturationBits - PaletteSaturationShift - PaletteSaturationBits */
#define Hsb24ToPaletteBrightnessShift  5   /* HSB24BrightnessShift + HSB24BrightnessBits - PaletteBrightnessShift - PaletteBrightnessBits */
#define ConvertHsb24ToPaletteIndex(HSB) \
  ( (((HSB) >> Hsb24ToPaletteHueShift) & PaletteHueMask)	\
    | (((HSB) >> Hsb24ToPaletteSaturationShift) & PaletteSaturationMask) \
    | (((HSB) >> Hsb24ToPaletteBrightnessShift) & PaletteBrightnessMask) )


/*
  ColorRule parameterizes the following formula:
   hsb24 = (((state >> rightShift) & mask) * multiplier) + offset

  where hsb24 is a 24-bit HSB value (0x00HHSSBB in hex)
*/
typedef struct ColorRule {
  unsigned long mask, offset;
  int multiplier;
  unsigned char rightShift;
} ColorRule;

/* ColorRule evaluation is simple enough to take place in a macro */
#define evalColorRule(colorRulePtr,state) \
  ((HSB24) (((((state) >> (colorRulePtr)->rightShift) & (colorRulePtr)->mask) * (colorRulePtr)->multiplier) + (colorRulePtr)->offset))

#endif /* COLOR_INCLUDED */
