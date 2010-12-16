#ifndef COLOR_INCLUDED
#define COLOR_INCLUDED

/* RGB color */
typedef struct RGB {
  unsigned char r, g, b;
} RGB;

/* HSB to RGB:
    0 <= H < 1
    0 <= S <= 1
    0 <= B <= 1
*/
void convertHSBtoRGB (double H, double S, double B, RGB* rgb);

/* 24-bit HSB */
typedef unsigned long HSB24;

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

/* ConvertPaletteHsbToPaletteIndex:
    0 <= PaletteHue < PaletteHues
    0 <= PaletteSaturation < PaletteSaturations
    0 <= PaletteBrightness < PaletteBrightnesses
 */
#define ConvertPaletteHsbToPaletteIndex(PaletteHue,PaletteSaturation,PaletteBrightness) \
  ((PaletteIndex) ((((PaletteHue) % PaletteHues) << PaletteHueShift)	\
		   | (((PaletteSaturation) % PaletteSaturations) << PaletteSaturationShift) \
		   | (((PaletteBrightness) % PaletteBrightnesses) << PaletteBrightnessShift)))

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
#define ConvertHsb24ToPaletteIndex(HSB) \
  ConvertRealHsbToPaletteIndex ( ((double)(((HSB)>>16)&0xff)) / 255., ((double)(((HSB)>>8)&0xff)) / 255., ((double)((HSB)&0xff)) / 255. )

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
