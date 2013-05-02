start
 = body

body
 = particle_decl spc* body?
 / rule spc* body?
 / spc* body?

spc
  = [ \t\n\r]

particle_decl
 = "type" spc+ symbol spc* "{" spc* property_list spc* "}" spc* ";" spc*

symbol
 = [A-Za-z_] [0-9A-Za-z_]*

property_list
 = property spc* "," spc* property_list
 / property

property
 = icon_property
 / neighborhood_property
 / isometric_property
 / sync_property

icon_property
 = "icon" spc* ":" spc* image_path

image_path
 = [A-Za-z0-9] [A-Za-z0-9/\-_]*

neighborhood_property
 = "moore" / "neumann" / "bishop"
 / "dir" spc* ":" spc* compass_dir

compass_dir = "n" / "e" / "s" / "w" / "nw" / "ne" / "se" / "sw"

isometric_property
 = "isometric" / "directed"

sync_property
 = "sync" / "async"

rule
 = symbol spc+ symbol_or_wild spc* "->" symbol_or_macro spc+ symbol_or_macro spc* rate? caption? ";"

symbol_or_null = symbol / "_"

symbol_or_wild = symbol_or_null / "*"

symbol_or_macro = symbol_or_null / "$" macro

macro = "s" / "t" / "sl" / "sr" / "sb" / "tl" / "tr" / "tb"

rate = "(" spc* nonnegative_real spc* ")" spc*

nonnegative_real
 = [0-9]+
 / [0-9]* "." [0-9]+

caption = "[" [^\]]* "]" spc*
