start
 = body

body
 = particle_decl spc* body?
 / rule spc* body?
 / tool_decl spc* body?
 / goal_decl spc* body?
 / spc* body?

spc
  = [ \t\n\r]

particle_decl
 = "type" spc+ symbol spc* "{" spc* particle_property_list spc* "}" spc* ";" spc*

symbol
 = [A-Za-z_] [0-9A-Za-z_]*

particle_property_list
 = particle_property spc* "," spc* particle_property_list
 / particle_property

particle_property
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

compass_dir = "nw" / "ne" / "se" / "sw" / "n" / "e" / "s" / "w"

isometric_property
 = "isometric" / "directed"

sync_property
 = "sync" / "async"

rule
 = symbol dir? spc+ symbol_or_wild dir? spc* "->" symbol_or_macro dir? spc+ symbol_or_macro dir? spc* rate? caption? ";"

symbol_or_null = symbol / "_"

symbol_or_wild = symbol_or_null / "*"

symbol_or_macro = symbol_or_null / "$" macro

macro = "s" / "t"

dir
 = "." compass_dir
 / "." relative_dir

relative_dir = "fl" / "fr" / "bl" / "br" / "f" / "b" / "l" / "r"

rate = "(" spc* nonnegative_real spc* ")" spc*

nonnegative_real
 = [0-9]+
 / [0-9]* "." [0-9]+

caption = "[" [^\]]* "]" spc*

tool_decl
 = "tool" spc* "{" spc* tool_property_list spc* "}" spc* ";" spc*

tool_property_list
 = tool_property spc* "," spc* tool_property_list
 / tool_property

tool_property
 = "type" symbol_value
 / "intensity" numeric_value
 / "radius" numeric_value
 / "reserve" numeric_value
 / "recharge" numeric_value

numeric_value
 = spc* ":" spc* positive_integer

symbol_value
 = spc* ":" spc* symbol

positive_integer
 = [1-9] [0-9]*

nonnegative_integer = "0" / positive_integer

init_block
 = "init" spc* "{" spc* init_list spc* "}" spc* ";" spc*

init_list
 = init spc* "," spc* init_list
 / init

init
 = "[" spc* nonnegative_integer spc* "," spc* nonnegative_integer spc* "," spc* symbol spc* "]"

goal
 = "time" spc+ positive_integer symbol_value
 / "kill" spc+ symbol symbol_value
