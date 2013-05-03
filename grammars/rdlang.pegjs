start
 = spc* body

body
 = statement spc* body
 / statement

statement
 = particle_decl
 / rule
 / param_decl
 / tool_decl
 / goal_decl
 / size_decl
 / init_block

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

isometric_property
 = "isometric" / "directed"

sync_property
 = "sync" / "async"

rule
 = lhs_source spc+ lhs_target spc* lhs_qualifier? spc* "->" spc* rhs_source spc+ rhs_target spc* rate_clause? spc* ";"

lhs_source
 = symbol dir?

lhs_target
 = symbol_or_wild dir?

rhs_source
 = symbol_or_lhs_macro dir?

rhs_target
 = symbol_or_lhs_macro dir?

symbol_or_null = symbol / "_"

symbol_or_wild = symbol_or_null / "*"

symbol_or_lhs_macro = symbol_or_null / lhs_macro

lhs_macro = "$" ("s" / "t")
rhs_macro = "$" ("S" / "T")

dir
 = "." compass_dir
 / "." relative_dir

compass_dir = "nw" / "ne" / "se" / "sw" / "n" / "e" / "s" / "w"

relative_dir = "fl" / "fr" / "bl" / "br" / "f" / "b" / "l" / "r"

rate_clause
 = ":" spc* sum_expr

sum_expr
  = product_expr spc* "+" spc* sum_expr
  / product_expr

product_expr
  = primary_expr spc* ("*" / "/") spc* product_expr
  / primary_expr

primary_expr
  = nonnegative_real
  / symbol
  / "(" spc* sum_expr spc* ")"

rate_expr
 = nonnegative_real

nonnegative_real
 = [0-9]+
 / [0-9]* "." [0-9]+

caption
 = "caption" string_value

string_value
 = spc* ":" spc* "[" [^\]] "]"

param_decl
 = symbol spc* "=" spc* sum_expr spc* ";"

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
 / "overwrite" symbol_or_wild_value

numeric_value
 = spc* ":" spc* positive_integer

symbol_value
 = spc* ":" spc* symbol

symbol_or_wild_value
 = spc* ":" spc* symbol_or_wild

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

goal_decl
 = "timeout" spc+ positive_integer symbol_value spc* ";" spc*
 / "extinct" spc+ symbol symbol_value spc* ";" spc*

size_decl
 = "size" spc* "[" spc* positive_integer spc* "," spc* positive_integer spc* "]" spc* ";" spc*
