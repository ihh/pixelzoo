
start
 = body

body
 = particle_decl spc* body?
 / label spc* body?
 / spc* body?

spc
  = [ \t\n\r]

particle_decl
 = "type" spc+ symbol spc* "{" spc* member_list spc* "}" spc* ";"

symbol
 = [A-Za-z_] [0-9A-Za-z_]*

member_list
 = member spc* "," spc* member_list
 / member

member
 = member_variable_decl
 / handler_subroutine_decl

member_variable_decl
 = symbol spc* ":" spc* power_of_two

power_of_two
 = positive_integer

positive_integer
 = [1-9] [0-9]*

handler_subroutine_decl
 = message ":" spc* code

message
 = "!" spc* symbol spc*

label
 = symbol spc* ":" spc* code

delimited_code
 = assignment_or_increment_statement* code

code
 = bind_statement
 / switch_statement
 / goto_statement
 / addressed_message
 / assignment_or_increment_statement
 / "{" spc* delimited_code "}"

assignment_or_increment_statement
 = assignment_or_increment_expr spc* ";" spc* 

assignment_or_increment_expr
 = ass_inc_lhs spc* "=" spc* positive_integer
 / ass_inc_lhs spc* "+=" spc* positive_integer
 / ass_inc_lhs spc* "-=" spc* positive_integer
 / ass_inc_lhs spc* "++"
 / ass_inc_lhs spc* "--"
 / "++" spc* ass_inc_lhs
 / "--" spc* ass_inc_lhs

ass_inc_lhs
 = local_member_identifier
 / symbol

local_member_identifier
 = location_identifier "." spc* symbol
 / symbol

location_identifier
 = "@" symbol spc*

location_expr
 = location_identifier
 / "[" spc* integer spc* "," spc* integer spc* "]" spc*

integer
 = positive_integer
 / sign spc* positive_integer
 / "0"

sign = "+" / "-"

bind_statement
 = "bind" spc* "(" spc* location_identifier "=" spc* location_expr ")" spc* "{" spc* bind_case_block spc* "}" spc*

bind_case_block
 = "case" spc+ symbol spc* ":" spc* code? spc* break bind_case_block?
 / default_clause

default_clause
 = "default" spc* ":" spc* code? spc* break

break
 = "break" spc* ";" spc*

switch_statement
 = "switch" spc* "(" spc* local_member_identifier spc* ")" spc* "{" spc* switch_case_block spc* "}" spc*

switch_case_block
 = "case" spc+ integer spc* ":" spc* code? spc* break switch_case_block?
 / default_clause

goto_statement
 = "goto" spc+ symbol spc* ";" spc*

addressed_message
 = location_identifier message ";" spc*
