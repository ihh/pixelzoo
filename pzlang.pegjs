
start
 = body

body
 = particle_decl spc* body?
 / label spc* body?
 / spc* body?

spc
  = [ \t\n\r]

particle_decl
 = "type" spc+ symbol spc* "{" spc* member_list spc* "}" spc*

symbol
  = [A-Za-z_] [0-9A-Za-z_]*

member_identifier
 = symbol spc* "." spc* symbol
 / symbol

member_list
 = member spc* "," spc* member_list
 / member

member
 = member_variable_decl
 / handler_subroutine_decl

member_variable_decl
 = symbol spc* ":" spc* bitfield_width

bitfield_width
 = positive_integer

positive_integer
 = [1-9] [0-9]*

handler_subroutine_decl
 = message ":" spc* code

message
 = "!" spc* symbol spc*

label
 = symbol spc* ":" spc* code

code
 = subroutine_decl
 / bind_statement
 / switch_statement
 / goto_statement
 / message
 / assignment_or_increment
 / "{" spc* assignment_or_increment* code "}"

assignment_or_increment
 = member_identifier spc* "=" spc* positive_integer spc*
 / member_identifier spc* "+=" spc* positive_integer spc*
 / member_identifier spc* "-=" spc* positive_integer spc*
 / member_identifier spc* "++" spc*
 / member_identifier spc* "--" spc*
 / "++" spc* member_identifier spc*
 / "--" spc* member_identifier spc*
