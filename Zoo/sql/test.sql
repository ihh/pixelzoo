
insert into user values (1, 'yam', 'pass', 100);
insert into user values (2, 'dog', 'pass', 100);

insert into world_meta values (1, 128, 1, 1, 1, 60, 10, 'guest', 'id', '<!-- incumbent -->', '<!-- challenger -->');

insert into world values (1, 'earth', 1, 2, 0, 1000, 2000, '<board><size>128</size><init><x>9</x><y>9</y><gvars><type>guest</type><val var="id">2</val></gvars></init></board>');
insert into world values (2, 'mars', 1, 2, 0, 1000, 2000, '<board><size>128</size><init><x>9</x><y>9</y><gvars><type>guest</type><val var="id">2</val></gvars></init></board>');

insert into toolbox values (1, 'standard tools', 6);

insert into particle values ('empty', 1, NULL, 1, '<particle><name>empty</name><rate>0</rate><rule><nop/></rule></particle>');

insert into particle values ('guest', 1, NULL, 1, '<particle><name>guest</name><vars><varsize><name>id</name><size>24</size></varsize></vars><colrule><scheme>(hsb 100)</scheme></colrule><rate>.005</rate><rule><scheme>`(rule ,(neumann-drift))</scheme></rule></particle>');
insert into tool values (1, 'guest placer', 1, '<tool><scheme>`(tool (name "guest placer") (size 2) (gvars (type "guest") (val (@ (var "id")) ,challenger-id)) (overwrite (gstate "empty")) (spray 1000) (reserve 1000) (recharge 100))</scheme></tool>');
insert into toolbox_tool values (1, 1, 0);
insert into tool_dependency values (1, 'guest');

insert into particle values ('acid', 1, NULL, 1, '<particle><name>acid</name><colrule><scheme>(hsb 200)</scheme></colrule><rate>.1</rate><rule><scheme>(rule (random-rule .05 (kill-self) (neumann-drift nop-rule suicide-pact)))</scheme></rule></particle>');
insert into tool_dependency values ('acid', 'empty');
insert into tool values (2, 'acid spray', 1, '<tool><scheme>`(tool (name "acid spray") (size 2) (gstate "acid") (overwrite (gstate "empty")) (spray 1000) (reserve 1000) (recharge 100))</scheme></tool>');
insert into toolbox_tool values (1, 2, 1);
insert into tool_dependency values (2, 'acid');
