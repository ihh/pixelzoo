
insert into user values (1, 'yam', 'pass', 100);

insert into world values (1, 'earth', 1, 128, 0, 1000, 2000, '<board><size>128</size><init><x>99</x><y>99</y><gvars><type>cement</type></gvars></init></board>', '<game><goal><!-- owner --></goal></game>', '<game><goal><!-- guest --></goal></game>', '<game><goal><!-- voyeur -->Hello voyeur!</goal></game>');

insert into world values (2, 'mars', 1, 128, 0, 1000, 2000, '<board><size>128</size><init><x>99</x><y>99</y><gvars><type>cement</type></gvars></init></board>', '<game><goal><!-- owner --></goal></game>', '<game><goal><!-- guest --></goal></game>', '<game><goal><!-- voyeur -->Hello voyeur!</goal></game>');

insert into lock values (1, 1, 1, 3000, 4000, 'proto', 'compiled', 'turn');

insert into particle values ('wall', 1, NULL, 1, '<particle><name>wall</name><rate>0</rate><rule><nop/></rule></particle>');
insert into particle values ('cement', 1, NULL, 1, '<particle><name>cement</name><rate>1</rate><rule><nop/></rule></particle>');

insert into dependency values ('cement', 'wall');
