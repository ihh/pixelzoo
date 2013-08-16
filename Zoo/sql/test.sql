
insert into user values (1, 'yam', 'pass', 100);

insert into world values (1, 'earth', 1, 128, 0, 1000, 2000, '<board><size>128</size><init><x>99</x><y>99</y><gvars><type>cement</type></gvars></init></board>', '<game><goal><!-- owner --></goal></game>', '<game><goal><!-- guest --></goal></game>', '<game><goal><!-- voyeur -->Hello voyeur!</goal></game>');

insert into world values (2, 'mars', 1, 128, 0, 1000, 2000, '<board><size>128</size><init><x>99</x><y>99</y><gvars><type>cement</type></gvars></init></board>', '<game><goal><!-- owner --></goal></game>', '<game><goal><!-- guest --></goal></game>', '<game><goal><!-- voyeur -->Hello voyeur!</goal></game>');

insert into lock values (1, 1, 1, 3000, 4000, 'proto', 'compiled', 'turn');

insert into particle values ('wall', 1, NULL, 1, '<particle><name>wall</name><rate>0</rate><rule><nop/></rule></particle>');
insert into particle values ('cement', 1, NULL, 1, '<particle><name>cement</name><rate>1</rate><rule><nop/></rule></particle>');

insert into dependency values ('cement', 'wall');


insert into tool values ('cement spray (fine)', 1, '<tool><name>cement spray (fine)</name><size>2</size><gstate>cement</gstate><overwrite><gstate>empty</gstate></overwrite><spray>1000</spray><reserve>1000</reserve><recharge>100</recharge></tool>');
insert into tool values ('cement spray (medium)', 1, '<tool><name>cement spray (medium)</name><size>4</size><gstate>cement</gstate><overwrite><gstate>empty</gstate></overwrite><spray>1000</spray><reserve>1000</reserve><recharge>100</recharge></tool>');
insert into tool values ('cement spray (coarse)', 1, '<tool><name>cement spray (coarse)</name><size>6</size><gstate>cement</gstate><overwrite><gstate>empty</gstate></overwrite><spray>1000</spray><reserve>1000</reserve><recharge>100</recharge></tool>');
insert into tool values ('acid spray (small)', 1, '<tool><name>acid spray (small)</name><size>6</size><gstate>acid</gstate><overwrite><gstate>empty</gstate></overwrite><spray>2</spray><reserve>1000</reserve><recharge>100</recharge></tool>');
insert into tool values ('acid spray (large)', 1, '<tool><name>acid spray (large)</name><size>16</size><gstate>acid</gstate><overwrite><gstate>empty</gstate></overwrite><spray>2</spray><reserve>1000</reserve><recharge>100</recharge></tool>');
insert into tool values ('seed spray', 1, '<tool><name>seed spray</name><size>2</size><gstate>seed</gstate><overwrite><gstate>empty</gstate></overwrite><spray>1000</spray><reserve>1000</reserve><recharge>100</recharge></tool>');
insert into tool values ('cyclobs', 1, '<tool><name>cyclobs</name><size>8</size><gvars><type>cyclobs</type><val var="species">3</val></gvars><overwrite><gstate>empty</gstate></overwrite><spray>100</spray><reserve>5</reserve><recharge>100</recharge></tool>');
insert into tool values ('perfume spray', 1, '<tool><name>perfume spray</name><size>4</size><gstate>perfume</gstate><overwrite><gstate>empty</gstate></overwrite><spray>2500</spray><reserve>1000</reserve><recharge>100</recharge></tool>');

