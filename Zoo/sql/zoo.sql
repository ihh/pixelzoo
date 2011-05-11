
PRAGMA foreign_keys = ON;

CREATE TABLE image (
	name varchar(255) PRIMARY KEY,  -- the name of this image
	xml TEXT  -- SVG image XML
	);

CREATE TABLE particle (
	name varchar(255) PRIMARY KEY,  -- the name of this Particle
	image_name varchar(255) REFERENCES image(name) ON DELETE SET NULL ON UPDATE CASCADE,
	cost DECIMAL,
	xml TEXT  -- Particle XML
	);

CREATE TABLE tool (
	name varchar(255) PRIMARY KEY,  -- the name of this Tool
	xml TEXT  -- Tool XML
	);

CREATE TABLE particle_dep (  -- Particle dependencies
	particle_name varchar(255) REFERENCES particle(name) ON DELETE CASCADE ON UPDATE CASCADE,  -- the dependent type
	dep_name varchar(255) REFERENCES particle(name) ON DELETE RESTRICT ON UPDATE RESTRICT,  -- the depended-upon type
	PRIMARY KEY (particle_name, dep_name)
	);

CREATE TABLE user (
	id INTEGER PRIMARY KEY,   -- the UserID
	username VARCHAR(15),   -- same max length as a Twitter handle
        password TEXT,
	cash DECIMAL  -- user's current cash level
	);

CREATE TABLE role (
        id   INTEGER PRIMARY KEY,
        name TEXT
	);

insert into role values (1, 'artist');  -- user can upload new images
insert into role values (2, 'vandal');  -- user can replace existing images
insert into role values (3, 'smith');  -- user can upload new tools
insert into role values (4, 'maker');  -- user can replace existing tools
insert into role values (5, 'coder');  -- user can upload new types
insert into role values (6, 'hacker');  -- user can replace existing types

CREATE TABLE user_role (
        user_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,
        role_id INTEGER REFERENCES role(id) ON DELETE CASCADE ON UPDATE CASCADE,
        PRIMARY KEY (user_id, role_id)
	);

CREATE TABLE inventory (
	user_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,   -- the UserID of the particle owner
	particle_name TEXT REFERENCES particle(name) ON DELETE CASCADE ON UPDATE CASCADE,  -- the particle type
	amount INTEGER,  -- the number of particles owned
	PRIMARY KEY (user_id, particle_name)
	);

CREATE TABLE world (
	id INTEGER PRIMARY KEY,   -- the WorldID
	name TEXT,  -- the name of the world
	owner_id INTEGER REFERENCES user(id) ON DELETE SET NULL ON UPDATE SET NULL,   -- the UserID of the world owner
	board_size INTEGER,   -- size of the board
	board_time INTEGER,   -- number of "microticks" on the board clock
	board_xml TEXT,  -- current state of the Board
	owner_game_xml TEXT,  -- Game headers for owner's turn
	guest_game_xml TEXT,  -- Game headers for guest's turn
	voyeur_game_xml TEXT  -- Game headers for voyeur's turn (world/XXX/view)
	);

CREATE TABLE lock (
	lock_id INTEGER PRIMARY KEY,   -- the LockID
	world_id INTEGER REFERENCES world(id) ON DELETE CASCADE ON UPDATE CASCADE,   -- the WorldID
	owner_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,   -- the UserID of the lock owner
	expiry_time TEXT,  -- lock expiration time
	proto_xml TEXT,  -- temporary assembled board
	compiled_xml TEXT,  -- temporary compiled board
	turn_xml TEXT  -- the lock owner's turn
	);
