
PRAGMA foreign_keys = ON;

CREATE TABLE image (
	name varchar(255) PRIMARY KEY,  -- the name of this image
        creator_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,
	xml TEXT  -- SVG image XML
	);

CREATE TABLE particle (
	name varchar(255) PRIMARY KEY,  -- the Type of this Particle, as referenced in update rules, etc.
        creator_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,
	image_id varchar(255) REFERENCES image(name) ON DELETE SET NULL ON UPDATE CASCADE,
	cost DECIMAL,
	xml TEXT  -- Particle XML
	);

CREATE TABLE tool (
        id INTEGER PRIMARY KEY,
	name varchar(255),  -- the displayed name of this Tool
        creator_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,
	xml TEXT  -- Tool XML
	);

CREATE TABLE toolbox (
        id integer PRIMARY KEY,
	name varchar(255),  -- the name of this Toolbox
	max_tools integer -- maximum allowable tools
	);

CREATE TABLE particle_tool (
        particle_name varchar(255) PRIMARY KEY REFERENCES particle(name),
        tool_id integer REFERENCES tool(id)
	);

CREATE TABLE toolbox_tool (
        toolbox_id integer REFERENCES toolbox(id),
        tool_id integer REFERENCES tool(id),
	is_default integer,
	PRIMARY KEY (toolbox_id, tool_id)
	);

CREATE TABLE dependency (  -- Particle-Particle dependencies
	ancestor_id varchar(255) REFERENCES particle(name) ON DELETE CASCADE ON UPDATE CASCADE,  -- the type that owns the original event
	descendant_id varchar(255) REFERENCES particle(name) ON DELETE RESTRICT ON UPDATE RESTRICT,  -- the type that may be created any number of events downstream
	PRIMARY KEY (ancestor_id, descendant_id)
	);

CREATE TABLE tool_dependency (  -- Tool-Particle dependencies
	tool_id INTEGER REFERENCES tool(id) ON DELETE CASCADE ON UPDATE CASCADE,  -- the tool
	particle_id varchar(255) REFERENCES particle(name) ON DELETE RESTRICT ON UPDATE RESTRICT,  -- the particle being created by the tool
	PRIMARY KEY (tool_id, particle_id)
	);

CREATE TABLE user (
	id INTEGER PRIMARY KEY AUTOINCREMENT,   -- the UserID
	username VARCHAR(15),   -- same max length as a Twitter handle
        password TEXT,
	cash DECIMAL  -- user's current cash level
	);

CREATE TABLE role (
        id   INTEGER PRIMARY KEY,
        name TEXT
	);

insert into role values (1, 'artist');  -- user can upload images
insert into role values (2, 'maker');  -- user can upload tools
insert into role values (3, 'hacker');  -- user can upload types

CREATE TABLE user_role (
        user_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,
        role_id INTEGER REFERENCES role(id) ON DELETE CASCADE ON UPDATE CASCADE,
        PRIMARY KEY (user_id, role_id)
	);

CREATE TABLE inventory (
	user_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,   -- the UserID of the particle owner
	particle_id TEXT REFERENCES particle(name) ON DELETE CASCADE ON UPDATE CASCADE,  -- the particle type
	amount INTEGER,  -- the number of particles owned
	PRIMARY KEY (user_id, particle_id)
	);

CREATE TABLE world_meta (
	id INTEGER PRIMARY KEY,   -- the WorldMetaID
	board_size INTEGER,   -- size of the board
	board_depth INTEGER,   -- depth of the board
	owner_toolbox_id INTEGER REFERENCES toolbox(id) ON DELETE SET NULL ON UPDATE SET NULL,
	guest_toolbox_id INTEGER REFERENCES toolbox(id) ON DELETE SET NULL ON UPDATE SET NULL,
	lock_expiry_delay INTEGER,  -- number of seconds that locking grants control of the board
	lock_delete_delay INTEGER,  -- minimum number of seconds between locks
	contest_type varchar(255) REFERENCES particle(name),
	contest_var varchar(255),
	owner_toolset_xml TEXT,  -- Game headers for owner's turn
	guest_toolset_xml TEXT  -- Game headers for guest's turn
	);

CREATE TABLE world (
	id INTEGER PRIMARY KEY,   -- the WorldID
	name TEXT,  -- the name of the world
	meta_id INTEGER REFERENCES world_meta(id) ON DELETE SET NULL ON UPDATE SET NULL,
	owner_id INTEGER REFERENCES user(id) ON DELETE SET NULL ON UPDATE SET NULL,   -- the UserID of the world owner
	board_time INTEGER,   -- number of "microticks" on the board clock
	last_modified_time INTEGER,  -- time board was last modified (UNIX timestamp)
	last_stolen_time INTEGER,  -- time world's ownership was last changed (UNIX timestamp)
	board_xml TEXT  -- current state of the Board
	);

CREATE TABLE lock (
	lock_id INTEGER PRIMARY KEY AUTOINCREMENT,   -- the LockID
	world_id INTEGER REFERENCES world(id) ON DELETE CASCADE ON UPDATE CASCADE,   -- the WorldID
	owner_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,   -- the UserID of the lock owner
	create_time INTEGER,  -- lock creation time (UNIX timestamp)
	expiry_time INTEGER,  -- lock expiration time (UNIX timestamp)
	delete_time INTEGER,  -- lock deletion time (UNIX timestamp)
	toolset_xml   -- toolset XML used for lock
	);

CREATE TABLE lock_tool (
	lock_id INTEGER REFERENCES lock(lock_id) ON DELETE CASCADE ON UPDATE CASCADE,  -- the lock
	tool_id INTEGER REFERENCES tool(id) ON DELETE CASCADE ON UPDATE CASCADE,  -- the tool
	PRIMARY KEY (lock_id, tool_id)
);
