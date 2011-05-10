
PRAGMA foreign_keys = ON;

CREATE TABLE image (
	name varchar(255) PRIMARY KEY,  -- the name of this image
	data TEXT  -- SVG image XML
	);

CREATE TABLE particle (
	name varchar(255) PRIMARY KEY,  -- the name of this Particle
	image_name varchar(255) REFERENCES image(name) ON DELETE CASCADE ON UPDATE CASCADE,
	data TEXT  -- Particle XML
	);

CREATE TABLE tool (
	name varchar(255) PRIMARY KEY,  -- the name of this Tool
	data TEXT  -- Tool XML
	);

CREATE TABLE particle_dep (  -- Particle dependencies
	particle_name varchar(255) REFERENCES particle(name) ON DELETE CASCADE ON UPDATE CASCADE,  -- the dependent type
	dep_name varchar(255) REFERENCES particle(name) ON DELETE CASCADE ON UPDATE CASCADE,  -- the depended-upon type
	PRIMARY KEY (particle_name, dep_name)
);

CREATE TABLE user (
	id INTEGER PRIMARY KEY,   -- the UserID
	name VARCHAR(15),   -- same max length as a Twitter handle
	cash DECIMAL,
	is_artist BOOLEAN,  -- user can upload new images
	is_vandal BOOLEAN,  -- user can replace existing images
	is_smith BOOLEAN,  -- user can upload new tools
	is_maker BOOLEAN,  -- user can replace existing tools
	is_coder BOOLEAN,  -- user can upload new types
	is_hacker BOOLEAN  -- user can replace existing types
	);

CREATE TABLE inventory (
	user_id INTEGER REFERENCES user(id) ON DELETE CASCADE ON UPDATE CASCADE,   -- the UserID
	particle_name TEXT REFERENCES particle(name) ON DELETE CASCADE ON UPDATE CASCADE,  -- the type
	amount INTEGER
	);
