#used to test queries looking at ad source type and clicks v loads

CREATE TABLE user_action (
  id int auto_increment PRIMARY KEY,
  user_id int references item(idi),
  item_id int,
  action text,
  date_of timestamp
  );

insert into user_action(id, user_id, item_id, action, date_of) values (1, 100, 200, 'click', '2015-01-01');
insert into user_action(id, user_id, item_id, action, date_of) values (2, 100, 201, 'load', '2015-02-03');
insert into user_action(id, user_id, item_id, action, date_of) values (3, 100, 200, 'load', '2015-02-05');
insert into user_action(id, user_id, item_id, action, date_of) values (4, 100, 201, 'click', '2015-02-05');
insert into user_action(id, user_id, item_id, action, date_of) values (5, 101, 200, 'click', '2015-02-05');
insert into user_action(id, user_id, item_id, action, date_of) values (6, 101, 203, 'load', '2015-02-05');
insert into user_action(id, user_id, item_id, action, date_of) values (7, 101, 202, 'load', '2015-02-05');
insert into user_action(id, user_id, item_id, action, date_of) values (8, 101, 203, 'click', '2015-02-05');
insert into user_action(id, user_id, item_id, action, date_of) values (9, 101, 202, 'click', '2015-02-05');
insert into user_action(id, user_id, item_id, action, date_of) values (10, 101, 200, 'click', '2015-02-05');

CREATE TABLE item (
  id int PRIMARY KEY, 
  body text,
  source_id numeric references source(id)
  );

insert into item(id, body, source_id) values (200, 'some text for item 200', 300);
insert into item(id, body, source_id) values (201, 'some text for item 201', 300);
insert into item(id, body, source_id) values (202, 'some text for item 202', 301);
insert into item(id, body, source_id) values (203, 'some text for item 203', 302);

CREATE TABLE source (
  id int PRIMARY KEY,
  source_type text
  );

insert into source(id, source_type) values (300, 'advertorial');
insert into source(id, source_type) values (301, 'organic');
insert into source(id, source_type) values (302, 'something');