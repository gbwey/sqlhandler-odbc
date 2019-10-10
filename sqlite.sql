create table mixed
( id int not null primary key
 ,total float not null
 ,eventdate date not null
 ,description varchar(100) not null
);

insert into mixed values(1, 123.76, '2010-01-02', 'first row');
insert into mixed values(2, 212.04, '2005-03-04', 'second row');
insert into mixed values(3, 0, '1990-10-12', 'third row');
insert into mixed values(4, 10.34, '2007-01-02', 'fourth row');
insert into mixed values(5, 20.56, '2012-01-02', 'fifth row');
insert into mixed values(6, 777, '2005-01-02', 'sixth row');

create table cardinfo
( id int not null primary key
 ,name varchar(100) not null
 ,card varchar(100) not null
 ,hex varchar(100) not null
 ,datetime varchar(100) not null
);

insert into cardinfo values(1, 'abel tasman', '1234-5678-903', 'ff', '2001-07-04 12:13:14');
insert into cardinfo values(2, 'james cook',  '1111-2841-991', '128ab', 'June 21 2009 12:13:14');
insert into cardinfo values(3, 'van dieman',  '6433-1000-006', '278fec', '01/12/09 12:13:14');
insert into cardinfo values(4, 'peter smith', '1234-5678-903', '100', '2001-07-04 12:13:14');
insert into cardinfo values(5, 'john doe',    '1111-2841-991', '128ac', 'June 21 2009 12:13:14');
insert into cardinfo values(6, 'fred parks',  '6433-1000-006', '278fed', '01/12/09 12:13:14');
insert into cardinfo values(7, 'fred parks',  '6433-1000-006', '278fed', '01/12/09 12:13:14');
