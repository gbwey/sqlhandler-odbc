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

