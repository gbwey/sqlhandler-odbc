create table mixed
( id int not null primary key     
 ,total float not null
 ,eventdate date not null
 ,description varchar(100) not null 
);

insert into mixed values(1, 123.76, to_date('2010-01-02', 'YYYY-MM-DD'), 'first row');
insert into mixed values(2, 212.04, to_date('2005-03-04', 'YYYY-MM-DD'), 'second row');
insert into mixed values(3, 0, to_date('1990-10-12', 'YYYY-MM-DD'), 'third row');
insert into mixed values(4, 10.34, to_date('2007-01-02', 'YYYY-MM-DD'), 'fourth row');
insert into mixed values(5, 20.56, to_date('2012-01-02', 'YYYY-MM-DD'), 'fifth row');
insert into mixed values(6, 777, to_date('2005-01-02', 'YYYY-MM-DD'), 'sixth row');

