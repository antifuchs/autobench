create database sbclarch;

drop table result;
drop table benchmark;
drop table version;
drop table impl;
drop table machine;


create table benchmark (
       name varchar(90),
       primary key (name)
);

create table machine (
       name varchar(90),
       type varchar(10),
       primary key (name)
);

create table impl (
       name varchar(90),
       field_offset integer,
       primary key (name)
);

create table version (
       i_name varchar(90),
       version varchar(90),
       release_date timestamp,
       primary key (i_name, version),
       foreign key (i_name) references impl (name)
);

create table result (
       date timestamp,
       v_name varchar(90),
       v_version varchar(90),
       b_name varchar(90),
       m_name varchar(90),

       seconds double precision,
       
       primary key (date, v_name, v_version, b_name, m_name),
       constraint result_x_machine foreign key (m_name) references machine (name),
       constraint result_x_benchmark foreign key (b_name) references benchmark (name),
       constraint result_x_impl foreign key (v_name,v_version) references version (i_name,version)
);

insert into machine values ('walrus.boinkor.net', 'x86');

insert into impl values ('SBCL', 0);
insert into impl values ('CMU Common Lisp', 2);


begin transaction;
insert into version values ('CMU Common Lisp', '18c', '2000-12-01 16:48:53');
insert into version values ('CMU Common Lisp', '18d', '2002-04-10 14:57:15');
insert into version values ('CMU Common Lisp', '18e', '2003-04-03 20:27:32');
insert into version values ('CMU Common Lisp', 'pre-18f', '2003-11-01 12:47:23');
insert into version values ('SBCL', '0.7.0', '2002-01-20 04:55');
insert into version values ('SBCL', '0.7.1', '2002-01-27 04:15');
insert into version values ('SBCL', '0.7.2', '2002-03-24 01:00');
insert into version values ('SBCL', '0.7.3', '2002-04-25 02:00');
insert into version values ('SBCL', '0.7.4', '2002-05-24 02:00');
insert into version values ('SBCL', '0.8.0', '2003-05-25 02:00');
insert into version values ('SBCL', '0.8.1', '2003-06-24 02:00');
insert into version values ('SBCL', '0.8.2', '2003-07-26 02:00');
insert into version values ('SBCL', '0.8.3', '2003-08-25 02:00');
insert into version values ('SBCL', '0.8.4', '2003-10-03 02:00');
insert into version values ('SBCL', '0.8.5', '2003-10-25 02:00');
insert into version values ('SBCL', '0.8.6', '2003-11-25 01:00');
insert into version values ('SBCL', '0.8.7', '2003-12-29 01:00');
insert into version values ('SBCL', '0.8.8', '2004/02/24 22:52:25');
insert into version values ('SBCL', '0.8.8.1', '2004/02/25 17:41:42');
insert into version values ('SBCL', '0.8.8.3', '2004/02/26 08:36:22');
insert into version values ('SBCL', '0.8.8.4', '2004/02/26 12:15:01');
insert into version values ('SBCL', '0.8.8.5', '2004/02/27 09:41:37');
insert into version values ('SBCL', '0.8.8.6', '2004/03/01 15:08:21');
insert into version values ('SBCL', '0.8.8.7', '2004/03/01 16:21:14');
insert into version values ('SBCL', '0.8.8.8', '2004/03/01 20:23:30');
insert into version values ('SBCL', '0.8.8.9', '2004/03/01 20:30:23');
insert into version values ('SBCL', '0.8.8.10', '2004/03/01 21:32:42');
insert into version values ('SBCL', '0.8.8.11', '2004/03/01 23:22:25');
insert into version values ('SBCL', '0.8.8.12', '2004/03/02 09:37:48');
insert into version values ('SBCL', '0.8.8.13', '2004/03/02 16:23:22');
insert into version values ('SBCL', '0.8.8.14', '2004/03/04 11:12:45');
insert into version values ('SBCL', '0.8.8.15', '2004/03/05 13:02:20');
insert into version values ('SBCL', '0.8.8.16', '2004/03/06 03:02:19');
insert into version values ('SBCL', '0.8.8.17', '2004/03/06 19:54:51');
insert into version values ('SBCL', '0.8.8.18', '2004/03/07 07:50:51');
commit;

begin transaction;
insert into benchmark values ('1D-ARRAYS');
insert into benchmark values ('2D-ARRAYS');
insert into benchmark values ('3D-ARRAYS');
insert into benchmark values ('ACKERMANN');
insert into benchmark values ('BENCH-STRINGS');
insert into benchmark values ('BIGNUM/ELEM-100-1000');
insert into benchmark values ('BIGNUM/ELEM-1000-100');
insert into benchmark values ('BIGNUM/ELEM-10000-1');
insert into benchmark values ('BIGNUM/PARI-100-10');
insert into benchmark values ('BIGNUM/PARI-200-5');
insert into benchmark values ('BITVECTORS');
insert into benchmark values ('BOEHM-GC');
insert into benchmark values ('BOYER');
insert into benchmark values ('BROWSE');
insert into benchmark values ('CLOS/complex-methods');
insert into benchmark values ('CLOS/defclass');
insert into benchmark values ('CLOS/defmethod');
insert into benchmark values ('CLOS/instantiate');
insert into benchmark values ('CLOS/method+after');
insert into benchmark values ('CLOS/methodcalls');
insert into benchmark values ('CLOS/simple-instantiate');
insert into benchmark values ('COMPILER');
insert into benchmark values ('CRC40');
insert into benchmark values ('CTAK');
insert into benchmark values ('DDERIV');
insert into benchmark values ('DEFLATE-FILE');
insert into benchmark values ('DERIV');
insert into benchmark values ('DESTRUCTIVE');
insert into benchmark values ('DIV2-TEST-1');
insert into benchmark values ('DIV2-TEST-2');
insert into benchmark values ('EQL-SPECIALIZED-FIB');
insert into benchmark values ('FACTORIAL');
insert into benchmark values ('FFT');
insert into benchmark values ('FIB');
insert into benchmark values ('FIB-RATIO');
insert into benchmark values ('FPRINT/PRETTY');
insert into benchmark values ('FPRINT/UGLY');
insert into benchmark values ('FRPOLY/BIGNUM');
insert into benchmark values ('FRPOLY/FIXNUM');
insert into benchmark values ('FRPOLY/FLOAT');
insert into benchmark values ('HASH-INTEGERS');
insert into benchmark values ('HASH-STRINGS');
insert into benchmark values ('LOAD-FASL');
insert into benchmark values ('MANDELBROT/COMPLEX');
insert into benchmark values ('MANDELBROT/DFLOAT');
insert into benchmark values ('MRG32K3A');
insert into benchmark values ('PI-ATAN');
insert into benchmark values ('PI-DECIMAL/BIG');
insert into benchmark values ('PI-DECIMAL/SMALL');
insert into benchmark values ('PI-RATIOS');
insert into benchmark values ('PUZZLE');
insert into benchmark values ('RICHARDS');
insert into benchmark values ('SEARCH-SEQUENCE');
insert into benchmark values ('SLURP-LINES');
insert into benchmark values ('STAK');
insert into benchmark values ('STRING-CONCAT');
insert into benchmark values ('SUM-PERMUTATIONS');
insert into benchmark values ('TAK');
insert into benchmark values ('TAKL');
insert into benchmark values ('TRAVERSE');
insert into benchmark values ('TRIANGLE');
insert into benchmark values ('TRTAK');
insert into benchmark values ('WALK-LIST/MESS');
insert into benchmark values ('WALK-LIST/SEQ');
insert into benchmark values ('fill-strings/adjustable');
insert into benchmark values ('MAKE-SEQUENTIAL-LIST/PUSH-NREVERSE');
insert into benchmark values ('MAKE-SEQUENTIAL-LIST/PUSH-RPLACD');
commit;


-- select v_name, v_version, avg(seconds) as mean, stddev(seconds) / sqrt(count(seconds)) as stderr, count(seconds), release_date, field_offset from result join version on (v_name=i_name and v_version = version) join impl on i_name = name where b_name='CTAK' group by v_name, v_version, release_date, field_offset order by release_date;

-- arch-tag: "325cce44-ff31-11d8-8b1b-000c76244c24"
