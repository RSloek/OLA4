use Bilbasen;

-- pris table --
create table pris as
select Bil_id, pris from bbscrape;

ALTER TABLE pris
  ADD pris_id INT,
  ADD scrapedate DATE;

alter table pris modify pris_id int first;
alter table pris modify bil_id varchar(7) after pris_id;
alter table pris modify pris int after bil_id;
alter table pris modify scrapedate date after pris;

update pris set scrapedate = '2026-11-11' where pris_id > 0;

select * from pris;

-- bil --
select * from bbscrape;

create table bil as
select Bil_id, forhandler_id, bilmodel_kort as model, registrerings_år, kørte_km, drivmiddel, rækkevidde, link from bbscrape;

ALTER TABLE bil
  ADD sold boolean default false;

select * from bil;


-- forhandler --

create table forhandler as
select distinct forhandler_id, lokation from bbscrape where forhandler_id is not null;

select * from forhandler;
drop table forhandler;
select * from bbscrape;

































drop table pris;
drop table bil;
drop table forhandler;


CREATE TABLE forhandler (
  forhandler_id  VARCHAR(36) PRIMARY KEY,
  lokation       VARCHAR(37)
);

CREATE TABLE bil (
  bil_id              VARCHAR(7) PRIMARY KEY,
  forhandler_id       VARCHAR(36),
  model               VARCHAR(17),
  registrerings_år  INT,
  kørte_km         INT,
  drivmiddel          VARCHAR(6),
  rækkevidde        INT,
  link                VARCHAR(105),
  sold                BOOLEAN DEFAULT FALSE,
  FOREIGN KEY (forhandler_id) REFERENCES forhandler(forhandler_id)
);

CREATE TABLE pris (
  pris_id     INT AUTO_INCREMENT PRIMARY KEY,
  bil_id      VARCHAR(7) NOT NULL,
  pris        INT,
  scrapedate  DATE,
  FOREIGN KEY (bil_id) REFERENCES bil(Bil_id)
);


INSERT INTO forhandler (forhandler_id, lokation)
select distinct forhandler_id, lokation from bbscrape where forhandler_id is not null;


INSERT INTO bil (bil_id, forhandler_id, model, registrerings_år,
                 kørte_km, rækkevidde, drivmiddel, link)
SELECT Bil_id, forhandler_id, bilmodel_kort as model, registrerings_år,
       kørte_km, rækkevidde, drivmiddel, link
FROM bbscrape;


INSERT INTO pris (bil_id, pris)
SELECT bil_id, pris
FROM bbscrape;
update pris set scrapedate = '2026-11-11' where pris_id > 0;


















































































