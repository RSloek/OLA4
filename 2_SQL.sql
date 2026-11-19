create database Bilbasen;
use Bilbasen;

-- Create bilbasen's tables --

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

-- Send bbscrape fra r --
-- Insert bbscrape data i tabler --

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

