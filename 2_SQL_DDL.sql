create database bilbasen;
use bilbasen;

CREATE TABLE `forhandler` (
  `forhandler_id` varchar(36) NOT NULL,
  `lokation` varchar(37) DEFAULT NULL,
  PRIMARY KEY (`forhandler_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE `bil` (
  `bil_id` varchar(7) NOT NULL,
  `forhandler_id` varchar(36) DEFAULT NULL,
  `model` varchar(17) DEFAULT NULL,
  `registrerings_år` int DEFAULT NULL,
  `kørte_km` int DEFAULT NULL,
  `drivmiddel` varchar(6) DEFAULT NULL,
  `rækkevidde` int DEFAULT NULL,
  `link` varchar(105) DEFAULT NULL,
  `sold` tinyint(1) DEFAULT '0',
  PRIMARY KEY (`bil_id`),
  KEY `forhandler_id` (`forhandler_id`),
  CONSTRAINT `bil_ibfk_1` FOREIGN KEY (`forhandler_id`) REFERENCES `forhandler` (`forhandler_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE `pris` (
  `pris_id` int NOT NULL AUTO_INCREMENT,
  `bil_id` varchar(7) NOT NULL,
  `pris` int DEFAULT NULL,
  `scrapedate` date DEFAULT NULL,
  PRIMARY KEY (`pris_id`),
  KEY `bil_id` (`bil_id`),
  CONSTRAINT `pris_ibfk_1` FOREIGN KEY (`bil_id`) REFERENCES `bil` (`bil_id`)
) ENGINE=InnoDB AUTO_INCREMENT=4106 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;


