
CREATE TABLE `bil` (
  `Bil_id` varchar(7) NOT NULL,
  `forhandler_id` varchar(36) DEFAULT NULL,
  `model` varchar(17) DEFAULT NULL,
  `registrerings_år` double DEFAULT NULL,
  `kørte_km` double DEFAULT NULL,
  `drivmiddel` varchar(6) DEFAULT NULL,
  `rækkevidde` double DEFAULT NULL,
  `link` varchar(105) DEFAULT NULL,
  `sold` tinyint(1) DEFAULT '0',
  PRIMARY KEY (`Bil_id`),
  KEY `forhandler_id_idx` (`forhandler_id`),
  CONSTRAINT `forhandler_id` FOREIGN KEY (`forhandler_id`) REFERENCES `forhandler` (`forhandler_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE `forhandler` (
  `forhandler_id` varchar(36) NOT NULL,
  `lokation` varchar(37) DEFAULT NULL,
  PRIMARY KEY (`forhandler_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE `pris` (
  `pris_id` int NOT NULL AUTO_INCREMENT,
  `bil_id` varchar(7) NOT NULL,
  `pris` int DEFAULT NULL,
  `scrapedate` date DEFAULT NULL,
  PRIMARY KEY (`pris_id`),
  KEY `bil_id_idx` (`bil_id`),
  CONSTRAINT `bil_id` FOREIGN KEY (`bil_id`) REFERENCES `bil` (`Bil_id`)
) ENGINE=InnoDB AUTO_INCREMENT=2654 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;