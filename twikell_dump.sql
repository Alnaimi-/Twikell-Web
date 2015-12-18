-- MySQL dump 10.13  Distrib 5.5.46, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: twikell
-- ------------------------------------------------------
-- Server version	5.5.46-0ubuntu0.14.04.2

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `article`
--

DROP TABLE IF EXISTS `article`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `article` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` varchar(1024) DEFAULT NULL,
  `bodyText` text,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=15 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `article`
--

LOCK TABLES `article` WRITE;
/*!40000 ALTER TABLE `article` DISABLE KEYS */;
INSERT INTO `article` VALUES (13,'xy','test'),(14,'Ben','Your marks will be less.');
/*!40000 ALTER TABLE `article` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tweet`
--

DROP TABLE IF EXISTS `tweet`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tweet` (
  `id` bigint(20) NOT NULL DEFAULT '0',
  `tweet` varchar(140) DEFAULT NULL,
  `reCount` int(11) DEFAULT NULL,
  `hashtags` varchar(60) DEFAULT NULL,
  `name` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `name_idx` (`name`),
  CONSTRAINT `name` FOREIGN KEY (`name`) REFERENCES `twitter_user` (`screen_name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tweet`
--

LOCK TABLES `tweet` WRITE;
/*!40000 ALTER TABLE `tweet` DISABLE KEYS */;
INSERT INTO `tweet` VALUES (659109693806223360,'More on collab w/ #Southpaw to create the soundtrack. If you missed it, grab it on DVD: https://t.co/I0UNAdRlt3 - https://t.co/q49WKewHnR',1547,'Southpaw','Eminem'),(659109923859636228,'And of course, our Soundtrack is available everywhere. Check it out on @AppleMusic here: https://t.co/BZE1mNvwLo',1526,'','Eminem'),(659360714075729920,'See how we made the #Phenomenal Music Film off the #Southpaw soundtrack: https://t.co/mFoRhRaE1I',2207,'Phenomenal,Southpaw','Eminem'),(660474702415052804,'Went in the lab real quick, now @djwhookid has it reloaded https://t.co/JKAHhq58Ie',2411,'','Eminem'),(661563054727925760,'Eminem X Jordan X Carhartt.\n\nDetails: https://t.co/2saAyEHF1P\n\n@Jumpman23 @Carhartt https://t.co/OXB1UuFaIB',4002,'','Eminem'),(662076798730014720,'“I’m ready for war, got machetes and swords…\" #ShadyWars https://t.co/T0sXCkBrtx',4603,'ShadyWars','Eminem'),(662383922517422080,'RT @ShadyRecords: #ShadyWars is now available on the App Store and Google Play. Download the game here: https://t.co/tNB7ehaCoX\nhttps://t.c…',1688,'ShadyWars','Eminem'),(663839888987549696,'Just knocked off Till Its Gone on #ShadyWars @yelawolf https://t.co/rkSje35dTO https://t.co/wzl6DCWl9W',1542,'ShadyWars','Eminem'),(663915796054319109,'https://t.co/i9j65bjkqf',1648,'','Eminem'),(664110010687885314,'RT @ShadyRecords: .@Eminem? catches up with @therealredman. https://t.co/qX4DwcMLIw',1026,'','Eminem'),(665230420716486656,'Support @MusicMSU\'s #LiteracyThroughSongwriting via the #MMF’s charity auction live on @ebay now #Detroit: https://t.co/0RTfMocCZu',1135,'LiteracyThroughSongwriting,MMF,Detroit','Eminem'),(665230622647066625,'This charity fundraiser is the exclusive official public offering for the Eminem X Jordan X Carhartt sneakers: https://t.co/0RTfMocCZu',1550,'','Eminem'),(667810396686778368,'Jingle Bells, Ginger-Boo smells, Ginger-Bae\'s selfie slays.\nCypress, Snow and Mr. &amp; Mrs. Claus\nAre… https://t.co/6C2hUmrkhf',4258,'','katyperry'),(668573240864972800,'#ShadyWars https://t.co/rkSje35dTO https://t.co/QGBR9oito9',3148,'ShadyWars','Eminem'),(668618423887245312,'keeping my rep up ???? https://t.co/SrEAIvg9H8',4314,'','katyperry'),(668701895049940992,'???????????? Friends! Watch me make ?magic? as your festive fairy for the @hm #HappyandMerry campaign! See the… https://t.co/WOM4t3Z4be',5558,'HappyandMerry','katyperry'),(668841366651293697,'RT @eBayNewsroom: Bid on #sneakers for a great cause. @Eminem x @Jumpman23 x @Carhartt, 100% to @msucms: https://t.co/vw0QEKh5Py https://t.…',842,'sneakers','Eminem'),(669634187591217152,'????????????Be in the season! Go behind the scenes of my #HappyandMerry campaign with @hm???????????? https://t.co/bjfOS4fMGz',4278,'HappyandMerry','katyperry'),(669720556493664256,'\"My diet starts tomorrow!\" -says no one today.',15007,'','katyperry'),(669727242742833153,'RT @ShadyRecords: Black on Black for Black Friday to return! Sign up for updates here: https://t.co/XEhVFSFYpG https://t.co/zqlGdKK1WF',759,'','Eminem'),(669776615002345473,'??Wine\n??Waist trainers\nBring. It. On. \n#TGiving \n#notanadd https://t.co/iPB3fT8dK8',3851,'TGiving,notanadd','katyperry'),(669955024118747137,'Throwback &amp; new limited edition designs will be available starting at midnight tonight at https://t.co/IbxoWqLi6n https://t.co/mnPk4E9BA',2477,'','Eminem'),(669963682214899712,'RT if your parents have one of these in their living room: https://t.co/Vfg7eYHrOR',4568,'','katyperry'),(669985714155589632,'I\'m thankful for good????\'s #94andkillingthegame https://t.co/BNNC3QSto3',4303,'94andkillingthegame','katyperry'),(670139547510943744,'When ur face is desperate for likes ???? https://t.co/2ytPSC9a0A',4659,'','katyperry'),(670245986028101632,'RT @ShadyRecords: Black on Black for Black Friday: Limited edition hoodie and t-shirts available for 24 hours! Shop now: https://t.co/9UjkS…',661,'','Eminem'),(670430820922884096,'RT @HillaryClinton: Today and every day, we #StandWithPP. https://t.co/oifqTQLx1X',4944,'StandWithPP','katyperry'),(671086917748989952,'#ShadyWars https://t.co/rkSje35dTO https://t.co/8Ljhwa2LYg',2938,'ShadyWars','Eminem'),(671720671840342016,'Kindness competition time! Spread the ?? this Christmas and tell me about your charitable good… https://t.co/PPO1qnmDhV',4890,'','katyperry'),(673021012359819264,'Allison Williams taught me about love tonight: https://t.co/NPYYjhA6Y1',3803,'','katyperry'),(673912431844945921,'Get your team\'s official Hands High gear today! https://t.co/OGyRoKa0PP https://t.co/QTC8Ui1jYA #HandsHigh https://t.co/jQx75vk7Mf',186,'HandsHigh','jimmyfallon'),(674226469053272065,'Playing Pictionary with Claire Danes, @RealRonHoward and Higgins https://t.co/CpNWIpRlSx #FallonTonight',158,'FallonTonight','jimmyfallon'),(674229503049269253,'Thanks @AlecBaldwin. This was fun. Good morning. #HeresTheThing https://t.co/78kf8tbIJ3',182,'HeresTheThing','jimmyfallon'),(674241457717424128,'Tonight: Presidential candidate Senator @BernieSanders, @JohnCena, and @troyesivan makes his TV debut! #TroyeOnFallon #FallonTonight',1755,'TroyeOnFallon,FallonTonight','jimmyfallon'),(674308998846398464,'Check the link in bio for a real weather update! Let’s encourage our world leaders to take… https://t.co/BVKfwZjGav',4005,'','katyperry'),(674607127311327232,'Tonight: Amy Poehler is here! Plus talk + stand-up from @kevin_nealon and #12DaysOfChristmasSweaters continues! #FallonTonight',257,'12DaysOfChristmasSweaters,FallonTonight','jimmyfallon'),(674645217409114112,'I love Christmas music! Here\'s a new collection from @PattySmyth. https://t.co/q0Z2dhnTRn #anynewChristmasjamsoutthereyoulike',191,'anynewChristmasjamsoutthereyoulike','jimmyfallon'),(674665474811166720,'On my 10th day of my holiday popcorn tin diet. #feelinggood https://t.co/O2z0nPTOC2',280,'feelinggood','jimmyfallon'),(674707080092123137,'Hashtag game! Update the lyrics of a famous Christmas song to modern times, and tag with #UpdatedXmasCarols. Could be on our show!',483,'UpdatedXmasCarols','jimmyfallon'),(674707169426575361,'\"Grandma Got Run Over By An Uber\" #UpdatedXmasCarols',2658,'UpdatedXmasCarols','jimmyfallon'),(674777157965504512,'Your 30\'s are your best years! RT if you agree, even if you\'re in your 20\'s ????',8723,'','katyperry'),(674819805002641408,'Thank you, kind human! Keep the stories coming! I\'ll announce a winner tomorrow! #HMdresstogive #HappyandMerry https://t.co/AjO4IYWUEb',3647,'HMdresstogive,HappyandMerry','katyperry'),(674953918477111297,'Amy Poehler and I play \"Truth or Truth\" https://t.co/0wsaGlBpE2 #FallonTonight',347,'FallonTonight','jimmyfallon'),(674968515892367360,'Fun show tonight: @chrishemsworth, @JimGaffigan, music from @jamielawsonuk, and your #UpdatedXmasCarols tweets! #FallonTonight',240,'UpdatedXmasCarols,FallonTonight','jimmyfallon'),(675045096157544449,'Streaming the monologue rehearsal on Facebook Live today. Go to our show page at https://t.co/cSb3S1zs3m and I\'ll be live there soon...',204,'','jimmyfallon'),(675060704223862786,'The first! Salute. #ShadyWars https://t.co/rkSje35dTO https://t.co/VAi5kMsbQF',1136,'ShadyWars','Eminem'),(675185755476611074,'Congrats on winning the fairy dress @Miss_Lilyjames! Thank you for doing your part to spread the #HappyandMerry! https://t.co/NMYq8fm345',2695,'HappyandMerry','katyperry'),(675185838955868161,'I hope everyone continues to be so giving this season and all year!',6923,'','katyperry'),(675316313288478720,'.@chrishemsworth and I have a Sleigh Scooter race https://t.co/JcFEBBZiEi #FallonTonight',271,'FallonTonight','jimmyfallon'),(675330451830083584,'Happy #ThankYouNoteFriday! Tonight: Catchphrase with Will Smith, @kirstendunst is here, and @CalvinHarris performs! #FallonTonight',323,'ThankYouNoteFriday,FallonTonight','jimmyfallon'),(676426286689046528,'Fun show tonight! Tina Fey, @danedehaan, music from @_KennyRogers and #12DaysOfChristmasSweaters continues! #FallonTonight',295,'12DaysOfChristmasSweaters,FallonTonight','jimmyfallon'),(676556682835591169,'RT @AppStoreGames: This Friday at 2 pm PST, make sure you’re wide awake for our #AppStoreChat with @katyperry. Reply now with your Qs! http…',2744,'AppStoreChat','katyperry'),(676764532602224640,'Tina Fey and I play a new game called \"First Impressions\" https://t.co/sVDqGKHl0X #FallonTonight',330,'FallonTonight','jimmyfallon'),(676778600364974080,'Tonight: @mark_wahlberg, @billburr, music from @SherylCrow, + something fun for #StarWars fans! Don\'t miss!! #FallonTonight #TheForceAwakens',282,'StarWars,FallonTonight,TheForceAwakens','jimmyfallon'),(676807350968274945,'ppplay this pppodcast ppproudly https://t.co/vL86xPKsYj',230,'','jimmyfallon'),(676828138064752640,'Sneak peek of the Tonight Show tonight! #StarWars #FallonTonight https://t.co/nQyFsFAgfm',269,'StarWars,FallonTonight','jimmyfallon'),(676856884553363456,'FINALLY my colorful ???? musical ???? game, #KatyPerryPop, is here! Check the @AppStore &amp; @GooglePlay as it starts rolling out globally ',2612,'KatyPerryPop','katyperry'),(676857071707418624,'…and being as it’s the coziest time of year, curl up with #KatyPerryPop to play your way up the charts &amp; take stages all over the world!',2327,'KatyPerryPop','katyperry'),(676934256342515712,'Set your TiVos and DVRs this one is fun. #FallonTonight #StarWars https://t.co/9m6roQ1qPV',290,'FallonTonight,StarWars','jimmyfallon');
/*!40000 ALTER TABLE `tweet` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `twitter_user`
--

DROP TABLE IF EXISTS `twitter_user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `twitter_user` (
  `screen_name` varchar(20) NOT NULL DEFAULT '',
  `name` varchar(30) DEFAULT NULL,
  `image` varchar(200) DEFAULT NULL,
  `location` varchar(20) DEFAULT NULL,
  `followers` int(11) DEFAULT NULL,
  PRIMARY KEY (`screen_name`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `twitter_user`
--

LOCK TABLES `twitter_user` WRITE;
/*!40000 ALTER TABLE `twitter_user` DISABLE KEYS */;
INSERT INTO `twitter_user` VALUES ('Eminem','Ben Steer','updated.com','London',1),('jimmyfallon','jimmy fallon','http://pbs.twimg.com/profile_images/1194467116/new-resize-square_normal.jpg','New York, New York',32664288),('kanyewest','KANYE WEST','http://pbs.twimg.com/profile_images/585565077207678977/N_eNSBXi_normal.jpg','',16224464),('katyperry','Ben Steer','updated.com','London',1),('TheEllenShow','Ellen DeGeneres','http://pbs.twimg.com/profile_images/668873018035142656/Tox11UsS_normal.png','California',51058461);
/*!40000 ALTER TABLE `twitter_user` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `login` varchar(256) NOT NULL,
  `password` varchar(256) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `login` (`login`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
INSERT INTO `user` VALUES (1,'hamza','e99a18c428cb38d5f260853678922e03');
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2015-12-18  2:06:13
