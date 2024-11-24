data <- read.csv("survey_686732_R_data_file.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


# LimeSurvey Field type: F
data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "id"
names(data)[1] <- "id"
# LimeSurvey Field type: DATETIME23.2
data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "submitdate"
names(data)[2] <- "submitdate"
# LimeSurvey Field type: F
data[, 3] <- as.numeric(data[, 3])
attributes(data)$variable.labels[3] <- "lastpage"
names(data)[3] <- "lastpage"
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "startlanguage"
names(data)[4] <- "startlanguage"
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "seed"
names(data)[5] <- "seed"
# LimeSurvey Field type: A
data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "Pentru început, te rugăm să îți oferi consimțământul pentru participarea la acest studiu prin completarea și încărcarea documentului ”Consimțământ informat” (pe care îl poți descărca de aici).  Utilizând butonul de mai jos, te rugăm să încarci consimțământul semnat de tine, sub formă de poză sau document. Poți încărca png, gif, doc, odt, jpg, jpeg, pdf, png, heic sub 10240 KB."
names(data)[6] <- "G01Q49"
# LimeSurvey Field type: A
data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "filecount - Pentru început, te rugăm să îți oferi consimțământul pentru participarea la acest studiu prin completarea și încărcarea documentului ”Consimțământ informat” (pe care îl poți descărca de aici).  Utilizând butonul de mai jos, te rugăm să încarci consimțământul semnat de tine, sub formă de poză sau document. Poți încărca png, gif, doc, odt, jpg, jpeg, pdf, png, heic sub 10240 KB."
names(data)[7] <- "G01Q49__filecount"
# LimeSurvey Field type: A
data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- "În cadrul aceste secțiuni este necesar să faci dovada că un părinte/tutore legal își oferă consimțământul pentru participarea ta la această cercetare.   Utilizând butonul de mai jos, te rugăm să încarci consimțământul semnat de un părinte/tutore legal (documentul poate fi descărcat de aici), sub formă de poză sau document. Puteți încărca png, gif, doc, odt, jpg, jpeg, pdf, png, heic sub 10240 KB."
names(data)[8] <- "G01Q50"
# LimeSurvey Field type: A
data[, 9] <- as.character(data[, 9])
attributes(data)$variable.labels[9] <- "filecount - În cadrul aceste secțiuni este necesar să faci dovada că un părinte/tutore legal își oferă consimțământul pentru participarea ta la această cercetare.   Utilizând butonul de mai jos, te rugăm să încarci consimțământul semnat de un părinte/tutore legal (documentul poate fi descărcat de aici), sub formă de poză sau document. Puteți încărca png, gif, doc, odt, jpg, jpeg, pdf, png, heic sub 10240 KB."
names(data)[9] <- "G01Q50__filecount"
# LimeSurvey Field type: A
data[, 10] <- as.character(data[, 10])
attributes(data)$variable.labels[10] <- "[Număr Matricol]"
names(data)[10] <- "G01Q62_SQ001"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "[Nume]"
names(data)[11] <- "G01Q63_SQ001"
# LimeSurvey Field type: A
data[, 12] <- as.character(data[, 12])
attributes(data)$variable.labels[12] <- "[Prenume]"
names(data)[12] <- "G01Q63_SQ002"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- "Cod Liceu"
names(data)[13] <- "L"
# LimeSurvey Field type: A
data[, 14] <- as.character(data[, 14])
attributes(data)$variable.labels[14] <- "Sexul atribuit la naștere:"
data[, 14] <- factor(data[, 14], levels=c("AO01","AO02"),labels=c("Masculin", "Feminin"))
names(data)[14] <- "Q00"
# LimeSurvey Field type: A
data[, 15] <- as.character(data[, 15])
attributes(data)$variable.labels[15] <- "Anul nașterii:"
data[, 15] <- factor(data[, 15], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13"),labels=c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012"))
names(data)[15] <- "G01Q23"
# LimeSurvey Field type: A
data[, 16] <- as.character(data[, 16])
attributes(data)$variable.labels[16] <- "Identitatea de gen:"
data[, 16] <- factor(data[, 16], levels=c("AO01","AO02"),labels=c("Masculin", "Feminin"))
names(data)[16] <- "G01Q22"
# LimeSurvey Field type: A
data[, 17] <- as.character(data[, 17])
attributes(data)$variable.labels[17] <- "[Altele] Identitatea de gen:"
names(data)[17] <- "G01Q22_other"
# LimeSurvey Field type: A
data[, 18] <- as.character(data[, 18])
attributes(data)$variable.labels[18] <- "Anul de studii:"
data[, 18] <- factor(data[, 18], levels=c("AO01","AO02","AO03","AO04"),labels=c("9", "10", "11", "12"))
names(data)[18] <- "G01Q24"
# LimeSurvey Field type: A
data[, 19] <- as.character(data[, 19])
attributes(data)$variable.labels[19] <- "Ți-a trecut prin minte vreodată să renunți la școală sau să îți întrerupi cel puțin temporar studiile?"
data[, 19] <- factor(data[, 19], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[19] <- "G01Q34"
# LimeSurvey Field type: A
data[, 20] <- as.character(data[, 20])
attributes(data)$variable.labels[20] <- "Etnia:"
data[, 20] <- factor(data[, 20], levels=c("AO02","AO03","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO04","AO12"),labels=c("Român", "Maghiar", "Sas/ German", "Rrom", "Rus-Lipovean", "Turc", "Tătar", "Sârb", "Slovac", "Ucrainean", "Prefer să nu răspund"))
names(data)[20] <- "G01Q25"
# LimeSurvey Field type: A
data[, 21] <- as.character(data[, 21])
attributes(data)$variable.labels[21] <- "[Altele] Etnia:"
names(data)[21] <- "G01Q25_other"
# LimeSurvey Field type: A
data[, 22] <- as.character(data[, 22])
attributes(data)$variable.labels[22] <- " Locuiești în:"
data[, 22] <- factor(data[, 22], levels=c("AO01","AO02"),labels=c("Mediul urban", "Mediul rural"))
names(data)[22] <- "G01Q26"
# LimeSurvey Field type: A
data[, 23] <- as.character(data[, 23])
attributes(data)$variable.labels[23] <- "Modelul de smartphone pe care îl deții:"
data[, 23] <- factor(data[, 23], levels=c("AO01","AO02","AO03"),labels=c("iPhone", "Android", "Nu dețin un smartphone"))
names(data)[23] <- "G01Q28"
# LimeSurvey Field type: A
data[, 24] <- as.character(data[, 24])
attributes(data)$variable.labels[24] <- "[Altele] Modelul de smartphone pe care îl deții:"
names(data)[24] <- "G01Q28_other"
# LimeSurvey Field type: A
data[, 25] <- as.character(data[, 25])
attributes(data)$variable.labels[25] <- "[O cameră a mea] În gospodăria mea/la mine acasă există:"
data[, 25] <- factor(data[, 25], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[25] <- "G01Q59_SQ002"
# LimeSurvey Field type: A
data[, 26] <- as.character(data[, 26])
attributes(data)$variable.labels[26] <- "[Un birou la care pot studia] În gospodăria mea/la mine acasă există:"
data[, 26] <- factor(data[, 26], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[26] <- "G01Q59_SQ003"
# LimeSurvey Field type: A
data[, 27] <- as.character(data[, 27])
attributes(data)$variable.labels[27] <- "[Un loc liniștit în care pot studia] În gospodăria mea/la mine acasă există:"
data[, 27] <- factor(data[, 27], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[27] <- "G01Q59_SQ004"
# LimeSurvey Field type: A
data[, 28] <- as.character(data[, 28])
attributes(data)$variable.labels[28] <- "[Un calculator pe care îl pot folosi (inclusiv pentru școală)] În gospodăria mea/la mine acasă există:"
data[, 28] <- factor(data[, 28], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[28] <- "G01Q59_SQ005"
# LimeSurvey Field type: A
data[, 29] <- as.character(data[, 29])
attributes(data)$variable.labels[29] <- "[Conexiune la internet] În gospodăria mea/la mine acasă există:"
data[, 29] <- factor(data[, 29], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[29] <- "G01Q59_SQ006"
# LimeSurvey Field type: A
data[, 30] <- as.character(data[, 30])
attributes(data)$variable.labels[30] <- "[Software educațional] În gospodăria mea/la mine acasă există:"
data[, 30] <- factor(data[, 30], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[30] <- "G01Q59_SQ007"
# LimeSurvey Field type: A
data[, 31] <- as.character(data[, 31])
attributes(data)$variable.labels[31] <- "[Conexiune la internet] În gospodăria mea/la mine acasă există:"
data[, 31] <- factor(data[, 31], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[31] <- "G01Q59_SQ008"
# LimeSurvey Field type: A
data[, 32] <- as.character(data[, 32])
attributes(data)$variable.labels[32] <- "[Cărți de literatură clasică] În gospodăria mea/la mine acasă există:"
data[, 32] <- factor(data[, 32], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[32] <- "G01Q59_SQ009"
# LimeSurvey Field type: A
data[, 33] <- as.character(data[, 33])
attributes(data)$variable.labels[33] <- "[Cărți de poezie] În gospodăria mea/la mine acasă există:"
data[, 33] <- factor(data[, 33], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[33] <- "G01Q59_SQ010"
# LimeSurvey Field type: A
data[, 34] <- as.character(data[, 34])
attributes(data)$variable.labels[34] <- "[Opere de artă] În gospodăria mea/la mine acasă există:"
data[, 34] <- factor(data[, 34], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[34] <- "G01Q59_SQ011"
# LimeSurvey Field type: A
data[, 35] <- as.character(data[, 35])
attributes(data)$variable.labels[35] <- "[Cărți pe care le pot folosi pentru școală] În gospodăria mea/la mine acasă există:"
data[, 35] <- factor(data[, 35], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[35] <- "G01Q59_SQ012"
# LimeSurvey Field type: A
data[, 36] <- as.character(data[, 36])
attributes(data)$variable.labels[36] <- "[Cărți inginerești/ tehnice] În gospodăria mea/la mine acasă există:"
data[, 36] <- factor(data[, 36], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[36] <- "G01Q59_SQ013"
# LimeSurvey Field type: A
data[, 37] <- as.character(data[, 37])
attributes(data)$variable.labels[37] <- "[Dicționar] În gospodăria mea/la mine acasă există:"
data[, 37] <- factor(data[, 37], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[37] <- "G01Q59_SQ014"
# LimeSurvey Field type: A
data[, 38] <- as.character(data[, 38])
attributes(data)$variable.labels[38] <- "[Cărți despre artă, muzică sau design] În gospodăria mea/la mine acasă există:"
data[, 38] <- factor(data[, 38], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[38] <- "G01Q59_SQ015"
# LimeSurvey Field type: A
data[, 39] <- as.character(data[, 39])
attributes(data)$variable.labels[39] <- "[Televizor] În gospodăria mea/la mine acasă există:"
data[, 39] <- factor(data[, 39], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("AO02", "1", "2", "3", "Mai multe"))
names(data)[39] <- "G01Q60_SQ002"
# LimeSurvey Field type: A
data[, 40] <- as.character(data[, 40])
attributes(data)$variable.labels[40] <- "[Mașină ] În gospodăria mea/la mine acasă există:"
data[, 40] <- factor(data[, 40], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("AO02", "1", "2", "3", "Mai multe"))
names(data)[40] <- "G01Q60_SQ003"
# LimeSurvey Field type: A
data[, 41] <- as.character(data[, 41])
attributes(data)$variable.labels[41] <- "[Cadă sau duș] În gospodăria mea/la mine acasă există:"
data[, 41] <- factor(data[, 41], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("AO02", "1", "2", "3", "Mai multe"))
names(data)[41] <- "G01Q60_SQ004"
# LimeSurvey Field type: A
data[, 42] <- as.character(data[, 42])
attributes(data)$variable.labels[42] <- "[Smartphone ] În gospodăria mea/la mine acasă există:"
data[, 42] <- factor(data[, 42], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("AO02", "1", "2", "3", "Mai multe"))
names(data)[42] <- "G01Q60_SQ005"
# LimeSurvey Field type: A
data[, 43] <- as.character(data[, 43])
attributes(data)$variable.labels[43] <- "[Tabletă ] În gospodăria mea/la mine acasă există:"
data[, 43] <- factor(data[, 43], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("AO02", "1", "2", "3", "Mai multe"))
names(data)[43] <- "G01Q60_SQ006"
# LimeSurvey Field type: A
data[, 44] <- as.character(data[, 44])
attributes(data)$variable.labels[44] <- "[E-reader ] În gospodăria mea/la mine acasă există:"
data[, 44] <- factor(data[, 44], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("AO02", "1", "2", "3", "Mai multe"))
names(data)[44] <- "G01Q60_SQ007"
# LimeSurvey Field type: A
data[, 45] <- as.character(data[, 45])
attributes(data)$variable.labels[45] <- "[Instrument muzical ] În gospodăria mea/la mine acasă există:"
data[, 45] <- factor(data[, 45], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("AO02", "1", "2", "3", "Mai multe"))
names(data)[45] <- "G01Q60_SQ008"
# LimeSurvey Field type: A
data[, 46] <- as.character(data[, 46])
attributes(data)$variable.labels[46] <- "[Cărți]"
data[, 46] <- factor(data[, 46], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0-10", "11-25", "26-100", "101-200", "201-500", "Peste 500"))
names(data)[46] <- "G01Q61_SQ001"
# LimeSurvey Field type: F
data[, 47] <- as.numeric(data[, 47])
attributes(data)$variable.labels[47] <- "Nivelul de educație al mamei (alegeți cel mai înalt nivel)"
data[, 47] <- factor(data[, 47], levels=c(001,002,003,004,005,006,007,008,009),labels=c("Niciunul", "Școală primară (cls. I-IV)", "Școală gimnazială (cls. V-VIII)", "Studiile liceale (cls. IX-XII)", "Unele studii postliceale", "Absolvent universitar", "Masterat", "Doctorat", "Nu știu/ Nu răspund"))
names(data)[47] <- "G01Q55"
# LimeSurvey Field type: A
data[, 48] <- as.character(data[, 48])
attributes(data)$variable.labels[48] <- "Nivelul de educație al tatălui (alegeți cel mai înalt nivel)"
data[, 48] <- factor(data[, 48], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO10","AO08","AO09"),labels=c("Niciunul", "Școală primară (cls. I-IV)", "Școală gimnazială (cls. V-VIII)", "Studiile liceale (IX-XII)", "Unele studii postliceale", "Absolvent universitar", "Masterat", "Doctorat", "Nu știu/ Nu răspund"))
names(data)[48] <- "G01Q56"
# LimeSurvey Field type: A
data[, 49] <- as.character(data[, 49])
attributes(data)$variable.labels[49] <- "Ocupația mamei:"
data[, 49] <- factor(data[, 49], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14","AO18","AO15","AO17"),labels=c("Specialist", "Ofițer poliție / Ofițer armată", "Cadru poliție / Cadru armată", "Angajat Biserică", "Artist", "Cadru didactic", "Director / Manager / Șef departament", "Funcționar public", "Muncitor calificat", "Muncitor necalificat", "Agent / Operator diverse", "Pensionar", "Student / Elev", "Antreprenor / Afacere proprie", "Șomer / Fără ocupație", "Nu știu / Nu răspund"))
names(data)[49] <- "G01Q57"
# LimeSurvey Field type: A
data[, 50] <- as.character(data[, 50])
attributes(data)$variable.labels[50] <- "Tipul de job:"
data[, 50] <- factor(data[, 50], levels=c("AO01","AO02","AO03"),labels=c("Normă întreagă (full-time)", "Normă parțială (part-time)", "Nu se aplică/Nu știu/ Nu răspund"))
names(data)[50] <- "G02Q60"
# LimeSurvey Field type: A
data[, 51] <- as.character(data[, 51])
attributes(data)$variable.labels[51] <- "Ocupația tatălui:"
data[, 51] <- factor(data[, 51], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14","AO17","AO15","AO16"),labels=c("Specialist", "Ofițer poliție / Ofițer armată", "Cadru poliție / Cadru armată", "Angajat Biserică", "Artist", "Cadru didactic", "Director / Manager / Șef departament", "Funcționar public", "Muncitor calificat", "Muncitor necalificat", "Agent / Operator diverse", "Pensionar", "Student/ Elev", "Antreprenor / Afacere proprie", "Șomer / Fără ocupație", "Nu știu / Nu răspund"))
names(data)[51] <- "G02Q58"
# LimeSurvey Field type: A
data[, 52] <- as.character(data[, 52])
attributes(data)$variable.labels[52] <- "Tipul de job:"
data[, 52] <- factor(data[, 52], levels=c("AO01","AO02","AO03"),labels=c("Normă întreagă (full-time)", "Normă parțială (part-time)", "Nu se aplică/Nu știu/ Nu răspund"))
names(data)[52] <- "G02Q61"
# LimeSurvey Field type: A
data[, 53] <- as.character(data[, 53])
attributes(data)$variable.labels[53] <- "Ai primit vreodată un diagnostic în baza căruia să îți fie recomandate intervenții educaționale adaptate pentru cerințe educaționale speciale?"
data[, 53] <- factor(data[, 53], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[53] <- "G01Q30"
# LimeSurvey Field type: A
data[, 54] <- as.character(data[, 54])
attributes(data)$variable.labels[54] <- "Dacă da, ce diagnostic?"
data[, 54] <- factor(data[, 54], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Dificultăți de învățare", "Dislexie/ disgrafie/ discalculie", "Tulburare de spectru autist", "Tulburări vizuale", "Tulburări de auz", "Dizabilități fizice", "Tulburări de conduită"))
names(data)[54] <- "G01Q31"
# LimeSurvey Field type: A
data[, 55] <- as.character(data[, 55])
attributes(data)$variable.labels[55] <- "[Altele] Dacă da, ce diagnostic?"
names(data)[55] <- "G01Q31_other"
# LimeSurvey Field type: A
data[, 56] <- as.character(data[, 56])
attributes(data)$variable.labels[56] <- "Ai primit vreodată un diagnostic psihiatric?"
data[, 56] <- factor(data[, 56], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[56] <- "G01Q32"
# LimeSurvey Field type: A
data[, 57] <- as.character(data[, 57])
attributes(data)$variable.labels[57] <- "Dacă da, ce diagnostic?"
data[, 57] <- factor(data[, 57], levels=c("AO02","AO03","AO04","AO05"),labels=c("Tulburare depresivă", "Tulburare anxioasă", "Tulburare bipolară", "Tulburare de alimentație"))
names(data)[57] <- "G01Q33"
# LimeSurvey Field type: A
data[, 58] <- as.character(data[, 58])
attributes(data)$variable.labels[58] <- "[Altele] Dacă da, ce diagnostic?"
names(data)[58] <- "G01Q33_other"
# LimeSurvey Field type: A
data[, 59] <- as.character(data[, 59])
attributes(data)$variable.labels[59] <- "[ Ai avut dureri inexplicabile (ca de exemplu, de cap, de spate, de articulații, de abdomen, de picioare).] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 59] <- factor(data[, 59], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[59] <- "G02Q02_SQ001"
# LimeSurvey Field type: A
data[, 60] <- as.character(data[, 60])
attributes(data)$variable.labels[60] <- "[Ți-ai făcut griji în legătură cu sănătatea ta?] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 60] <- factor(data[, 60], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[60] <- "G02Q02_SQ002"
# LimeSurvey Field type: A
data[, 61] <- as.character(data[, 61])
attributes(data)$variable.labels[61] <- "[Nu ai putut să adormi, să rămâi adormit sau te-ai trezit prea devreme] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 61] <- factor(data[, 61], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[61] <- "G02Q02_SQ025"
# LimeSurvey Field type: A
data[, 62] <- as.character(data[, 62])
attributes(data)$variable.labels[62] <- "[Nu ai putut să fii atent/ă când erai la ore, făceai teme, citeai sau te jucai ceva] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 62] <- factor(data[, 62], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[62] <- "G02Q02_SQ026"
# LimeSurvey Field type: A
data[, 63] <- as.character(data[, 63])
attributes(data)$variable.labels[63] <- "[Ai simțit interes sau plăcere scăzute pentru activități.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 63] <- factor(data[, 63], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[63] <- "G02Q02_SQ012"
# LimeSurvey Field type: A
data[, 64] <- as.character(data[, 64])
attributes(data)$variable.labels[64] <- "[Ai avut sentimentul de supărare, deprimare sau lipsă de speranță pentru câteva ore.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 64] <- factor(data[, 64], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[64] <- "G02Q02_SQ013"
# LimeSurvey Field type: A
data[, 65] <- as.character(data[, 65])
attributes(data)$variable.labels[65] <- "[Te-ai simțit mai iritabil sau mai enervat decât de obicei.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 65] <- factor(data[, 65], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[65] <- "G02Q02_SQ003"
# LimeSurvey Field type: A
data[, 66] <- as.character(data[, 66])
attributes(data)$variable.labels[66] <- "[Te-ai simțit furios sau ți-ai pierdut cumpătul.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 66] <- factor(data[, 66], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[66] <- "G02Q02_SQ014"
# LimeSurvey Field type: A
data[, 67] <- as.character(data[, 67])
attributes(data)$variable.labels[67] <- "[Ai dormit mai puțin decât de obicei, dar totuși ai multă energie.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 67] <- factor(data[, 67], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[67] <- "G02Q02_SQ015"
# LimeSurvey Field type: A
data[, 68] <- as.character(data[, 68])
attributes(data)$variable.labels[68] <- "[ Ai început mult mai multe proiecte sau ai făcut mai multe lucruri riscante decât de obicei.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 68] <- factor(data[, 68], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[68] <- "G02Q02_SQ016"
# LimeSurvey Field type: A
data[, 69] <- as.character(data[, 69])
attributes(data)$variable.labels[69] <- "[Ai simțit teamă, neliniște, agitație sau îngrijorare.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 69] <- factor(data[, 69], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[69] <- "G02Q02_SQ017"
# LimeSurvey Field type: A
data[, 70] <- as.character(data[, 70])
attributes(data)$variable.labels[70] <- "[Nu te-ai putut opri din a te îngrijora.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 70] <- factor(data[, 70], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[70] <- "G02Q02_SQ018"
# LimeSurvey Field type: A
data[, 71] <- as.character(data[, 71])
attributes(data)$variable.labels[71] <- "[Nu ai putut face lucruri pe care voiai sau trebuia să le faci pentru că te făceau să te simți neliniștit/ă.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 71] <- factor(data[, 71], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[71] <- "G02Q02_SQ019"
# LimeSurvey Field type: A
data[, 72] <- as.character(data[, 72])
attributes(data)$variable.labels[72] <- "[Ai auzit voci - atunci când nimeni nu era acolo -  vorbind despre tine sau spunându-ți ce să faci.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 72] <- factor(data[, 72], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[72] <- "G02Q02_SQ020"
# LimeSurvey Field type: A
data[, 73] <- as.character(data[, 73])
attributes(data)$variable.labels[73] <- "[Ai văzut - în timp ce erai treaz/ă - ceva pe care nu-l vedea nimeni altcineva?] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 73] <- factor(data[, 73], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[73] <- "G02Q02_SQ021"
# LimeSurvey Field type: A
data[, 74] <- as.character(data[, 74])
attributes(data)$variable.labels[74] <- "[Ai avut gândul că vei face ceva rău sau că ceva rău ți se va întâmpla ție sau cuiva drag.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 74] <- factor(data[, 74], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[74] <- "G02Q02_SQ004"
# LimeSurvey Field type: A
data[, 75] <- as.character(data[, 75])
attributes(data)$variable.labels[75] <- "[Ai simțit nevoia să verifici în repetate rânduri anumite lucruri (spre exemplu, dacă ușa e încuiată sau dacă ai lăsat aragazul pornit).] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 75] <- factor(data[, 75], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[75] <- "G02Q02_SQ022"
# LimeSurvey Field type: A
data[, 76] <- as.character(data[, 76])
attributes(data)$variable.labels[76] <- "[Ți-ai făcut griji că ai atins lucruri murdare sau că te poți contamina sau otrăvi.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 76] <- factor(data[, 76], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[76] <- "G02Q02_SQ023"
# LimeSurvey Field type: A
data[, 77] <- as.character(data[, 77])
attributes(data)$variable.labels[77] <- "[Ai simțit nevoia să faci lucrurile într-un anumit fel (cum ar fi să numeri sau să spui ceva special) ca ceva răuu să nu se întâmple. ] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 77] <- factor(data[, 77], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[77] <- "G02Q02_SQ024"
# LimeSurvey Field type: A
data[, 78] <- as.character(data[, 78])
attributes(data)$variable.labels[78] <- "[Ai avut sentimentul că nu ești apropiat/ă de ceilalți sau că nu te bucuri de relațiile cu ei.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 78] <- factor(data[, 78], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[78] <- "G02Q02_SQ008"
# LimeSurvey Field type: A
data[, 79] <- as.character(data[, 79])
attributes(data)$variable.labels[79] <- "[Ai avut senzația că nu știi cine ești cu adevărat sau ce îți dorești de la viață.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 79] <- factor(data[, 79], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[79] <- "G02Q02_SQ027"
# LimeSurvey Field type: A
data[, 80] <- as.character(data[, 80])
attributes(data)$variable.labels[80] <- "[Ai avut o senzație de detașare sau sentimentul de distanțare față de tine, de corpul tău, de împrejurimi sau de propriile amintiri.] În următoarele rânduri vei fi întrebat despre anumite lucruri care este posibil să te fi afectat recent sau care te afectează în general. La fiecare întrebare te rugăm să marchezi opțiunea care te caracterizează cel mai bine.    În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 80] <- factor(data[, 80], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Rar, mai puțin de o zi sau două", "Puțin, câteva zile", "Adesea, mai mult de jumătate din timp", "Mult, aproape în fiecare zi"))
names(data)[80] <- "G02Q02_SQ028"
# LimeSurvey Field type: A
data[, 81] <- as.character(data[, 81])
attributes(data)$variable.labels[81] <- "[ Ai consumat băuturi alcoolice (bere, vin, tărie, etc.)] În ultimele DOUĂ săptămâni..."
data[, 81] <- factor(data[, 81], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[81] <- "G02Q47_SQ001"
# LimeSurvey Field type: A
data[, 82] <- as.character(data[, 82])
attributes(data)$variable.labels[82] <- "[Ai fumat țigări, trabuc sau ai folosit tabac ori tutun de mestecat.] În ultimele DOUĂ săptămâni..."
data[, 82] <- factor(data[, 82], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[82] <- "G02Q47_SQ002"
# LimeSurvey Field type: A
data[, 83] <- as.character(data[, 83])
attributes(data)$variable.labels[83] <- "[Ai folosit vreunul din următoarele medicamente pe cont propriu, adică fără prescripția unui medic, în cantități mai mari sau pe perioade mai lungi decât au fost prescrise (de exemplu, analgezice, precum Codeină, stimulenți, precum Metilfenidat sau Amfetamine/Adderall), sedative sau tranchilizante (precum somniferele sau Diazepam) ori droguri, precum marijuana, cocaină sau drogurile de club (precum Ecstasy), halucinogene (precum LSD), heroină, inhalante sau solvenți (precum lipiciul) ori metamfetamină.] În ultimele DOUĂ săptămâni..."
data[, 83] <- factor(data[, 83], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[83] <- "G02Q47_SQ003"
# LimeSurvey Field type: A
data[, 84] <- as.character(data[, 84])
attributes(data)$variable.labels[84] <- "Te-ai gândit vreodată să te sinucizi?"
data[, 84] <- factor(data[, 84], levels=c("AO01","AO02"),labels=c("Da", "Nu"))
names(data)[84] <- "G02Q48"
# LimeSurvey Field type: A
data[, 85] <- as.character(data[, 85])
attributes(data)$variable.labels[85] <- "[Ai simțit puțin interes sau plăcere în a face diverse lucruri.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 85] <- factor(data[, 85], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[85] <- "G09Q39_SQ002"
# LimeSurvey Field type: A
data[, 86] <- as.character(data[, 86])
attributes(data)$variable.labels[86] <- "[Te-ai simțit trist/ă, deprimat/ă, iritabil/ă sau lipsit/ă de speranță.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 86] <- factor(data[, 86], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[86] <- "G09Q39_SQ003"
# LimeSurvey Field type: A
data[, 87] <- as.character(data[, 87])
attributes(data)$variable.labels[87] <- "[Ți-a fost greu să adormi sau să rămâi adormit/ă, sau ai dormit prea mult.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 87] <- factor(data[, 87], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[87] <- "G09Q39_SQ004"
# LimeSurvey Field type: A
data[, 88] <- as.character(data[, 88])
attributes(data)$variable.labels[88] <- "[Te-ați simțit obosit/ă sau ai avut foarte puțină energie.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 88] <- factor(data[, 88], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[88] <- "G09Q39_SQ005"
# LimeSurvey Field type: A
data[, 89] <- as.character(data[, 89])
attributes(data)$variable.labels[89] <- "[Nu ai avut poftă de mâncare sau ai mâncat prea mult.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 89] <- factor(data[, 89], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[89] <- "G09Q39_SQ006"
# LimeSurvey Field type: A
data[, 90] <- as.character(data[, 90])
attributes(data)$variable.labels[90] <- "[Te-ai simțit deziluzionat/ă - ai simțit că ești un/o ratat/ă sau că ți-ai dezamăgit familia.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 90] <- factor(data[, 90], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[90] <- "G09Q39_SQ007"
# LimeSurvey Field type: A
data[, 91] <- as.character(data[, 91])
attributes(data)$variable.labels[91] <- "[Ți-a fost greu să te concentrezi, ca de exemplu să citești, să faci temele sau să te uiți la televizor.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 91] <- factor(data[, 91], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[91] <- "G09Q39_SQ008"
# LimeSurvey Field type: A
data[, 92] <- as.character(data[, 92])
attributes(data)$variable.labels[92] <- "[Te-ai mișcat și ai vorbit atât de încet încât au observat și ceilalți. Sau dimpotrivă, ai fost atât de agitat/ă și neliniștit/ă încât te-ai mișcat mai mult decât de obicei.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 92] <- factor(data[, 92], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[92] <- "G09Q39_SQ009"
# LimeSurvey Field type: A
data[, 93] <- as.character(data[, 93])
attributes(data)$variable.labels[93] <- "[Ți-a trecut prin cap că ar fi mai bine dacă ai muri sau dacă ți-ați face rău în vreun fel.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 93] <- factor(data[, 93], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[93] <- "G09Q39_SQ010"
# LimeSurvey Field type: A
data[, 94] <- as.character(data[, 94])
attributes(data)$variable.labels[94] <- "[Dacă ai avut una sau mai multe dintre problemele de mai sus, în ce măsură te-au împiedicat aceste probleme să lucrezi, să te ocupi de lucrurile casnice sau să te înțelegi cu alți oameni?] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 94] <- factor(data[, 94], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[94] <- "G09Q39_SQ011"
# LimeSurvey Field type: A
data[, 95] <- as.character(data[, 95])
attributes(data)$variable.labels[95] <- "[Dacă ai avut una sau mai multe dintre problemele de mai sus, în ce măsură te-au împiedicat aceste probleme să lucrezi, să te ocupi de lucrurile casnice sau să te înțelegi cu alți oameni?]"
data[, 95] <- factor(data[, 95], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În mică măsură", "În mare măsură", "Extrem de mult"))
names(data)[95] <- "G09Q43_SQ001"
# LimeSurvey Field type: A
data[, 96] <- as.character(data[, 96])
attributes(data)$variable.labels[96] <- "[Te-ai simțit agitat/ă, anxios/oasă sau tensionat/ă.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 96] <- factor(data[, 96], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[96] <- "G10Q41_SQ002"
# LimeSurvey Field type: A
data[, 97] <- as.character(data[, 97])
attributes(data)$variable.labels[97] <- "[Nu ai putut să îți oprești sau să îți controlezi îngrijorarea.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 97] <- factor(data[, 97], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[97] <- "G10Q41_SQ003"
# LimeSurvey Field type: A
data[, 98] <- as.character(data[, 98])
attributes(data)$variable.labels[98] <- "[Te-ai îngrijorat prea mult în legătură cu diferite lucruri.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 98] <- factor(data[, 98], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[98] <- "G10Q41_SQ004"
# LimeSurvey Field type: A
data[, 99] <- as.character(data[, 99])
attributes(data)$variable.labels[99] <- "[Ai avut dificultăți în a te relaxa.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 99] <- factor(data[, 99], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[99] <- "G10Q41_SQ005"
# LimeSurvey Field type: A
data[, 100] <- as.character(data[, 100])
attributes(data)$variable.labels[100] <- "[Ai fost așa agitat/ă că nu ai putut sta locului.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 100] <- factor(data[, 100], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[100] <- "G10Q41_SQ006"
# LimeSurvey Field type: A
data[, 101] <- as.character(data[, 101])
attributes(data)$variable.labels[101] <- "[Ai fost iritabil/ă sau te-ai enervat ușor.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 101] <- factor(data[, 101], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[101] <- "G10Q41_SQ007"
# LimeSurvey Field type: A
data[, 102] <- as.character(data[, 102])
attributes(data)$variable.labels[102] <- "[Te-ai simțit temător/oare, ca și cum ceva groaznic ar urma să se întâmple.] În ultimele DOUĂ (2) SĂPTĂMÂNI, cât de mult (sau cât de des) te-au afectat următoarele probleme?"
data[, 102] <- factor(data[, 102], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În unele zile", "Mai mult de jumătate din zile", "Aproape zilnic"))
names(data)[102] <- "G10Q41_SQ008"
# LimeSurvey Field type: A
data[, 103] <- as.character(data[, 103])
attributes(data)$variable.labels[103] <- "[Dacă ai avut una sau mai multe dintre problemele de mai sus, în ce măsură te-au împiedicat aceste probleme să lucrezi, să te ocupi de lucrurile casnice sau să te înțelegi cu alți oameni?]"
data[, 103] <- factor(data[, 103], levels=c("AO02","AO03","AO04","AO05"),labels=c("Deloc", "În mică măsură", "În mare măsură", "Extrem de mult"))
names(data)[103] <- "G10Q44_SQ001"
# LimeSurvey Field type: A
data[, 104] <- as.character(data[, 104])
attributes(data)$variable.labels[104] <- "[Ai avut momente în care ai simțit brusc teroare, frică sau spaimă în situații sociale.] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 104] <- factor(data[, 104], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[104] <- "G11Q42_SQ002"
# LimeSurvey Field type: A
data[, 105] <- as.character(data[, 105])
attributes(data)$variable.labels[105] <- "[Te-ai simțit anxios/oasă, îngrijorat/ă sau agitat/ă în situații sociale.] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 105] <- factor(data[, 105], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[105] <- "G11Q42_SQ003"
# LimeSurvey Field type: A
data[, 106] <- as.character(data[, 106])
attributes(data)$variable.labels[106] <- "[Te-ai gândit că vei fi respins/ă, umilit/ă, jenat/ă, ridiculizat/ă sau că îi vei jigni pe ceilalți.] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 106] <- factor(data[, 106], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[106] <- "G11Q42_SQ004"
# LimeSurvey Field type: A
data[, 107] <- as.character(data[, 107])
attributes(data)$variable.labels[107] <- "[Ai simțit că îți crește pulsul, transpiri, ai dificultăți de respirație, tremuri sau ai stări de leșin în situații sociale.] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 107] <- factor(data[, 107], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[107] <- "G11Q42_SQ005"
# LimeSurvey Field type: A
data[, 108] <- as.character(data[, 108])
attributes(data)$variable.labels[108] <- "[Ai simțit tensiune în mușchi, te-ai simțit încordat/ă, neliniștit/ă sau ți-a fost greu să te relaxezi în situații sociale.] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 108] <- factor(data[, 108], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[108] <- "G11Q42_SQ006"
# LimeSurvey Field type: A
data[, 109] <- as.character(data[, 109])
attributes(data)$variable.labels[109] <- "[Ai evitat sau nu ai intrat în situații sociale.] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 109] <- factor(data[, 109], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[109] <- "G11Q42_SQ007"
# LimeSurvey Field type: A
data[, 110] <- as.character(data[, 110])
attributes(data)$variable.labels[110] <- "[Ai părăsit situațiile sociale devreme sau ai participat doar minimal/ foarte puțin la ele (ex: nu ai vorbit prea mult, ai evitat contactul vizual).] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 110] <- factor(data[, 110], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[110] <- "G11Q42_SQ008"
# LimeSurvey Field type: A
data[, 111] <- as.character(data[, 111])
attributes(data)$variable.labels[111] <- "[Ai petrecut mult timp plănuind ce să spui sau cum să te comporți în situații sociale.] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 111] <- factor(data[, 111], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[111] <- "G11Q42_SQ009"
# LimeSurvey Field type: A
data[, 112] <- as.character(data[, 112])
attributes(data)$variable.labels[112] <- "[Te-ai distras ca să eviți să te gândești la situații sociale.] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 112] <- factor(data[, 112], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[112] <- "G11Q42_SQ010"
# LimeSurvey Field type: A
data[, 113] <- as.character(data[, 113])
attributes(data)$variable.labels[113] <- "[Ai avut nevoie de ajutor ca să faci față situațiilor sociale (ex: alcool, medicamente, anumite obiecte).] Următoarele întrebări se referă la gânduri, emoții și comportamente pe care le-ai avut în contexte sociale, ca de exemplu vorbitul în public, vorbitul la întâlniri, participarea la evenimente sau petreceri, prezentarea ta în fața altora, participarea la conversații, oferirea sau primirea de complimente, cererea unui favor, mâncatul sau scrisul în public."
data[, 113] <- factor(data[, 113], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Niciodată", "Ocazional", "În jumătate din timp", "În majoritatea timpului", "Tot timpul"))
names(data)[113] <- "G11Q42_SQ011"
# LimeSurvey Field type: A
data[, 114] <- as.character(data[, 114])
attributes(data)$variable.labels[114] <- "[În ultimele 6 luni, cât de des ai avut probleme cu organizarea sau concentrarea, cum ar fi să ți se distragă ușor atenția în timpul orelor, să fii dezorganizat/ă sau să ai dificultăți în a finaliza un proiect sau o sarcină?]"
data[, 114] <- factor(data[, 114], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Deseori", "Uneori", "Rareori", "Niciodată"))
names(data)[114] <- "G03Q03_SQ001"
# LimeSurvey Field type: A
data[, 115] <- as.character(data[, 115])
attributes(data)$variable.labels[115] <- "[Cât de des ai avut dificultăți în ceea ce privește organizarea, concentrarea sau impulsivitatea, în ultimele 6 luni?]"
data[, 115] <- factor(data[, 115], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Deseori", "Uneori", "Rareori", "Niciodată"))
names(data)[115] <- "G03Q03_SQ002"
# LimeSurvey Field type: A
data[, 116] <- as.character(data[, 116])
attributes(data)$variable.labels[116] <- "[Ai evitat sau amânat să începi o sarcină care necesită a gândi mult]"
data[, 116] <- factor(data[, 116], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Deseori", "Uneori", "Rareori", "Niciodată"))
names(data)[116] <- "G03Q03_SQ003"
# LimeSurvey Field type: A
data[, 117] <- as.character(data[, 117])
attributes(data)$variable.labels[117] <- "[Ai întâmpinat probleme în a-ți aminti programări sau îndatoriri]"
data[, 117] <- factor(data[, 117], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Deseori", "Uneori", "Rareori", "Niciodată"))
names(data)[117] <- "G03Q03_SQ004"
# LimeSurvey Field type: A
data[, 118] <- as.character(data[, 118])
attributes(data)$variable.labels[118] <- "[Ți-a fost greu să structurezi sau să pui la punct o sarcină (ex: să faci o temă, un proiect)]"
data[, 118] <- factor(data[, 118], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Deseori", "Uneori", "Rareori", "Niciodată"))
names(data)[118] <- "G03Q03_SQ005"
# LimeSurvey Field type: A
data[, 119] <- as.character(data[, 119])
attributes(data)$variable.labels[119] <- "[Ți-a fost greu să termini un proiect deși părțile cele mai dificile fuseseră rezolvate.]"
data[, 119] <- factor(data[, 119], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Deseori", "Uneori", "Rareori", "Niciodată"))
names(data)[119] <- "G03Q03_SQ006"
# LimeSurvey Field type: A
data[, 120] <- as.character(data[, 120])
attributes(data)$variable.labels[120] <- "[Te-ai simțit foarte activ/ă și constrâns/ă să faceți lucruri, ca și cum ai fi condus/ă de un motor]"
data[, 120] <- factor(data[, 120], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Deseori", "Uneori", "Rareori", "Niciodată"))
names(data)[120] <- "G03Q03_SQ007"
# LimeSurvey Field type: A
data[, 121] <- as.character(data[, 121])
attributes(data)$variable.labels[121] <- "[Ți-ai foit sau agitat mâinile sau picioarele când ai fost nevoit/ă să stai jos pentru un timp mai îndelungat.]"
data[, 121] <- factor(data[, 121], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Deseori", "Uneori", "Rareori", "Niciodată"))
names(data)[121] <- "G03Q03_SQ008"
# LimeSurvey Field type: A
data[, 122] <- as.character(data[, 122])
attributes(data)$variable.labels[122] <- " Aproximativ câți ani aveai atunci când au apărut pentru prima dată probleme legate de organizare, concentrare sau impulsivitate?"
data[, 122] <- factor(data[, 122], levels=c("AO16","AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14","AO15"),labels=c("Nu este aplicabil", "4 sau mai puțin", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"))
names(data)[122] <- "G03Q04"
# LimeSurvey Field type: A
data[, 123] <- as.character(data[, 123])
attributes(data)$variable.labels[123] <- "[Dezastru natural (de exemplu: inundație, uragan, tornadă, cutremur)] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 123] <- factor(data[, 123], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[123] <- "G05Q09_SQ001Dezastrunatural"
# LimeSurvey Field type: A
data[, 124] <- as.character(data[, 124])
attributes(data)$variable.labels[124] <- "[Foc sau explozie	] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 124] <- factor(data[, 124], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[124] <- "G05Q09_SQ002Focsauexplozie"
# LimeSurvey Field type: A
data[, 125] <- as.character(data[, 125])
attributes(data)$variable.labels[125] <- "[Accident de circulație (de exemplu, accident de mașină, de barcă, de tren sau de avion)	] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 125] <- factor(data[, 125], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[125] <- "G05Q09_SQ003Accidentdecircu"
# LimeSurvey Field type: A
data[, 126] <- as.character(data[, 126])
attributes(data)$variable.labels[126] <- "[Accident grav la școală, acasă sau în timpul liber	] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 126] <- factor(data[, 126], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[126] <- "G05Q09_SQ004Accidentgravlac"
# LimeSurvey Field type: A
data[, 127] <- as.character(data[, 127])
attributes(data)$variable.labels[127] <- "[ Expunere la substanțe toxice (de exemplu, substanțe chimice periculoase sau radiații)] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 127] <- factor(data[, 127], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[127] <- "G05Q09_SQ005Expunerelasubst"
# LimeSurvey Field type: A
data[, 128] <- as.character(data[, 128])
attributes(data)$variable.labels[128] <- "[Agresiune fizică (de exemplu, atacat, lovit, pălmuit, lovit cu piciorul, bătut)] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 128] <- factor(data[, 128], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[128] <- "G05Q09_SQ006Agresiunefizicd"
# LimeSurvey Field type: A
data[, 129] <- as.character(data[, 129])
attributes(data)$variable.labels[129] <- "[Atac armat (de exemplu, împușcătură, înjunghiere, amenințare cu un cuțit, armă, bombă)	] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 129] <- factor(data[, 129], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[129] <- "G05Q09_SQ007Atacarmatdeexem"
# LimeSurvey Field type: A
data[, 130] <- as.character(data[, 130])
attributes(data)$variable.labels[130] <- "[Agresiune sexuală (viol, tentativă de viol, silire la orice tip de act sexual prin forță sau amenințare)] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 130] <- factor(data[, 130], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[130] <- "G05Q09_SQ008Agresiunesexual"
# LimeSurvey Field type: A
data[, 131] <- as.character(data[, 131])
attributes(data)$variable.labels[131] <- "[Alte experiențe sexuale nedorite sau inconfortabile] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 131] <- factor(data[, 131], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[131] <- "G05Q09_SQ009Alteexperienese"
# LimeSurvey Field type: A
data[, 132] <- as.character(data[, 132])
attributes(data)$variable.labels[132] <- "[Expunere într-o zonă de război	] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 132] <- factor(data[, 132], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[132] <- "G05Q09_SQ010Expunerentrozon"
# LimeSurvey Field type: A
data[, 133] <- as.character(data[, 133])
attributes(data)$variable.labels[133] <- "[Captivitate (de exemplu, răpit/ă, ținut/ă ostatic, prizonier/ă în război)	] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 133] <- factor(data[, 133], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[133] <- "G05Q09_SQ011Captivitatedeex"
# LimeSurvey Field type: A
data[, 134] <- as.character(data[, 134])
attributes(data)$variable.labels[134] <- "[Rănire sau boală gravă		] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 134] <- factor(data[, 134], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[134] <- "G05Q09_SQ012Rniresauboalgra"
# LimeSurvey Field type: A
data[, 135] <- as.character(data[, 135])
attributes(data)$variable.labels[135] <- "[Suferință umană severă] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 135] <- factor(data[, 135], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[135] <- "G05Q09_SQ013Suferinumanseve"
# LimeSurvey Field type: A
data[, 136] <- as.character(data[, 136])
attributes(data)$variable.labels[136] <- "[Moarte violentă subită (de exemplu, omucidere, sinucidere)] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 136] <- factor(data[, 136], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[136] <- "G05Q09_SQ014Moarteviolentsu"
# LimeSurvey Field type: A
data[, 137] <- as.character(data[, 137])
attributes(data)$variable.labels[137] <- "[Moarte accidentală subită] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 137] <- factor(data[, 137], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[137] <- "G05Q09_SQ015Moarteaccidenta"
# LimeSurvey Field type: A
data[, 138] <- as.character(data[, 138])
attributes(data)$variable.labels[138] <- "[Rănire gravă sau moarte cauzate de dvs. altcuiva.] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 138] <- factor(data[, 138], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[138] <- "G05Q09_SQ016Rniregravsaumoa"
# LimeSurvey Field type: A
data[, 139] <- as.character(data[, 139])
attributes(data)$variable.labels[139] <- "[Moarte a unui membru al familei de la COVID] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 139] <- factor(data[, 139], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[139] <- "G05Q09_SQ018"
# LimeSurvey Field type: A
data[, 140] <- as.character(data[, 140])
attributes(data)$variable.labels[140] <- "[Oricare alt eveniment sau experiență foarte stresantă] Mai jos sunt prezentate o serie de evenimente stresante prin care trec uneori oamenii. Pentru fiecare dintre ele indică dacă: (a) ți s-a întâmplat personal; (b) ai fost martor când i s-a întâmplat altcuiva; (c) ai aflat că i s-a întâmplat unui membru apropiat al familiei sau unui prieten apropiat; (d) nu ești sigur/ă dacă ți se potrivește; sau (e) nu ți s-a întâmplat/nu se aplică.   Ai grijă să iei în considerare întreaga ta viață (din copilărie până acum) atunci când treci prin lista de evenimente."
data[, 140] <- factor(data[, 140], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Mi s-a întâmplat mie", "S-a întâmplat unei persoane apropiate", "Am fost martor", "Am aflat despre el", "Nu sunt sigur/ă", "Nu s-a întâmplat"))
names(data)[140] <- "G05Q09_SQ017"
# LimeSurvey Field type: A
data[, 141] <- as.character(data[, 141])
attributes(data)$variable.labels[141] <- "Dacă ai bifat opțiunea „oricare alt eveniment sau experiență foarte stresantă”, te rugăm să precizezi care este aceea."
names(data)[141] <- "G05Q10"
# LimeSurvey Field type: A
data[, 142] <- as.character(data[, 142])
attributes(data)$variable.labels[142] <- " De asemenea, te rugăm să apreciezi cât de puternic te-ai simțit afectat(ă) în mod negativ sau stresat(ă) de perioada Martie 2020 - Martie 2022, cu tot ce a însemnat aceasta (pandemie, restricții, școala online etc)."
data[, 142] <- factor(data[, 142], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte mult / Extrem", "Mult", "Moderat / Așa și așa", "Puțin", "Deloc / Nesemnificativ"))
names(data)[142] <- "G05Q11"
# LimeSurvey Field type: F
data[, 143] <- as.numeric(data[, 143])
attributes(data)$variable.labels[143] <- "[Îmi vin în minte diverse amintiri despre eveniment pe care nu le pot controla.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[143] <- "G05Q12_SQ002"
# LimeSurvey Field type: F
data[, 144] <- as.numeric(data[, 144])
attributes(data)$variable.labels[144] <- "[Retrăiesc evenimentul ca și cum s-ar întâmpla din nou.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[144] <- "G05Q12_SQ004"
# LimeSurvey Field type: F
data[, 145] <- as.numeric(data[, 145])
attributes(data)$variable.labels[145] <- "[Am o stare de neliniște și zbucium intern când întâlnesc stimuli care-mi amintesc de ce mi s-a întâmplat.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[145] <- "G05Q12_SQ006"
# LimeSurvey Field type: F
data[, 146] <- as.numeric(data[, 146])
attributes(data)$variable.labels[146] <- "[Am senzații intense și neplăcute în corp atunci când ceva îmi amintește de evenimentul traumatic.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[146] <- "G05Q12_SQ008"
# LimeSurvey Field type: F
data[, 147] <- as.numeric(data[, 147])
attributes(data)$variable.labels[147] <- "[Mă străduiesc să uit ceea ce s-a întâmplat.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[147] <- "G05Q12_SQ010"
# LimeSurvey Field type: F
data[, 148] <- as.numeric(data[, 148])
attributes(data)$variable.labels[148] <- "[Evit să intru în contact cu ceva ce îmi poate aminti de acel eveniment.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[148] <- "G05Q12_SQ012"
# LimeSurvey Field type: F
data[, 149] <- as.numeric(data[, 149])
attributes(data)$variable.labels[149] <- "[Nu-mi place să stau de vorbă despre lucruri care-mi amintesc de acel eveniment.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[149] <- "G05Q12_SQ014"
# LimeSurvey Field type: F
data[, 150] <- as.numeric(data[, 150])
attributes(data)$variable.labels[150] <- "[Am dificultăți să-mi amintesc părți importante ale evenimentului traumatic.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[150] <- "G05Q12_SQ016"
# LimeSurvey Field type: F
data[, 151] <- as.numeric(data[, 151])
attributes(data)$variable.labels[151] <- "[Mi-am pierdut sentimentul încrederii în mine și în ceilalți.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[151] <- "G05Q12_SQ018"
# LimeSurvey Field type: F
data[, 152] <- as.numeric(data[, 152])
attributes(data)$variable.labels[152] <- "[Cred că sunt de vină pentru ceea ce mi se întâmplă.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[152] <- "G05Q12_SQ020"
# LimeSurvey Field type: F
data[, 153] <- as.numeric(data[, 153])
attributes(data)$variable.labels[153] <- "[Mi-e greu să mă mai bucur de lucrurile care înainte îmi făceau plăcere.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[153] <- "G05Q12_SQ022"
# LimeSurvey Field type: F
data[, 154] <- as.numeric(data[, 154])
attributes(data)$variable.labels[154] <- "[Mă simt rupt de lume.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[154] <- "G05Q12_SQ024"
# LimeSurvey Field type: F
data[, 155] <- as.numeric(data[, 155])
attributes(data)$variable.labels[155] <- "[Parcă mi-am pierdut bucuria de viață.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[155] <- "G05Q12_SQ026"
# LimeSurvey Field type: F
data[, 156] <- as.numeric(data[, 156])
attributes(data)$variable.labels[156] <- "[Pot fi scos ușor din sărite de lucruri de nimic.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[156] <- "G05Q12_SQ028"
# LimeSurvey Field type: F
data[, 157] <- as.numeric(data[, 157])
attributes(data)$variable.labels[157] <- "[Îmi vine să-mi fac rău mie sau altora.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[157] <- "G05Q12_SQ030"
# LimeSurvey Field type: F
data[, 158] <- as.numeric(data[, 158])
attributes(data)$variable.labels[158] <- "[Mi-e greu să mă concentrez pe ceea ce am de făcut.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[158] <- "G05Q12_SQ032"
# LimeSurvey Field type: F
data[, 159] <- as.numeric(data[, 159])
attributes(data)$variable.labels[159] <- "[Dorm puțin și nu mă pot odihni cu adevărat.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[159] <- "G05Q12_SQ034"
# LimeSurvey Field type: F
data[, 160] <- as.numeric(data[, 160])
attributes(data)$variable.labels[160] <- "[Stau aproape permanent în gardă, ca să mă pot proteja.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[160] <- "G05Q12_SQ036"
# LimeSurvey Field type: F
data[, 161] <- as.numeric(data[, 161])
attributes(data)$variable.labels[161] <- "[Am observat că tresar puternic la unele sunete care vin pe neașteptate.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[161] <- "G05Q12_SQ038"
# LimeSurvey Field type: F
data[, 162] <- as.numeric(data[, 162])
attributes(data)$variable.labels[162] <- "[Uneori sar ca un arc.] Uitându-te în urmă la ceea ce s-a întâmplat, ajută-ne să înțelegem cum ai reacționat după evenimentul/ evenimentele stresante cu care te-ai confruntat (cel/e bifat/e mai sus, în prima parte a acestei secțiuni).   În continuare, te rugăm să răspunzi bifând varianta care se potrivește cel mai bine cu experiența ta din ultima perioadă, între 1 = Fals, Dezacord și 10 = Adevărat, Acord."
names(data)[162] <- "G05Q12_SQ040"
# LimeSurvey Field type: F
data[, 163] <- as.numeric(data[, 163])
attributes(data)$variable.labels[163] <- "[Părinții sau tutorii tăi legali s-au separat sau au divorțat] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 163] <- factor(data[, 163], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[163] <- "G04Q05_SQ001"
# LimeSurvey Field type: F
data[, 164] <- as.numeric(data[, 164])
attributes(data)$variable.labels[164] <- "[Ai locuit în aceeași casă cu o persoană care a fost deținută de către poliție sau încarcerată (în închisoare)] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 164] <- factor(data[, 164], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[164] <- "G04Q05_SQ002"
# LimeSurvey Field type: F
data[, 165] <- as.numeric(data[, 165])
attributes(data)$variable.labels[165] <- "[Ai locuit în aceeași casă cu o persoană deprimată, bolnavă mintal sau care a încercat să se sinucidă] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 165] <- factor(data[, 165], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[165] <- "G04Q05_SQ003"
# LimeSurvey Field type: F
data[, 166] <- as.numeric(data[, 166])
attributes(data)$variable.labels[166] <- "[Ai văzut sau auzit cum persoane din casă se rănesc sau se amenință că se vor răni] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 166] <- factor(data[, 166], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[166] <- "G04Q05_SQ004"
# LimeSurvey Field type: F
data[, 167] <- as.numeric(data[, 167])
attributes(data)$variable.labels[167] <- "[O persoană din casă te-a înjurat, insultat, umilit, sau te-a redus la tăcere într-un mod care te-a speriat SAU persoana s-a purtat în așa manieră încât să-ți fie frică că persoana îți poate face rău fizic] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 167] <- factor(data[, 167], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[167] <- "G04Q05_SQ005"
# LimeSurvey Field type: F
data[, 168] <- as.numeric(data[, 168])
attributes(data)$variable.labels[168] <- "[O persoană te-a atins în zonele intime sau ți-a cerut să îi atingi părți intime într-o manieră sexuală, nedorită, împotriva voinței tale, sau care te-a făcut să te simți inconfortabil] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 168] <- factor(data[, 168], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[168] <- "G04Q05_SQ006"
# LimeSurvey Field type: F
data[, 169] <- as.numeric(data[, 169])
attributes(data)$variable.labels[169] <- "[S-a întâmplat nu doar o dată să trebuiască să stai fără mâncare, fără haine, fără o locuință, sau să nu ai pe nimeni care să te protejeze] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 169] <- factor(data[, 169], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[169] <- "G04Q05_SQ007"
# LimeSurvey Field type: F
data[, 170] <- as.numeric(data[, 170])
attributes(data)$variable.labels[170] <- "[O persoană te-a împins, apucat, pălmuit, a aruncat cu ceva în tine SAU ai fost lovit/ă cu suficientă foță încât să fii rănit/ă sau să îți rămână urme pe corp] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 170] <- factor(data[, 170], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[170] <- "G04Q05_SQ008"
# LimeSurvey Field type: F
data[, 171] <- as.numeric(data[, 171])
attributes(data)$variable.labels[171] <- "[Te-ai simțit adesea nesprijinit/ă, neiubit/ă și/sau neprotejat/ă] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 171] <- factor(data[, 171], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[171] <- "G04Q05_SQ009"
# LimeSurvey Field type: F
data[, 172] <- as.numeric(data[, 172])
attributes(data)$variable.labels[172] <- "[Ai locuit cu o persoană care a avut o problemă cu alcoolul sau cu drogurile] Secțiunea 1. În orice moment de la nașterea ta...  "
data[, 172] <- factor(data[, 172], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[172] <- "G04Q05_SQ010"
# LimeSurvey Field type: A
data[, 173] <- as.character(data[, 173])
attributes(data)$variable.labels[173] <- "Dintre afirmațiile din Secțiunea 1, câte ți se aplică? Bifează-le sau notează numărul total de afirmații în căsuță.               "
data[, 173] <- factor(data[, 173], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11"),labels=c("AO01", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
names(data)[173] <- "G04Q06"
# LimeSurvey Field type: F
data[, 174] <- as.numeric(data[, 174])
attributes(data)$variable.labels[174] <- "[ Ai fost instituționalizat/ă într-un centru de plasament]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 174] <- factor(data[, 174], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[174] <- "G04Q07_SQ001"
# LimeSurvey Field type: F
data[, 175] <- as.numeric(data[, 175])
attributes(data)$variable.labels[175] <- "[Ai fost hărțuit/ă sau terorizat/ă la școală]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 175] <- factor(data[, 175], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[175] <- "G04Q07_SQ002"
# LimeSurvey Field type: F
data[, 176] <- as.numeric(data[, 176])
attributes(data)$variable.labels[176] <- "[ Ai locuit cu un părinte sau tutore care a decedat]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 176] <- factor(data[, 176], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[176] <- "G04Q07_SQ003"
# LimeSurvey Field type: F
data[, 177] <- as.numeric(data[, 177])
attributes(data)$variable.labels[177] <- "[Ai fost separat de persoana care te-a îngrijit din cauza deportării sau imigrării]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 177] <- factor(data[, 177], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[177] <- "G04Q07_SQ004"
# LimeSurvey Field type: F
data[, 178] <- as.numeric(data[, 178])
attributes(data)$variable.labels[178] <- "[Ai suferit o intervenție medicală majoră sau ai avut o boală care ți-a amenințat viața]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 178] <- factor(data[, 178], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[178] <- "G04Q07_SQ005"
# LimeSurvey Field type: F
data[, 179] <- as.numeric(data[, 179])
attributes(data)$variable.labels[179] <- "[Ai asistat (privind sau auzind) adesea la violențe, în cartier sau la școală]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 179] <- factor(data[, 179], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[179] <- "G04Q07_SQ006"
# LimeSurvey Field type: F
data[, 180] <- as.numeric(data[, 180])
attributes(data)$variable.labels[180] <- "[Ai fost reținut/ă de către poliție, arestat/ă sau încarcerat/ă (în închisoare)]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 180] <- factor(data[, 180], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[180] <- "G04Q07_SQ007"
# LimeSurvey Field type: F
data[, 181] <- as.numeric(data[, 181])
attributes(data)$variable.labels[181] <- "[Ai fost adesea tratat/ă cu răutate de către alții din cauza rasei, orientării sexuale, locului unde te-ai născut, religiei sau a unei dizabilități ale tale]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 181] <- factor(data[, 181], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[181] <- "G04Q07_SQ008"
# LimeSurvey Field type: F
data[, 182] <- as.numeric(data[, 182])
attributes(data)$variable.labels[182] <- "[Ai fost abuzat/ă verbal sau fizic sau ai primit amenințări din partea unui partener romantic (iubit/ă)]   Secțiunea 2. În orice moment de la nașterea ta..."
data[, 182] <- factor(data[, 182], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[182] <- "G04Q07_SQ009"
# LimeSurvey Field type: A
data[, 183] <- as.character(data[, 183])
attributes(data)$variable.labels[183] <- "Dintre afirmațiile din Secțiunea 2, câte ți se aplică? Notează numărul total de afirmații în căsuță."
data[, 183] <- factor(data[, 183], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10"),labels=c("AO01", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
names(data)[183] <- "G04Q08"
# LimeSurvey Field type: A
data[, 184] <- as.character(data[, 184])
attributes(data)$variable.labels[184] <- "[Dacă aș crede că am o cădere nervoasă, primul meu impuls ar fi să apelez la ajutor specializat.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 184] <- factor(data[, 184], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[184] <- "G06Q13_SQ002"
# LimeSurvey Field type: A
data[, 185] <- as.character(data[, 185])
attributes(data)$variable.labels[185] <- "[Ideea de a vorbi cu un psiholog mi se pare o modalitate proastă de a rezolva probleme emoționale.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 185] <- factor(data[, 185], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[185] <- "G06Q13_SQ003"
# LimeSurvey Field type: A
data[, 186] <- as.character(data[, 186])
attributes(data)$variable.labels[186] <- "[Dacă aș trece prin probleme emoționale serioase, aș avea încredere că pot apela la un psihoterapeut.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 186] <- factor(data[, 186], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[186] <- "G06Q13_SQ004"
# LimeSurvey Field type: A
data[, 187] <- as.character(data[, 187])
attributes(data)$variable.labels[187] <- "[Este ceva de admirat la o persoană care trece prin conflicte și frici fără să apeleze la ajutor specializat.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 187] <- factor(data[, 187], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[187] <- "G06Q13_SQ005"
# LimeSurvey Field type: A
data[, 188] <- as.character(data[, 188])
attributes(data)$variable.labels[188] <- "[Aș vrea să primesc ajutor psihologic dac aș fi supărat/ă sau îngrijorat/ă pentru o perioadă îndelungată.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 188] <- factor(data[, 188], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[188] <- "G06Q13_SQ006"
# LimeSurvey Field type: A
data[, 189] <- as.character(data[, 189])
attributes(data)$variable.labels[189] <- "[S-ar putea să îmi doresc consiliere psihologică în viitor.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 189] <- factor(data[, 189], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[189] <- "G06Q13_SQ007"
# LimeSurvey Field type: A
data[, 190] <- as.character(data[, 190])
attributes(data)$variable.labels[190] <- "[E puțin probabil ca o persoană să rezolve o problemă emoțonală de una singură; e mai probabil să o rezolve cu ajutor specializat.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 190] <- factor(data[, 190], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[190] <- "G06Q13_Epuinprobabilcaopers"
# LimeSurvey Field type: A
data[, 191] <- as.character(data[, 191])
attributes(data)$variable.labels[191] <- "[Având în vedere timpul și costurile necesare pentru psihoterapie, nu consider că ar fi de valoare pentru mine.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 191] <- factor(data[, 191], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[191] <- "G06Q13_SQ009"
# LimeSurvey Field type: A
data[, 192] <- as.character(data[, 192])
attributes(data)$variable.labels[192] <- "[O persoană trebuie să își rezolve singură problemele; consilierea pihologică trebuie să fie ultima variantă la care apelează.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 192] <- factor(data[, 192], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[192] <- "G06Q13_Opersoantrebuiesirez"
# LimeSurvey Field type: A
data[, 193] <- as.character(data[, 193])
attributes(data)$variable.labels[193] <- "[Problemele emoționale și personale tind să se rezolve de la sine.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele."
data[, 193] <- factor(data[, 193], levels=c("AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Acord parțial", "Acord total"))
names(data)[193] <- "G06Q13_SQ011"
# LimeSurvey Field type: A
data[, 194] <- as.character(data[, 194])
attributes(data)$variable.labels[194] <- "[Cât de des ai fost agresat/ă fizic la școală (de ex., ai fost lovit/ă sau împins/ă în mod repetat, sau rănit/ă fizic)?] Următoarele întrebări fac referire la experiențele tale cu bullyingul. Termenul “bullying” se referă la momentele în care cineva rănește sau sperie intenționat o altă persoană, iar persoanei bullied / agresate îi este greu să se apere. În majoritatea cazurilor, bullyingul  se repetă iar și iar. Având explicația aceasta în minte, cât de des ai fost bullied/agresat,-ă în fiecare dintre următoarele moduri? "
data[, 194] <- factor(data[, 194], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Des", "Uneori", "Rar", "Niciodată"))
names(data)[194] <- "G07Q15_SQ001"
# LimeSurvey Field type: A
data[, 195] <- as.character(data[, 195])
attributes(data)$variable.labels[195] <- "[Cât de des ai fost agresat/ă verbal la școală (de ex., tachinat/ă, poreclit/ă)?] Următoarele întrebări fac referire la experiențele tale cu bullyingul. Termenul “bullying” se referă la momentele în care cineva rănește sau sperie intenționat o altă persoană, iar persoanei bullied / agresate îi este greu să se apere. În majoritatea cazurilor, bullyingul  se repetă iar și iar. Având explicația aceasta în minte, cât de des ai fost bullied/agresat,-ă în fiecare dintre următoarele moduri? "
data[, 195] <- factor(data[, 195], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Des", "Uneori", "Rar", "Niciodată"))
names(data)[195] <- "G07Q15_SQ002"
# LimeSurvey Field type: A
data[, 196] <- as.character(data[, 196])
attributes(data)$variable.labels[196] <- "[Cât de des ai fost agresat/ă la școală de către cineva care te-a ignorat în mod intenționat, te-a exclus sau a răspândit zvonuri despre tine?] Următoarele întrebări fac referire la experiențele tale cu bullyingul. Termenul “bullying” se referă la momentele în care cineva rănește sau sperie intenționat o altă persoană, iar persoanei bullied / agresate îi este greu să se apere. În majoritatea cazurilor, bullyingul  se repetă iar și iar. Având explicația aceasta în minte, cât de des ai fost bullied/agresat,-ă în fiecare dintre următoarele moduri? "
data[, 196] <- factor(data[, 196], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Des", "Uneori", "Rar", "Niciodată"))
names(data)[196] <- "G07Q15_SQ003"
# LimeSurvey Field type: A
data[, 197] <- as.character(data[, 197])
attributes(data)$variable.labels[197] <- "[Cât de des ai fost agresat/ă pe internet (de ex., pe Facebook, Twitter, Instagram) sau prin mesaje text?] Următoarele întrebări fac referire la experiențele tale cu bullyingul. Termenul “bullying” se referă la momentele în care cineva rănește sau sperie intenționat o altă persoană, iar persoanei bullied / agresate îi este greu să se apere. În majoritatea cazurilor, bullyingul  se repetă iar și iar. Având explicația aceasta în minte, cât de des ai fost bullied/agresat,-ă în fiecare dintre următoarele moduri? "
data[, 197] <- factor(data[, 197], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Des", "Uneori", "Rar", "Niciodată"))
names(data)[197] <- "G07Q15_SQ004"
# LimeSurvey Field type: A
data[, 198] <- as.character(data[, 198])
attributes(data)$variable.labels[198] <- "[Cât de des ai fost implicat/ă într-o relație romantică în care partenerul/a te-a lovit sau rănit repetat?] Următoarele întrebări fac referire la experiențele tale cu bullyingul. Termenul “bullying” se referă la momentele în care cineva rănește sau sperie intenționat o altă persoană, iar persoanei bullied / agresate îi este greu să se apere. În majoritatea cazurilor, bullyingul  se repetă iar și iar. Având explicația aceasta în minte, cât de des ai fost bullied/agresat,-ă în fiecare dintre următoarele moduri? "
data[, 198] <- factor(data[, 198], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Des", "Uneori", "Rar", "Niciodată"))
names(data)[198] <- "G07Q15_SQ005"
# LimeSurvey Field type: A
data[, 199] <- as.character(data[, 199])
attributes(data)$variable.labels[199] <- "[Cât de des ai fost implicat/ă în relații romantice în care partenerul/a te-a jignit sau insultat?] Următoarele întrebări fac referire la experiențele tale cu bullyingul. Termenul “bullying” se referă la momentele în care cineva rănește sau sperie intenționat o altă persoană, iar persoanei bullied / agresate îi este greu să se apere. În majoritatea cazurilor, bullyingul  se repetă iar și iar. Având explicația aceasta în minte, cât de des ai fost bullied/agresat,-ă în fiecare dintre următoarele moduri? "
data[, 199] <- factor(data[, 199], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Foarte des", "Des", "Uneori", "Rar", "Niciodată"))
names(data)[199] <- "G07Q15_SQ006"
# LimeSurvey Field type: F
data[, 200] <- as.numeric(data[, 200])
attributes(data)$variable.labels[200] <- "[Indică pe slider numărul care se potrivește] În ultimele 3 luni, de câte ori ai simțit că îți pierzi controlul ȘI ai mâncat, de asemenea, ceea ce majoritatea oamenilor ar considera o cantitate neobișnuit de mare de mâncare la un moment dat?"
names(data)[200] <- "G08Q20_SQ001"
# LimeSurvey Field type: F
data[, 201] <- as.numeric(data[, 201])
attributes(data)$variable.labels[201] <- "[Ai vomitat] În ultimele 3 luni, de câte ori ai făcut următoarele lucruri pentru a-ți controla greutatea sau forma fizică? (inserează un număr pentru fiecare)"
names(data)[201] <- "G08Q36_SQ001"
# LimeSurvey Field type: F
data[, 202] <- as.numeric(data[, 202])
attributes(data)$variable.labels[202] <- "[Ai folosit diuretice sau laxative ] În ultimele 3 luni, de câte ori ai făcut următoarele lucruri pentru a-ți controla greutatea sau forma fizică? (inserează un număr pentru fiecare)"
names(data)[202] <- "G08Q36_SQ002"
# LimeSurvey Field type: F
data[, 203] <- as.numeric(data[, 203])
attributes(data)$variable.labels[203] <- "[Ai făcut exerciții fizice excesiv (te-ai forțat foarte tare să respecți un anumit program de exerciții în orice situație, inclusiv dacă erai accidentat/ă sau bolnav/ă sau dacă asta însemna să ratezi ore sau alte activități importante; te-ai simțit obligat/ă să faci exerciții)] În ultimele 3 luni, de câte ori ai făcut următoarele lucruri pentru a-ți controla greutatea sau forma fizică? (inserează un număr pentru fiecare)"
names(data)[203] <- "G08Q36_SQ003"
# LimeSurvey Field type: F
data[, 204] <- as.numeric(data[, 204])
attributes(data)$variable.labels[204] <- "[Ai postit (nemâncat în mod intenționat timp de cel puțin 24 de ore pentru a preveni îngrășarea sau a pierde în greutate)] În ultimele 3 luni, de câte ori ai făcut următoarele lucruri pentru a-ți controla greutatea sau forma fizică? (inserează un număr pentru fiecare)"
names(data)[204] <- "G08Q36_SQ004"
# LimeSurvey Field type: F
data[, 205] <- as.numeric(data[, 205])
attributes(data)$variable.labels[205] <- "[Ai folosit suplimente de slăbit fără a-ți fi solicitat de către un medic] În ultimele 3 luni, de câte ori ai făcut următoarele lucruri pentru a-ți controla greutatea sau forma fizică? (inserează un număr pentru fiecare)"
names(data)[205] <- "G08Q36_SQ005"
# LimeSurvey Field type: A
data[, 206] <- as.character(data[, 206])
attributes(data)$variable.labels[206] <- "[] Cât de mult simți că te îngrijorezi în legătură cu greutatea ta și forma corpului tău comparativ cu alte persoane de vârsta ta?"
data[, 206] <- factor(data[, 206], levels=c("AO02","AO03","AO04","AO05","AO06"),labels=c("Mult mai puțin decât alte persoane", "Mai puțin decât alte persoane", "Cam la fel ca alte persoane", "Mai mult decât alte persoaane", "Mult mai mult decât alte persoane"))
names(data)[206] <- "G08Q15_SQ001"
# LimeSurvey Field type: A
data[, 207] <- as.character(data[, 207])
attributes(data)$variable.labels[207] <- "[] Cât de frică îți este că te-ai putea îngrășa 1 kilogram?"
data[, 207] <- factor(data[, 207], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc frică", "Puțin frică", "Frică moderată", "Foarte frică", "Extrem de frică"))
names(data)[207] <- "G08Q16_SQ001"
# LimeSurvey Field type: A
data[, 208] <- as.character(data[, 208])
attributes(data)$variable.labels[208] <- "Comparativ cu alte lucruri din viața ta, cât de importantă este greutatea ta pentru tine?"
data[, 208] <- factor(data[, 208], levels=c("AO01","AO02","AO03","AO04"),labels=c("Comparativ cu alte lucruri din viața mea, greutatea mea nu este importantă", "Greutatea mea este puțin mai importantă decât unele lucruri din viața mea", "Greutatea mea este mai importantă decât multe, dar nu toate lucrurile din viața mea", "Greutatea mea este cel mai important lucru din viața mea"))
names(data)[208] <- "G08Q18"
# LimeSurvey Field type: A
data[, 209] <- as.character(data[, 209])
attributes(data)$variable.labels[209] <- "[] Când ai ținut ultima dată dietă?"
data[, 209] <- factor(data[, 209], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("Nu am fost niciodată la dietă", "Acum un an", "Acum 6 luni", "Acum 3 luni", "Acum o lună", "Acum mai puțin de o lună", "Sunt la dietă acum"))
names(data)[209] <- "G08Q17_SQ001"
# LimeSurvey Field type: A
data[, 210] <- as.character(data[, 210])
attributes(data)$variable.labels[210] <- "[] Te simți vreodată gras/ă?"
data[, 210] <- factor(data[, 210], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Niciodată", "Rar", "Uneori", "Deseori", "Întotdeauna"))
names(data)[210] <- "G08Q19_SQ001"
# LimeSurvey Field type: F
data[, 211] <- as.numeric(data[, 211])
attributes(data)$variable.labels[211] <- "[Mănânci mai repede decât în mod normal?] În timpul acestor episoade în care simți că îți pierzi controlul și mănânci o cantitate mai mare de mâncare decât în mod obișnuit:   (bifează toate răspunsurile care ți se aplică)"
data[, 211] <- factor(data[, 211], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[211] <- "G08Q21_SQ001"
# LimeSurvey Field type: F
data[, 212] <- as.numeric(data[, 212])
attributes(data)$variable.labels[212] <- "[Mănânci până când te simți inconfortabil de plin/ă?] În timpul acestor episoade în care simți că îți pierzi controlul și mănânci o cantitate mai mare de mâncare decât în mod obișnuit:   (bifează toate răspunsurile care ți se aplică)"
data[, 212] <- factor(data[, 212], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[212] <- "G08Q21_SQ002"
# LimeSurvey Field type: F
data[, 213] <- as.numeric(data[, 213])
attributes(data)$variable.labels[213] <- "[Mănânci cantități mari de mâncare deși nu îți este fizic foame?] În timpul acestor episoade în care simți că îți pierzi controlul și mănânci o cantitate mai mare de mâncare decât în mod obișnuit:   (bifează toate răspunsurile care ți se aplică)"
data[, 213] <- factor(data[, 213], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[213] <- "G08Q21_SQ003"
# LimeSurvey Field type: F
data[, 214] <- as.numeric(data[, 214])
attributes(data)$variable.labels[214] <- "[Mănânci singur/ă pentru că îți este rușine de cât de mult mănânci?] În timpul acestor episoade în care simți că îți pierzi controlul și mănânci o cantitate mai mare de mâncare decât în mod obișnuit:   (bifează toate răspunsurile care ți se aplică)"
data[, 214] <- factor(data[, 214], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[214] <- "G08Q21_SQ004"
# LimeSurvey Field type: F
data[, 215] <- as.numeric(data[, 215])
attributes(data)$variable.labels[215] <- "[Te simți dezgustat/ă, deprimat/ă sau extrem de vinovat/ă după?] În timpul acestor episoade în care simți că îți pierzi controlul și mănânci o cantitate mai mare de mâncare decât în mod obișnuit:   (bifează toate răspunsurile care ți se aplică)"
data[, 215] <- factor(data[, 215], levels=c(1,0),labels=c("Da", "Nu este selectat"))
names(data)[215] <- "G08Q21_SQ005"
# LimeSurvey Field type: A
data[, 216] <- as.character(data[, 216])
attributes(data)$variable.labels[216] <- "Cât de deranjat/ă sau de supărat/ă te-ai simțit în legatură cu aceste episoade?"
data[, 216] <- factor(data[, 216], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Deloc", "Puțin", "Moderat", "Foarte", "Extrem"))
names(data)[216] <- "G08Q35"
# LimeSurvey Field type: F
data[, 217] <- as.numeric(data[, 217])
attributes(data)$variable.labels[217] <- "Consumi în mod regulat o cantitate redusă de mâncare (sub 1200 calorii/zi) pentru a-ți influența greutatea sau forma fizică?"
data[, 217] <- factor(data[, 217], levels=c(1,2),labels=c("Da", "Nu"))
names(data)[217] <- "G08Q37"
# LimeSurvey Field type: A
data[, 218] <- as.character(data[, 218])
attributes(data)$variable.labels[218] <- "Urmezi tratament pentru o tulburare de alimentație?"
data[, 218] <- factor(data[, 218], levels=c("AO01","AO02","AO03"),labels=c("Da", "Nu", "Nu în prezent, dar am urmat în trecut"))
names(data)[218] <- "G08Q38"
# LimeSurvey Field type: F
data[, 219] <- as.numeric(data[, 219])
attributes(data)$variable.labels[219] <- "[Înălțime (în cm)]"
names(data)[219] <- "G08Q40_SQ001"
# LimeSurvey Field type: F
data[, 220] <- as.numeric(data[, 220])
attributes(data)$variable.labels[220] <- "[Greutatea cea mai redusă în ultimul an (în kg)]"
names(data)[220] <- "G08Q40_SQ002"
# LimeSurvey Field type: F
data[, 221] <- as.numeric(data[, 221])
attributes(data)$variable.labels[221] <- "[Greutatea curentă (în kg)]"
names(data)[221] <- "G08Q40_SQ003"
# LimeSurvey Field type: A
data[, 222] <- as.character(data[, 222])
attributes(data)$variable.labels[222] <- "[Primesc foarte multă înțelegere și siguranță de la alții.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură acestea ți se potrivesc sau nu. "
data[, 222] <- factor(data[, 222], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nu mi se potrivește deloc", "2", "3", "4", "5 - Mi se potrivește extrem de bine"))
names(data)[222] <- "G12Q45_SQ001"
# LimeSurvey Field type: A
data[, 223] <- as.character(data[, 223])
attributes(data)$variable.labels[223] <- "[Am o persoană foarte apropiată, pe ajutorul căreia pot conta mereu.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură acestea ți se potrivesc sau nu. "
data[, 223] <- factor(data[, 223], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nu mi se potrivește deloc", "2", "3", "4", "5 - Mi se potrivește extrem de bine"))
names(data)[223] <- "G12Q45_SQ002"
# LimeSurvey Field type: A
data[, 224] <- as.character(data[, 224])
attributes(data)$variable.labels[224] <- "[Dacă am nevoie, pot împrunuta ceva de la prieteni sau vecini fără nicio problemă.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură acestea ți se potrivesc sau nu. "
data[, 224] <- factor(data[, 224], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nu mi se potrivește deloc", "2", "3", "4", "5 - Mi se potrivește extrem de bine"))
names(data)[224] <- "G12Q45_SQ003"
# LimeSurvey Field type: A
data[, 225] <- as.character(data[, 225])
attributes(data)$variable.labels[225] <- "[Am mai multe persoane alături de care îmi place să fac lucruri/ să îmi petrec timpul.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură acestea ți se potrivesc sau nu. "
data[, 225] <- factor(data[, 225], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nu mi se potrivește deloc", "2", "3", "4", "5 - Mi se potrivește extrem de bine"))
names(data)[225] <- "G12Q45_SQ004"
# LimeSurvey Field type: A
data[, 226] <- as.character(data[, 226])
attributes(data)$variable.labels[226] <- "[Când sunt bolnav/ă, pot apela fără ezitare la prieteni sau rude să se ocupe de lucrurile importante pentru mine.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură acestea ți se potrivesc sau nu. "
data[, 226] <- factor(data[, 226], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nu mi se potrivește deloc", "2", "3", "4", "5 - Mi se potrivește extrem de bine"))
names(data)[226] <- "G12Q45_SQ005"
# LimeSurvey Field type: A
data[, 227] <- as.character(data[, 227])
attributes(data)$variable.labels[227] <- "[Dacă sunt foarte deprimat/ă, știu la cine pot apela.] Citește cu atenție urmăroarele propoziții și exprimă în ce măsură acestea ți se potrivesc sau nu. "
data[, 227] <- factor(data[, 227], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("1 - Nu mi se potrivește deloc", "2", "3", "4", "5 - Mi se potrivește extrem de bine"))
names(data)[227] <- "G12Q45_SQ006"
# LimeSurvey Field type: A
data[, 228] <- as.character(data[, 228])
attributes(data)$variable.labels[228] <- "[Majoritatea oamenilor consideră că adolescenții  cu probleme emoționale și de comportament sunt la fel de inteligenți ca toți ceilalți.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 228] <- factor(data[, 228], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[228] <- "G13Q46_SQ001"
# LimeSurvey Field type: A
data[, 229] <- as.character(data[, 229])
attributes(data)$variable.labels[229] <- "[Majoritatea oamenilor se uită de sus la  adolescenții care apelează la un psiholog pentru probleme emoționale sau de comportament.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 229] <- factor(data[, 229], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[229] <- "G13Q46_SQ002"
# LimeSurvey Field type: A
data[, 230] <- as.character(data[, 230])
attributes(data)$variable.labels[230] <- "[Celor mai mulți adolescenți le-ar plăcea să petreacă timp cu adolescenți cu probleme emoționale sau de comportament.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 230] <- factor(data[, 230], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[230] <- "G13Q46_SQ003"
# LimeSurvey Field type: A
data[, 231] <- as.character(data[, 231])
attributes(data)$variable.labels[231] <- "[Majoritatea oamenilor cred că adolescenții cu probleme emoționale sau de comportament sunt periculoși.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 231] <- factor(data[, 231], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[231] <- "G13Q46_SQ004"
# LimeSurvey Field type: A
data[, 232] <- as.character(data[, 232])
attributes(data)$variable.labels[232] <- "[Majoritatea oamenilor cred că adolescenții cu probleme emoționale sau de comportament nu sunt la fel de demni de încredere ca ceilalți.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 232] <- factor(data[, 232], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[232] <- "G13Q46_SQ005"
# LimeSurvey Field type: A
data[, 233] <- as.character(data[, 233])
attributes(data)$variable.labels[233] <- "[Majoritatea oamenilor cred că adolescenții cu probleme emoționale sau de comportament sunt de vină pentru problemele lor.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 233] <- factor(data[, 233], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[233] <- "G13Q46_SQ006"
# LimeSurvey Field type: A
data[, 234] <- as.character(data[, 234])
attributes(data)$variable.labels[234] <- "[Majoritatea oamenilor cred că adolescenții cu probleme emoționale sau de comportament se vor face bine într-o zi.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 234] <- factor(data[, 234], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[234] <- "G13Q46_SQ007"
# LimeSurvey Field type: A
data[, 235] <- as.character(data[, 235])
attributes(data)$variable.labels[235] <- "[Majoritatea angajatorilor cred că e o idee proastă să ofere o slujbă cu jumătate de normă unui adolescent cu probleme emoționale sau de comportament.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 235] <- factor(data[, 235], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[235] <- "G13Q46_SQ008"
# LimeSurvey Field type: A
data[, 236] <- as.character(data[, 236])
attributes(data)$variable.labels[236] <- "[Majoritatea oamenilor cred că adolescenții cu probleme emoționale sau de comportament pot obține note bune la școală.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 236] <- factor(data[, 236], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[236] <- "G13Q46_SQ009"
# LimeSurvey Field type: A
data[, 237] <- as.character(data[, 237])
attributes(data)$variable.labels[237] <- "[Profesorii cred că elevii cu probleme emoționale sau de comportament nu se comportă la fel de bine ca ceilalți la ore.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 237] <- factor(data[, 237], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[237] <- "G13Q46_SQ010"
# LimeSurvey Field type: A
data[, 238] <- as.character(data[, 238])
attributes(data)$variable.labels[238] <- "[Majoritatea oamenilor cred că adolescenții cu probleme emoționale sau de comportament nu pot avea grijă de ei înșiși la fel de bine ca ceilalți.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 238] <- factor(data[, 238], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[238] <- "G13Q46_SQ011"
# LimeSurvey Field type: A
data[, 239] <- as.character(data[, 239])
attributes(data)$variable.labels[239] <- "[Majoritatea oamenilor se tem de adolescenții care merg la psiholog pentru că au probleme emoționale sau de comportament.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 239] <- factor(data[, 239], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[239] <- "G13Q46_SQ012"
# LimeSurvey Field type: A
data[, 240] <- as.character(data[, 240])
attributes(data)$variable.labels[240] <- "[Cred că adolescenții cu probleme emoționale sau de comportament sunt la fel de inteligenți ca ceilalți.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 240] <- factor(data[, 240], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[240] <- "G13Q46_SQ013"
# LimeSurvey Field type: A
data[, 241] <- as.character(data[, 241])
attributes(data)$variable.labels[241] <- "[M-aș uita de sus la un adolescent dacă aș ști că merge la psiholog pentru probleme emoționale sau de comportament.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 241] <- factor(data[, 241], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[241] <- "G13Q46_SQ014"
# LimeSurvey Field type: A
data[, 242] <- as.character(data[, 242])
attributes(data)$variable.labels[242] <- "[Mi-ar plăcea să petrec timp cu cineva care are probleme emoționale sau de comportament.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 242] <- factor(data[, 242], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[242] <- "G13Q46_SQ015"
# LimeSurvey Field type: A
data[, 243] <- as.character(data[, 243])
attributes(data)$variable.labels[243] <- "[Cred că adolescenții cu probleme emoționale sau de comportament sunt periculoși.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 243] <- factor(data[, 243], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[243] <- "G13Q46_SQ016"
# LimeSurvey Field type: A
data[, 244] <- as.character(data[, 244])
attributes(data)$variable.labels[244] <- "[Cred că adolescenții cu probleme emoționale sau de comportament nu sunt la fel de demni de încredere ca ceilalți.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 244] <- factor(data[, 244], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[244] <- "G13Q46_SQ017"
# LimeSurvey Field type: A
data[, 245] <- as.character(data[, 245])
attributes(data)$variable.labels[245] <- "[Cred că adolescenții cu probleme emoționale sau de comportament sunt de vină pentru problemele lor.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 245] <- factor(data[, 245], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[245] <- "G13Q46_SQ018"
# LimeSurvey Field type: A
data[, 246] <- as.character(data[, 246])
attributes(data)$variable.labels[246] <- "[Cred că adolescenții cu probleme emoționale sau de comportament se vor face bine într-o zi.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 246] <- factor(data[, 246], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[246] <- "G13Q46_SQ019"
# LimeSurvey Field type: A
data[, 247] <- as.character(data[, 247])
attributes(data)$variable.labels[247] <- "[Cred că e o idee proastă ca angajatorii să ofere o slujbă cu jumătate de normă unui adolescent cu probleme emoționale sau de comportament.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 247] <- factor(data[, 247], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[247] <- "G13Q46_SQ020"
# LimeSurvey Field type: A
data[, 248] <- as.character(data[, 248])
attributes(data)$variable.labels[248] <- "[Cred că adolescenții cu probleme emoționale sau de comportament pot obține note bune la școală.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 248] <- factor(data[, 248], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[248] <- "G13Q46_SQ021"
# LimeSurvey Field type: A
data[, 249] <- as.character(data[, 249])
attributes(data)$variable.labels[249] <- "[Cred că adolescenții cu probleme emoționale sau de comportament nu se comportă la fel de bine ca ceilalți la ore.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 249] <- factor(data[, 249], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[249] <- "G13Q46_SQ022"
# LimeSurvey Field type: A
data[, 250] <- as.character(data[, 250])
attributes(data)$variable.labels[250] <- "[Cred că adolescenții cu probleme emoționale sau de comportament nu pot avea grijă de ei înșiși la fel de bine ca ceilalți.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 250] <- factor(data[, 250], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[250] <- "G13Q46_SQ023"
# LimeSurvey Field type: A
data[, 251] <- as.character(data[, 251])
attributes(data)$variable.labels[251] <- "[Mi-ar fi teamă de cineva dacă aș ști că are probleme emoționale sau de comportament.] Citește cu atenție următoarele propoziții și exprimă în ce măsură ești de acord sau nu cu ele. "
data[, 251] <- factor(data[, 251], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Dezacord total", "Dezacord parțial", "Neutru", "Acord parțial", "Acord total"))
names(data)[251] <- "G13Q46_SQ024"
# LimeSurvey Field type: A
data[, 252] <- as.character(data[, 252])
attributes(data)$variable.labels[252] <- "[Cât de des ai folosit Social Media în ultimele 15 minute înainte de a merge la somn?] Te rugam să te gândești la modul în care ai folosit Social Media (Instagram, TikTok, Facebook, etc) în ultima săptămână și răspunde la urmăroarele întrebări.   "
data[, 252] <- factor(data[, 252], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09"),labels=c("Nici măcar o zi", "O zi", "Două zile", "Trei zile", "Patru zile", "Cinci zile", "Șase zile", "În fiecare zi"))
names(data)[252] <- "G15Q51_SQ002"
# LimeSurvey Field type: A
data[, 253] <- as.character(data[, 253])
attributes(data)$variable.labels[253] <- "[Cât de des ai folosit Social Media în primele 15 minute dupa ce te-ai trezit?] Te rugam să te gândești la modul în care ai folosit Social Media (Instagram, TikTok, Facebook, etc) în ultima săptămână și răspunde la urmăroarele întrebări.   "
data[, 253] <- factor(data[, 253], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09"),labels=c("Nici măcar o zi", "O zi", "Două zile", "Trei zile", "Patru zile", "Cinci zile", "Șase zile", "În fiecare zi"))
names(data)[253] <- "G15Q51_SQ003"
# LimeSurvey Field type: A
data[, 254] <- as.character(data[, 254])
attributes(data)$variable.labels[254] <- "[Cât de des ai folosit Social Media în timpul micului dejun?] Te rugam să te gândești la modul în care ai folosit Social Media (Instagram, TikTok, Facebook, etc) în ultima săptămână și răspunde la urmăroarele întrebări.   "
data[, 254] <- factor(data[, 254], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09"),labels=c("Nici măcar o zi", "O zi", "Două zile", "Trei zile", "Patru zile", "Cinci zile", "Șase zile", "În fiecare zi"))
names(data)[254] <- "G15Q51_SQ004"
# LimeSurvey Field type: A
data[, 255] <- as.character(data[, 255])
attributes(data)$variable.labels[255] <- "[Cât de des ai folosit Social Media în timpul prânzului?] Te rugam să te gândești la modul în care ai folosit Social Media (Instagram, TikTok, Facebook, etc) în ultima săptămână și răspunde la urmăroarele întrebări.   "
data[, 255] <- factor(data[, 255], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09"),labels=c("Nici măcar o zi", "O zi", "Două zile", "Trei zile", "Patru zile", "Cinci zile", "Șase zile", "În fiecare zi"))
names(data)[255] <- "G15Q51_SQ005"
# LimeSurvey Field type: A
data[, 256] <- as.character(data[, 256])
attributes(data)$variable.labels[256] <- "[Cât de des ai folosit Social Media în timpul cinei?] Te rugam să te gândești la modul în care ai folosit Social Media (Instagram, TikTok, Facebook, etc) în ultima săptămână și răspunde la urmăroarele întrebări.   "
data[, 256] <- factor(data[, 256], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09"),labels=c("Nici măcar o zi", "O zi", "Două zile", "Trei zile", "Patru zile", "Cinci zile", "Șase zile", "În fiecare zi"))
names(data)[256] <- "G15Q51_SQ006"
# LimeSurvey Field type: A
data[, 257] <- as.character(data[, 257])
attributes(data)$variable.labels[257] <- "Câte ore pe zi petreci, de obicei, utilizând Social Media în timpul unei zile de școală (înainte, în timpul și după școală)?"
data[, 257] <- factor(data[, 257], levels=c("AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14"),labels=c("AO02", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12+"))
names(data)[257] <- "G16Q53"
# LimeSurvey Field type: A
data[, 258] <- as.character(data[, 258])
attributes(data)$variable.labels[258] <- "Câte ore pe zi petreci, de obicei, utilizând Social Media în timpul unei zile când nu mergi la școală (în weekend, în vacanțe)?"
data[, 258] <- factor(data[, 258], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08","AO09","AO10","AO11","AO12","AO13","AO14"),labels=c("AO01", "AO02", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12+"))
names(data)[258] <- "G16Q54"
# LimeSurvey Field type: A
data[, 259] <- as.character(data[, 259])
attributes(data)$variable.labels[259] <- "[Mă simt plictisit dacă nu pot utiliza Social Media.]"
data[, 259] <- factor(data[, 259], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[259] <- "G16Q52_SQ002"
# LimeSurvey Field type: A
data[, 260] <- as.character(data[, 260])
attributes(data)$variable.labels[260] <- "[Mă simt îngrijorat când nu știu ce spun prietenii mei pe Social Media. ]"
data[, 260] <- factor(data[, 260], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[260] <- "G16Q52_SQ003"
# LimeSurvey Field type: A
data[, 261] <- as.character(data[, 261])
attributes(data)$variable.labels[261] <- "[Sunt morocănos dacă nu pot utiliza Social Media.]"
data[, 261] <- factor(data[, 261], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[261] <- "G16Q52_SQ004"
# LimeSurvey Field type: A
data[, 262] <- as.character(data[, 262])
attributes(data)$variable.labels[262] <- "[Sfârșesc să petrec mai mult timp pe Social Media decât plănuiesc inițial. ]"
data[, 262] <- factor(data[, 262], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[262] <- "G16Q52_SQ005"
# LimeSurvey Field type: A
data[, 263] <- as.character(data[, 263])
attributes(data)$variable.labels[263] <- "[De îndată ce ies de pe Social Media, simt dorința imediată să intru din nou.]"
data[, 263] <- factor(data[, 263], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[263] <- "G16Q52_SQ006"
# LimeSurvey Field type: A
data[, 264] <- as.character(data[, 264])
attributes(data)$variable.labels[264] <- "[Arunc un ochi pe Social Media chiar și atunci când vorbesc cu cineva. ]"
data[, 264] <- factor(data[, 264], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[264] <- "G16Q52_SQ007"
# LimeSurvey Field type: A
data[, 265] <- as.character(data[, 265])
attributes(data)$variable.labels[265] <- "[Navighez pe Social Media atunci când mănânc.]"
data[, 265] <- factor(data[, 265], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[265] <- "G16Q52_SQ008"
# LimeSurvey Field type: A
data[, 266] <- as.character(data[, 266])
attributes(data)$variable.labels[266] <- "[Arunc un ochi pe Social Media chiar și atunci când sunt la ore. ]"
data[, 266] <- factor(data[, 266], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[266] <- "G16Q52_SQ009"
# LimeSurvey Field type: A
data[, 267] <- as.character(data[, 267])
attributes(data)$variable.labels[267] <- "[Navighez pe Social Media atunci când merg pe stradă. ]"
data[, 267] <- factor(data[, 267], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[267] <- "G16Q52_SQ010"
# LimeSurvey Field type: A
data[, 268] <- as.character(data[, 268])
attributes(data)$variable.labels[268] <- "[Navighez pe Social Media atunci când sunt în pat, înainte de culcare. ]"
data[, 268] <- factor(data[, 268], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("0 - Niciodată", "1", "2", "3", "4", "5", "6 - Foarte des"))
names(data)[268] <- "G16Q52_SQ011"
# LimeSurvey Field type: A
data[, 269] <- as.character(data[, 269])
attributes(data)$variable.labels[269] <- "Dacă te confrunți cu gânduri suicidare, te îndemnăm să vorbești cu un adult (un părinte/tutore, un profesor, consilierul școlar ș.a.m.d) să te ajute să obții ajutor de specialitate.   În caz de criză, apelează o linie de urgență, precum 112 (Serviciul de Urgență), 0800 801 200 (TelVerde Antisuicid), 0800 080 100 sau 116 123 (Centrul pentru Prevenirea Suicidului la Copii și Adolescenți)."
data[, 269] <- factor(data[, 269], levels=c("AO01"),labels=c("Am înțeles"))
names(data)[269] <- "G17Q65"
