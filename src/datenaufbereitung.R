#install.packages("readxl")   
#install.packages("sf")
library(sf)
library(readxl)
###################################################################################
Initiative = read.csv(
  "data/10Mio.csv",
  sep = ";"
)
Initiative = Initiative[
  grepl(
    "Keine 10-Millionen-Schweiz",
    Initiative$proposal_name,
    fixed = TRUE
  ),
]
# 1. Ja-Anteil berechnen in Prozent
Initiative$yes_share = 100* Initiative$yes_votes_count /
  (Initiative$yes_votes_count + Initiative$no_votes_count)
# 2. Nur Gemeinden behalten
Initiative = Initiative[Initiative$region_type == "municipality", ]
# 3. Nur benC6tigte Spalten auswC$hlen
Initiative = Initiative[, c(
  "region_id",
  "region_name",
  "yes_share",
  "parent_canton_name"
)]
Initiative$region_id = as.integer(Initiative$region_id)
Initiative$region_name = as.character(Initiative$region_name)
Initiative$yes_share = as.numeric(Initiative$yes_share)
Initiative$parent_canton_name = as.character(
  Initiative$parent_canton_name)
#####################################################################################
Bauzonen = read_excel(
  "data/bauzonenstatistik_schweiz_2022_gemeinden.xlsx",
  sheet = "Statistik_Hauptnutzung"
)
Bauzonen = Bauzonen[, c(
  "Gem_No",
  "Name",
  "Total"
)]
colnames(Bauzonen) = c(
  "bfs_id",
  "municipality",
  "Total"
)
Bauzonen$Total[is.na(Bauzonen$Total)] = 0
##############################################################
Bevoelkerung = read_excel(
  "data/Bevoelkerung.xlsx"
)
Bevoelkerung = Bevoelkerung[, c(1, 5, 8)]

colnames(Bevoelkerung) = c(
  "gemeinde_raw",
  "CH",
  "Auslaender"
)
Bevoelkerung = Bevoelkerung[
  grepl("^\\.\\.\\.\\.\\.\\.", Bevoelkerung[[1]]),
]
Bevoelkerung$bfs_id = sub(
  "^\\.\\.\\.\\.\\.\\.([0-9]{4}).*$",
  "\\1",
  Bevoelkerung[[1]]
)

Bevoelkerung$municipality = sub(
  "^\\.\\.\\.\\.\\.\\.[0-9]{4} ",
  "",
  Bevoelkerung[[1]]
)
Bevoelkerung$gemeinde_raw=NULL
Bevoelkerung$CH = as.numeric(Bevoelkerung$CH)
Bevoelkerung$Auslaender = as.numeric(Bevoelkerung$Auslaender)
Bevoelkerung$bfs_id = as.integer(Bevoelkerung$bfs_id)
Bevoelkerung$municipality = as.character(Bevoelkerung$municipality)
# gibt ein NA wo eine null sein sollte
Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 715
] = 0
####################################################################################
# Weil wir verschiedene Anzahlen an Zeilen haben vor dem Join C<berprC<fen
bevoelkerung_nicht_in_initiative = Bevoelkerung[
  Bevoelkerung$bfs_id %in%
    setdiff(Bevoelkerung$bfs_id, Initiative$region_id),
  c("bfs_id", "municipality")
]
initiative_nicht_in_bevoelkerung = Initiative[
  Initiative$region_id %in%
    setdiff(Initiative$region_id, Bevoelkerung$bfs_id),
  c("region_id", "region_name")
]

initiative_nicht_in_bevoelkerung
bevoelkerung_nicht_in_initiative

# Bevoelkerung ins richtige Format bringen, also Fusionierte Gemeinden nachtragen

# Harmonisierung der BevC6lkerungsdaten mit den AuszC$hlungseinheiten
# und BFS-IDs der Abstimmungsdaten:

# Moutier:
# Wechselte per 01.01.2026 vom Kanton Bern zum Kanton Jura.
# Alte BFS-ID 700 wird deshalb durch die neue BFS-ID 6831 ersetzt.

# FC)tigny-MC)niC(res:
# Neue Gemeinde aus der Fusion von FC)tigny (BFS 2016)
# und MC)niC(res (BFS 2027).
# Die BevC6lkerungswerte beider Gemeinden werden addiert
# und der neuen BFS-ID 2056 zugeordnet.

# Meienried:
# EigenstC$ndige Gemeinde, wird in den Abstimmungsdaten jedoch
# gemeinsam mit BC<ren an der Aare ausgezC$hlt.
# Meienried (BFS 389) wird deshalb zu BC<ren an der Aare (BFS 383) addiert.

# Hellsau:
# EigenstC$ndige Gemeinde, wird gemeinsam mit HC6chstetten ausgezC$hlt.
# Hellsau (BFS 408) wird zu HC6chstetten (BFS 410) addiert.

# Deisswil bei MC<nchenbuchsee:
# EigenstC$ndige Gemeinde, wird gemeinsam mit Wiggiswil ausgezC$hlt.
# Deisswil bei MC<nchenbuchsee (BFS 535) wird zu Wiggiswil (BFS 553) addiert.

# OberhC<nigen:
# EigenstC$ndige Gemeinde, wird gemeinsam mit ZC$ziwil ausgezC$hlt.
# OberhC<nigen (BFS 629) wird zu ZC$ziwil (BFS 628) addiert.

# Niedermuhlern:
# EigenstC$ndige Gemeinde, wird gemeinsam mit Wald (BE) ausgezC$hlt.
# Niedermuhlern (BFS 877) wird zu Wald (BE) (BFS 888) addiert.

# Ulmiz:
# Fusionierte mit Gurmels.
# Ulmiz (BFS 2278) wird zu Gurmels (BFS 2262) addiert.

# Halten und Oekingen:
# Beide Gemeinden gehC6ren neu zu Kriegstetten.
# Halten (BFS 2520) und Oekingen (BFS 2529)
# werden zu Kriegstetten (BFS 2525) addiert.

# Villnachern:
# Fusionierte mit Brugg.
# Villnachern (BFS 4122) wird zu Brugg (BFS 4095) addiert.

# Wichtig:
# Absolute Werte wie CH-BevC6lkerung und auslC$ndische BevC6lkerung
# werden addiert. Der AuslC$nderanteil wird erst danach neu berechnet.



Bevoelkerung$bfs_id[
  Bevoelkerung$bfs_id == 700
] = 6831

Bevoelkerung$municipality[
  Bevoelkerung$bfs_id == 6831
] = "Moutier"

Bevoelkerung = rbind(
  Bevoelkerung,
  data.frame(
    bfs_id = 2056,
    municipality = "FC)tigny-MC)niC(res",
    CH =
      Bevoelkerung$CH[Bevoelkerung$bfs_id == 2016] +
      Bevoelkerung$CH[Bevoelkerung$bfs_id == 2027],
    Auslaender =
      Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 2016] +
      Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 2027]
  )
)
Bevoelkerung = Bevoelkerung[
  !(Bevoelkerung$bfs_id %in% c(2016, 2027)),
]

Bevoelkerung$CH[
  Bevoelkerung$bfs_id == 383
] =
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 383] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 389]

Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 383
] =
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 383] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 389]

Bevoelkerung = Bevoelkerung[
  Bevoelkerung$bfs_id != 389,
]

Bevoelkerung$CH[
  Bevoelkerung$bfs_id == 410
] =
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 410] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 408]

Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 410
] =
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 410] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 408]

Bevoelkerung = Bevoelkerung[
  Bevoelkerung$bfs_id != 408,
]

Bevoelkerung$CH[
  Bevoelkerung$bfs_id == 553
] =
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 553] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 535]

Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 553
] =
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 553] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 535]

Bevoelkerung = Bevoelkerung[
  Bevoelkerung$bfs_id != 535,
]


Bevoelkerung$CH[
  Bevoelkerung$bfs_id == 628
] =
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 628] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 629]

Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 628
] =
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 628] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 629]

Bevoelkerung = Bevoelkerung[
  Bevoelkerung$bfs_id != 629,
]


Bevoelkerung$CH[
  Bevoelkerung$bfs_id == 888
] =
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 888] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 877]

Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 888
] =
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 888] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 877]

Bevoelkerung = Bevoelkerung[
  Bevoelkerung$bfs_id != 877,
]

Bevoelkerung$CH[
  Bevoelkerung$bfs_id == 2262
] =
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 2262] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 2278]

Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 2262
] =
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 2262] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 2278]

Bevoelkerung = Bevoelkerung[
  Bevoelkerung$bfs_id != 2278,
]
Bevoelkerung$CH[
  Bevoelkerung$bfs_id == 2525
] =
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 2525] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 2520] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 2529]

Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 2525
] =
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 2525] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 2520] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 2529]

Bevoelkerung = Bevoelkerung[
  !(Bevoelkerung$bfs_id %in% c(2520, 2529)),
]
Bevoelkerung$CH[
  Bevoelkerung$bfs_id == 4095
] =
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 4095] +
  Bevoelkerung$CH[Bevoelkerung$bfs_id == 4122]

Bevoelkerung$Auslaender[
  Bevoelkerung$bfs_id == 4095
] =
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 4095] +
  Bevoelkerung$Auslaender[Bevoelkerung$bfs_id == 4122]

Bevoelkerung = Bevoelkerung[
  Bevoelkerung$bfs_id != 4122,
]
#Zielvariablen berechen
Bevoelkerung$Bevoelkerung =
  Bevoelkerung$CH + Bevoelkerung$Auslaender

Bevoelkerung$Auslaenderanteil = 100*
  Bevoelkerung$Auslaender / Bevoelkerung$Bevoelkerung
#BenC6tigte Spalten in Initiative DF C<bertrage. 
Initiative = merge(
  Initiative,
  Bevoelkerung[, c(
    "bfs_id",
    "Bevoelkerung",
    "Auslaenderanteil"
  )],
  by.x = "region_id",
  by.y = "bfs_id",
  all.x = TRUE
)
###############################################################
#Bauzonen df ins richtige Format bringen

# Moutier: alte BFS-ID durch neue BFS-ID ersetzen
Bauzonen$bfs_id[Bauzonen$bfs_id == 700] = 6831
Bauzonen$municipality[Bauzonen$bfs_id == 6831] = "Moutier"
# FC)tigny + MC)niC(res = FC)tigny-MC)niC(res
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 2056,
    municipality = "FC)tigny-MC)niC(res",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 2016] +
      Bauzonen$Total[Bauzonen$bfs_id == 2027]
  )
)

Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(2016, 2027)),
]
# Meienried zu BC<ren an der Aare
Bauzonen$Total[Bauzonen$bfs_id == 383] =
  Bauzonen$Total[Bauzonen$bfs_id == 383] +
  Bauzonen$Total[Bauzonen$bfs_id == 389]

Bauzonen = Bauzonen[Bauzonen$bfs_id != 389, ]
# Hellsau zu HC6chstetten
Bauzonen$Total[Bauzonen$bfs_id == 410] =
  Bauzonen$Total[Bauzonen$bfs_id == 410] +
  Bauzonen$Total[Bauzonen$bfs_id == 408]

Bauzonen = Bauzonen[Bauzonen$bfs_id != 408, ]
# Deisswil bei MC<nchenbuchsee zu Wiggiswil
Bauzonen$Total[Bauzonen$bfs_id == 553] =
  Bauzonen$Total[Bauzonen$bfs_id == 553] +
  Bauzonen$Total[Bauzonen$bfs_id == 535]

Bauzonen = Bauzonen[Bauzonen$bfs_id != 535, ]
# OberhC<nigen zu ZC$ziwil
Bauzonen$Total[Bauzonen$bfs_id == 628] =
  Bauzonen$Total[Bauzonen$bfs_id == 628] +
  Bauzonen$Total[Bauzonen$bfs_id == 629]
# Niedermuhlern zu Wald (BE) addieren
Bauzonen$Total[Bauzonen$bfs_id == 888] =
  Bauzonen$Total[Bauzonen$bfs_id == 888] +
  Bauzonen$Total[Bauzonen$bfs_id == 877]

Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 877,
]
Bauzonen = Bauzonen[Bauzonen$bfs_id != 629, ]
# Ulmiz zu Gurmels
Bauzonen$Total[Bauzonen$bfs_id == 2262] =
  Bauzonen$Total[Bauzonen$bfs_id == 2262] +
  Bauzonen$Total[Bauzonen$bfs_id == 2278]

Bauzonen = Bauzonen[Bauzonen$bfs_id != 2278, ]
# Halten und Oekingen zu Kriegstetten
Bauzonen$Total[Bauzonen$bfs_id == 2525] =
  Bauzonen$Total[Bauzonen$bfs_id == 2525] +
  Bauzonen$Total[Bauzonen$bfs_id == 2520] +
  Bauzonen$Total[Bauzonen$bfs_id == 2529]

Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(2520, 2529)),
]
# Villnachern zu Brugg
Bauzonen$Total[Bauzonen$bfs_id == 4095] =
  Bauzonen$Total[Bauzonen$bfs_id == 4095] +
  Bauzonen$Total[Bauzonen$bfs_id == 4122]

Bauzonen = Bauzonen[Bauzonen$bfs_id != 4122, ]

########################
initiative_nicht_in_bauzonen = Initiative[
  Initiative$region_id %in%
    setdiff(Initiative$region_id, Bauzonen$bfs_id),
  c("region_id", "region_name")
]

bauzonen_nicht_in_initiative = Bauzonen[
  Bauzonen$bfs_id %in%
    setdiff(Bauzonen$bfs_id, Initiative$region_id),
  c("bfs_id", "municipality")
]

initiative_nicht_in_bauzonen

print(
  bauzonen_nicht_in_initiative,
  n = Inf
)
########
# Herznach + Ueken = Herznach-Ueken

Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 4186,
    municipality = "Herznach-Ueken",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 4166] +
      Bauzonen$Total[Bauzonen$bfs_id == 4179]
  )
)

Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(4166, 4179)),
]

# Adlikon + Andelfingen + Humlikon = neue Gemeinde Andelfingen

Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 291,
    municipality = "Andelfingen",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 21] +
      Bauzonen$Total[Bauzonen$bfs_id == 30] +
      Bauzonen$Total[Bauzonen$bfs_id == 32]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(21, 30, 32)),
]

# Diemerswil = MC<nchenbuchsee
Bauzonen$Total[Bauzonen$bfs_id == 546] =
  Bauzonen$Total[Bauzonen$bfs_id == 546] +
  Bauzonen$Total[Bauzonen$bfs_id == 536]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 536,
]


# Zwieselberg = Reutigen
Bauzonen$Total[Bauzonen$bfs_id == 767] =
  Bauzonen$Total[Bauzonen$bfs_id == 767] +
  Bauzonen$Total[Bauzonen$bfs_id == 947]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 947,
]


# Wangenried = Wangen an der Aare
Bauzonen$Total[Bauzonen$bfs_id == 992] =
  Bauzonen$Total[Bauzonen$bfs_id == 992] +
  Bauzonen$Total[Bauzonen$bfs_id == 993]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 993,
]


# Honau = Rheinau
Bauzonen$Total[Bauzonen$bfs_id == 38] =
  Bauzonen$Total[Bauzonen$bfs_id == 38] +
  Bauzonen$Total[Bauzonen$bfs_id == 1057]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 1057,
]


# Auboranges + Chapelle (GlC"ne) + Ecublens (FR) = Rue
Bauzonen$Total[Bauzonen$bfs_id == 2097] =
  Bauzonen$Total[Bauzonen$bfs_id == 2097] +
  Bauzonen$Total[Bauzonen$bfs_id == 2061] +
  Bauzonen$Total[Bauzonen$bfs_id == 2066] +
  Bauzonen$Total[Bauzonen$bfs_id == 2072]

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(2061, 2066, 2072)),
]


# Montet (GlC"ne) = Ursy
Bauzonen$Total[Bauzonen$bfs_id == 2102] =
  Bauzonen$Total[Bauzonen$bfs_id == 2102] +
  Bauzonen$Total[Bauzonen$bfs_id == 2089]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 2089,
]


# Grolley + Ponthaux = neue Gemeinde Grolley-Ponthaux
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 2239,
    municipality = "Grolley-Ponthaux",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 2200] +
      Bauzonen$Total[Bauzonen$bfs_id == 2217]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(2200, 2217)),
]


# LC<terswil-GC$chliwil = Buchegg
Bauzonen$Total[Bauzonen$bfs_id == 2465] =
  Bauzonen$Total[Bauzonen$bfs_id == 2465] +
  Bauzonen$Total[Bauzonen$bfs_id == 2456]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 2456,
]


# RC<te + Schwende = neue Gemeinde Schwende-RC<te
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 3112,
    municipality = "Schwende-RC<te",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 3103] +
      Bauzonen$Total[Bauzonen$bfs_id == 3105]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(3103, 3105)),
]


# Hemberg + Oberhelfenschwil + Neckertal = neue Gemeinde Neckertal
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 3396,
    municipality = "Neckertal",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 3372] +
      Bauzonen$Total[Bauzonen$bfs_id == 3375] +
      Bauzonen$Total[Bauzonen$bfs_id == 3378]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(3372, 3375, 3378)),
]


# Tschiertschen-Praden = Chur
Bauzonen$Total[Bauzonen$bfs_id == 3901] =
  Bauzonen$Total[Bauzonen$bfs_id == 3901] +
  Bauzonen$Total[Bauzonen$bfs_id == 3932]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 3932,
]


# Turgi = Baden
Bauzonen$Total[Bauzonen$bfs_id == 4021] =
  Bauzonen$Total[Bauzonen$bfs_id == 4021] +
  Bauzonen$Total[Bauzonen$bfs_id == 4042]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 4042,
]


# Burg (AG) = Menziken
Bauzonen$Total[Bauzonen$bfs_id == 4139] =
  Bauzonen$Total[Bauzonen$bfs_id == 4139] +
  Bauzonen$Total[Bauzonen$bfs_id == 4133]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 4133,
]


# Bodio = Giornico
Bauzonen$Total[Bauzonen$bfs_id == 5073] =
  Bauzonen$Total[Bauzonen$bfs_id == 5073] +
  Bauzonen$Total[Bauzonen$bfs_id == 5064]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 5064,
]


# Prato (Leventina) = Quinto
Bauzonen$Total[Bauzonen$bfs_id == 5079] =
  Bauzonen$Total[Bauzonen$bfs_id == 5079] +
  Bauzonen$Total[Bauzonen$bfs_id == 5078]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 5078,
]


# Astano + Bedigliora + Curio + Miglieglia + Novaggio = neue Gemeinde Lema
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 5395,
    municipality = "Lema",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 5146] +
      Bauzonen$Total[Bauzonen$bfs_id == 5149] +
      Bauzonen$Total[Bauzonen$bfs_id == 5181] +
      Bauzonen$Total[Bauzonen$bfs_id == 5200] +
      Bauzonen$Total[Bauzonen$bfs_id == 5207]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(5146, 5149, 5181, 5200, 5207)),
]


# Maroggia + Melano + Rovio = neue Gemeinde Val Mara
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 5240,
    municipality = "Val Mara",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 5195] +
      Bauzonen$Total[Bauzonen$bfs_id == 5197] +
      Bauzonen$Total[Bauzonen$bfs_id == 5219]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(5195, 5197, 5219)),
]


# Enges + Hauterive (NE) + Saint-Blaise + La TC(ne = neue Gemeinde LatC)na
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 6513,
    municipality = "LatC)na",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 6453] +
      Bauzonen$Total[Bauzonen$bfs_id == 6454] +
      Bauzonen$Total[Bauzonen$bfs_id == 6459] +
      Bauzonen$Total[Bauzonen$bfs_id == 6461]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(6453, 6454, 6459, 6461)),
]


# La Chaux-des-Breuleux = Les Breuleux
Bauzonen$Total[Bauzonen$bfs_id == 6743] =
  Bauzonen$Total[Bauzonen$bfs_id == 6743] +
  Bauzonen$Total[Bauzonen$bfs_id == 6744]

# Alte Gemeinde lC6schen
Bauzonen = Bauzonen[
  Bauzonen$bfs_id != 6744,
]


# BeurnevC)sin + Bonfol = neue Gemeinde Basse-Vendline
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 6812,
    municipality = "Basse-Vendline",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 6773] +
      Bauzonen$Total[Bauzonen$bfs_id == 6775]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(6773, 6775)),
]


# Damphreux + Lugnez = neue Gemeinde Damphreux-Lugnez
Bauzonen = rbind(
  Bauzonen,
  data.frame(
    bfs_id = 6811,
    municipality = "Damphreux-Lugnez",
    Total =
      Bauzonen$Total[Bauzonen$bfs_id == 6787] +
      Bauzonen$Total[Bauzonen$bfs_id == 6793]
  )
)

# Alte Gemeinden lC6schen
Bauzonen = Bauzonen[
  !(Bauzonen$bfs_id %in% c(6787, 6793)),
]


# Unterschiede zwischen initiative und bauzonen neu berechnen
initiative_nicht_in_bauzonen = Initiative[
  Initiative$region_id %in%
    setdiff(Initiative$region_id, Bauzonen$bfs_id),
  c("region_id", "region_name")
]

bauzonen_nicht_in_initiative = Bauzonen[
  Bauzonen$bfs_id %in%
    setdiff(Bauzonen$bfs_id, Initiative$region_id),
  c("bfs_id", "municipality")
]


# ergebnisse anzeigen
initiative_nicht_in_bauzonen

print(
  bauzonen_nicht_in_initiative,
  n = Inf
)


# PrC<fen, ob die BFS-IDs weiterhin eindeutig sind
length(Bauzonen$bfs_id) ==
  length(unique(Bauzonen$bfs_id))


colnames(Bauzonen)[
  colnames(Bauzonen) == "Total"
] = "bauzone_total"
Initiative = merge(
  Initiative,
  Bauzonen[, c("bfs_id", "bauzone_total")],
  by.x = "region_id",
  by.y = "bfs_id",
  all.x = TRUE
)
Initiative[
  is.na(Initiative$bauzone_total),
  c("region_id", "region_name")
]
############################################################################
Initiative$Siedlungsdruck =
  Initiative$Bevoelkerung / Initiative$bauzone_total

Initiative$Siedlungsdruck =
  Initiative$Bevoelkerung / Initiative$bauzone_total

Initiative$Siedlungsdruck[
  Initiative$bauzone_total == 0
] = NA

############################################################################
Initiative$parent_canton_name =
  as.factor(Initiative$parent_canton_name)
##########################################################################
Staedte_geo = st_read(
  "data/Boundaries_K4_STADTE2020_20260101_de.gpkg"
)

Staedte = st_drop_geometry(Staedte_geo)
Staedte = Staedte_geo[, c(
  "GDENR",
  "GDENAME",
  "STADTE2020"
)]

Staedte = st_drop_geometry(Staedte)

colnames(Staedte) = c(
  "region_id",
  "municipality",
  "Stadt"
)

Staedte$region_id = as.integer(Staedte$region_id)
Staedte$municipality = as.character(Staedte$municipality)
Staedte$Stadt = as.numeric(Staedte$Stadt)

# Nur statistische StC$dte auswC$hlen
Staedte_1 = Staedte[
  Staedte$Stadt == 1,
]

# PrC<fen, welche Stadt-IDs nicht in Initiative vorkommen
setdiff(
  Staedte_1$region_id,
  Initiative$region_id
)
#Dummy fC<r Stadt erstellen
Initiative$Stadt = ifelse(
  Initiative$region_id %in% Staedte_1$region_id,
  1,
  0
)
Initiative$Stadt = factor(
  Initiative$Stadt,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
####################################


Analyse = Initiative[, c(
  "region_name",
  "yes_share",
  "Auslaenderanteil",
  "Siedlungsdruck",
  "Stadt",
  "parent_canton_name",
  "Bevoelkerung"
)]

colnames(Analyse) = c(
  "Ortschaft",
  "Ja_Anteil",
  "Auslaenderanteil",
  "Siedlungsdruck",
  "Stadt",
  "Kanton",
  "Bevoelkerung"
)


rm(
  list = setdiff(
    ls(),
    "Analyse"
  )
)

#############################################################################
