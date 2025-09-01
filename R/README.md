**Habitattypen buiten Natura 2000**

Stageproject, Provincie Zeeland, Radboud Universiteit
16/01/2025 - 29/08/2025
Niels van Hof

(Zet voor de leesbaarheid van dit bestand "Automatische terugloop" aan, bovenaan in de balk onder "Opmaak")

Voor meer informatie over de achtergrond, methoden, resultaten en discussie van dit project raad ik aan om mijn verslag te lezen. Bij vragen kun je terecht bij Pieter Lievense: pcj.lievense@zeeland.nl of Marion Pross: m.pross@zeeland.nl.

*Over het project*

Dit project komt voort uit de Natuurherstelverordening, die stelt dat habitattypen die in slechte staat zijn hersteld moet worden. In Zeeland en veel andere provincies bestaan er weinig habitattypekaarten buiten Natura 2000, die wel nodig zijn om de doelen in de Natuurherstelverordening te behalen. Om het karteren van habitattypen in de natuurgebieden buiten Natura 2000 gebieden zo efficiënt mogelijk te maken, was het doel van dit project om de kans op voorkomen van ieder terrestrisch habitattype in de Provincie Zeeland te voorspellen. Dit project kan gezien worden als een pionieronderzoek, om te laten zien wat er mogelijk is in het voorspellen van habitattypen door middel van bestaande data. De resultaten zijn heel veelbelovend, de gemiddelde nauwkeurigheid van de modellen is 85%. Deze methode kan, met wat aanpassingen, ook door andere provincies gebruikt worden om het voorkomen van habitattypen te voorspellen en zo op een efficiëntere manier te voldoen aan de Natuurherstelverordening.

*Globale methode*

De voorspelling van habitattypen is gebaseerd op 4 variabelen:
- pH (zuurtegraad in de grond)
- Grondwaterdiepte (GVG)
- Zoutgehalte in grondwater
- Voorkomen van karakteristieke soorten
Eerst wordt per variabele de kans op voorkomen van elk habitattype benaderd aan de hand van de bekende eisen voor ieder habitattype. Vervolgens worden deze "suitability scores" samen met de bekende habitattypeverspreiding gebruikt om een Random Forest model te trainen dat de kans op voorkomen van ieder habitattype voorspelt. Het resultaat is een geopackage met hokjes op 250m resolutie voor ieder habitattype, met een kans op voorkomen van het betreffende habitattype tussen de 0 en 1 (0 = kleinste kans op voorkomen, 1 = grootste kans op voorkomen).

*Gebruik van de scripts*

De scripts zijn gesorteerd op volgorde van hoe ze gerund moeten worden. Om de scripts te runnen is een recente versie van R en RStudio (script 1, 2, 3, 5, 6, 7, 8, 9 en 10) en Python (script 4) vereist. Aan het begin van elk script staan de benodigde R packages, die geïnstalleerd dienen te worden alvorens het script gerund kan worden. Ook staat aan het begin van elk script kort beschreven wat het doet. Voor intern gebruik in de Provincie Zeeland zijn alle bronnen te vinden in het mapje "sources", en zijn alle scripts direct te runnen omdat de bronnen meteen kunnen worden ingeladen. Voor extern gebruik moeten eerst de bijbehorende bronnen verzameld worden, alvorens de scripts gerund kunnen worden. Dit omdat sommige bronnen beschermd eigendom van de Provincie Zeeland zijn. De meeste bronnen zijn echter wel online te vinden.

*Korte beschrijving van de scripts*

**1_pH_suitability.R**

Dit laadt een kaart van gemeten pH (Helfenstein et al., 2024) en eisen voor de pH voor elk habitattype (Runhaar et al., 2009) in en combineert deze om een kaart met geschiktheidsscores voor elk habitattype gebaseerd op pH te maken. De output is een .gpkg bestand voor elk habitattype met de geschiktheidsscores op basis van pH op een resolutie van 25m.

**2_Estimating_GVG_based_on_GHG.R**

Dit laadt een kaart van gemeten GHG (gemiddelde hoogste grondwaterstand) en GVG (gemiddelde voorjaarsgrondwaterstand) in en voorspelt met een lineair model de waardes van GVG op basis van GHG. De output is een .tif bestand met verwachte GVG.

**3_Groundwater_depth_suitability.R**

Dit laadt de kaart met verwachte GVG en eisen voor de GVG voor elk habitattype (Runhaar et al., 2009) in en combineert deze om een kaart met geschiktheidsscores voor elk habitattype gebaseerd op GVG te maken. De output is een .gpkg bestand voor elk habitattype met de geschiktheidsscores op basis van GVG op een resolutie van 50m.

**4_ahn_chloride_klassen.ipynb**

Dit laadt kaarten in van zoutgehaltes in het grondwater op verschillende dieptecategorieën ten opzichte van NAP (Delsman et al., 2018), en vertaalt deze aan de hand van een hoogtekaart naar dieptecategorieën ten opzichte van het maaiveld. De output zijn .tif bestanden met de zoutconcentratie op dieptes van 0 tot 300cm, in stappen van 50cm, op een resolutie van 50m.

**5_Mean_salinity_0-250m.R**

Dit laadt de kaarten met de zoutconcentratie ten opzichte van het maaiveld in en controleert met een lineair model het verschil tussen de lagen. Dan wordt het gemiddelde genomen van de bovenste 5 lagen (0-250cm). De output is een kaart met gemiddelde zoutconcentratie op een diepte van 0 tot 250cm, op een resolutie van 50m.

**6_Groundwater_salinity_suitability.R**

Dit laadt de kaart met gemiddelde zoutconcentratie op een diepte van 0 tot 250cm en eisen voor de pH voor elk habitattype (Runhaar et al., 2009) in en combineert deze om een kaart met geschiktheidsscores voor elk habitattype gebaseerd op zoutconcentratie in het grondwater te maken. De output is een .gpkg bestand voor elk habitattype met de geschiktheidsscores op basis van zoutconcentratie in de grond op een resolutie van 50m.

**7_Characteristic_species_suitability.R**

Dit laadt observaties van plant- en diersoorten uit de NDFF en een lijst van karakteristieke soorten van elk habitattype (Bijlsma et al., in press; Nijssen et al., in press) in, en combineert deze om een kaart met geschiktheidsscores voor elk habitattype gebaseerd op het voorkomen van karakteristieke soorten te maken. De output is een .gpkg bestand voor elk habitattype met de geschiktheidsscores op basis van karakteristieke soorten op een resolutie van 250m.

**8_Natura_2000_goals.R**

Dit laadt de doelstellingen van elk Natura 2000 gebied in Zeeland in, van de website natura2000.nl. De output is een Excel bestand met alle doelstellingen voor alle Natura 2000 gebieden in zeeland.

**9_Random_forest.R**

Dit laadt de kaarten met geschiktheidsscores voor elk habitattype voor de 4 variabelen (pH, GVG, zout en karakteristieke soorten) en de huidige habitattypekaarten in, en berekent eerst de gemiddelde geschiktheid voor pH, GVG en zout in het grondwater op een 250m resolutie. Hierna worden voor elk habitattype de 4 geschiktheidsscores en de huidige habitattypeverspreiding gebruikt om per habitattype een Random Forest model te fitten om de kans op voorkomen van elk habitattype in Zeeland te voorspellen. De output is een .gpkg bestand voor elk habitattype met de geschiktheidsscores op basis van de Random Forest, en .txt bestanden met statistiekwaardes voor elk model op basis van validatie.

**10_Report_graphs.R**

Dit laadt de statistieken van de Random Forests voor elk habitattype in en maakt grafieken voor veel van deze statistieken. De figuren werden in het verslag gebruikt.

**Literatuur:**

Bijlsma, R. J., Weeda, E. J., Janssen, J. A. M., & Sparrius, L. B. (in press). Karakteristieke soorten flora en funga voor de beoordeling van structuur & functie van habitattypen Natura 2000 (WOT-technical report 270). Wettelijke onderzoekstaken Natuur & Milieu.

Delsman, J. R., van Baaren, E. S., Siemon, B., Dabekaussen, W., Karaoulis, M. C., Pauw, P. S., Vermaas, T., Bootsma, H., de Louw, P. G. B., Gunnink, J. L., Dubelaar, C. W., Menkovic, A., Steuer, A., Meyer, U., Revil, A., & Oude Essink, G. H. P. (2018). Large-scale, probabilistic salinity mapping using airborne electromagnetics for groundwater management in Zeeland, the Netherlands. Environmental Research Letters, 13(8), 084011. https://doi.org/10.1088/1748-9326/aad19e

Helfenstein, A., Mulder, V. L., Hack-ten Broeke, M. J. D., van Doorn, M., Teuling, K., Walvoort, D. J. J., & Heuvelink, G. B. M. (2024). BIS-4D: Mapping soil properties and their uncertainties at 25&thinsp;m resolution in the Netherlands. Earth System Science Data, 16(6), Article 6. https://doi.org/10.5194/essd-16-2941-2024

Nijssen, M. E., van Riel, M., van Grunsven, R., Wallis de Vries, M., Kleukers, R., Creemers, R., & Bijlsma, R. J. (in press). Karakteristieke soorten kleine fauna voor de beoordeling van structuur & functie van habitattypen Natura 2000 (WOT-technical report 271). Wettelijke onderzoekstaken Natuur & Milieu.

Runhaar, J., Jalink, M. H., Hunneman, H., Witte, J. P. M., & Hennekens, S. M. (2009). Ecologische vereisten habitattypen (A307144; p. 93). KWR Watercycle Research Institute. https://edepot.wur.nl/4986



