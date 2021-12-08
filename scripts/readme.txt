lfv_modes_2_knmi

Scriptet lfv_modes_2_knmi.py använder ett API hos LFV fört att komma åt ModeS
data från svenskt luftrum. Dessa datas skickas sedan till EMADDC hos KNMI för
att processas.

Vid anrop till API't hos LFV utan några ingående parametrar fås en response
med de senaste 200 flygrörelserna.

Från denna response sorteras de FlightId'n bort som har en tidstämpel som ligger
före början av det aktuella tidsintervallet. Längd på tidsintervallet sätts i
scriptets variabel INTERVAL_LENGTH.

Ex. körs scriptet 12:03 och INTERVAL_LENGTH = 5, sätts aktuellt intervall
till 11:55-12:00 och alla FlightId'n med tidstämpel tidigare än 11:55
kommer att sorteras bort. (Men FlightId'n med tidsstämpel efter 12:00 kommer att
behållas.)

För alla FlightId'n som inte sorterats bort så anropas API't igen med
parametrarna: id = FlightId och history =  true. API anropen sker en flight i
taget. 

Responsen från alla dessa anrop filtreras igen på tid, där observationer
med tidsstämplar utanför intervallet tas bort. Sedan läggs observationerna till
en lista med samtliga obervationer (för aktuellt tidsintervall). Denna lista
skrivs sedan till en .json fil, efter det att listan sorterats med avseende på
tidsstämplarna.

Filnamnet ska vara på följande format,
SWE_%Y%m%d_%H:%M.json
där Y=år, m=månad, d=dag, H=timme, och M=minut är starten(i UTC tid) på
intervallet, tex SWE_2019091011:55.json.

Slutligen skickas json filen till EMADDC's ftp för inkommande data.

Uppgifter om adresser, användarnamn och lösenord för LFV's API samt FTP'n hos
EMADDC finns i filen lfv_modes.config

Scriptet sparar json filen i mappen data. Även en logfil modes.log sparas för
varje körning av scriptet.

Scriptet är testat med python 2.7 samt python 3.7.
