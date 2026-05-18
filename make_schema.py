"""
Build a Word document (.docx) — formal academic Italian register,
addressed to Prof. Keti Lelo as supervisor / co-author.
Target length: 3-4 pages, ~1500 words.
"""
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH

doc = Document()

for section in doc.sections:
    section.top_margin = Cm(2.5)
    section.bottom_margin = Cm(2.5)
    section.left_margin = Cm(2.5)
    section.right_margin = Cm(2.5)

style = doc.styles["Normal"]
style.font.name = "Garamond"
style.font.size = Pt(12)
style.paragraph_format.line_spacing = 1.25
style.paragraph_format.space_after = Pt(4)

def H1(text):
    p = doc.add_heading(text, level=1)
    p.style.font.size = Pt(12)

def P(text, justify=True, bold=False):
    p = doc.add_paragraph()
    p.paragraph_format.first_line_indent = Cm(0.6)
    if justify:
        p.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
    r = p.add_run(text)
    if bold:
        r.bold = True
    return p

def PP(text, justify=True):
    p = doc.add_paragraph()
    if justify:
        p.alignment = WD_ALIGN_PARAGRAPH.JUSTIFY
    p.add_run(text)
    return p

# ====================================================================
# Title
# ====================================================================
title = doc.add_paragraph()
title.alignment = WD_ALIGN_PARAGRAPH.CENTER
r = title.add_run(
    "Estensione empirica del paper «Driving the change. "
    "The socio-economic impact of Autostrada del Sole, 1950–1990»"
)
r.bold = True
r.font.size = Pt(13)

sub = doc.add_paragraph()
sub.alignment = WD_ALIGN_PARAGRAPH.CENTER
r = sub.add_run("Proposta di analisi quasi-sperimentale e nota di fattibilità")
r.italic = True
r.font.size = Pt(11)

doc.add_paragraph()

# ====================================================================
# 1. Inquadramento
# ====================================================================
H1("1. Inquadramento")

PP("Gentile Professoressa Lelo,")
P(
    "la presente nota descrive l'impostazione, lo stato di "
    "avanzamento e le prospettive di un'estensione empirica del "
    "Suo paper, sviluppata a complemento dell'analisi descrittiva "
    "e dell'inquadramento storico-istituzionale già contenuti nel "
    "Suo working paper. L'estensione non costituisce un volume "
    "separato bensì la sezione dati-metodo-risultati-robustezza "
    "dello stesso paper, di cui il Suo testo fornisce introduzione, "
    "contesto storico-economico e rassegna della letteratura: il "
    "deliverable previsto è dunque un unico paper integrato. La "
    "motivazione del lavoro risiede nell'osservazione, esplicitata "
    "a pag. 17 della Sua bozza, secondo cui la comparazione "
    "descrittiva fra comuni con e senza casello — pur indicativa — "
    "non consente di stabilire un nesso causale, e nel Suo "
    "esplicito richiamo a future analisi basate su «approcci "
    "econometrici avanzati, quali modelli panel o strategie di "
    "identificazione quasi-sperimentali»."
)

# ====================================================================
# 2. Domanda di ricerca
# ====================================================================
H1("2. Domanda di ricerca")

P(
    "La domanda principale del paper rimane quella formulata nella "
    "Sua bozza: in che misura l'Autostrada del Sole ha influenzato "
    "lo sviluppo socio-economico dei comuni serviti dal casello? "
    "L'estensione empirica articola il quesito in quattro "
    "sottoquesiti. Il primo riguarda l'effetto causale medio sui "
    "comuni trattati, una volta depurata la selezione su "
    "caratteristiche pre-trattamento osservabili. Il secondo "
    "riguarda la dimensione spaziale: a quale distanza dal casello "
    "lo spillover si esaurisce, e dove inizia un controllo "
    "credibilmente non contaminato. Il terzo introduce "
    "un'angolatura analitica originale: l'A1 ha raggiunto le aree "
    "interne nel senso della classificazione SNAI 2014 (categorie "
    "D-Intermedio, E-Periferico, F-Ultraperiferico)? Dove le ha "
    "raggiunte, ha innescato convergenza? Dove non le ha raggiunte, "
    "ha contribuito a consolidare il divario che la Strategia "
    "Nazionale Aree Interne tenta oggi di colmare? Il quarto "
    "quesito riguarda l'eterogeneità settoriale e permette di "
    "qualificare l'effetto generale in termini di trasformazione "
    "strutturale del tessuto produttivo."
)

# ====================================================================
# 3. Impostazione metodologica
# ====================================================================
H1("3. Impostazione metodologica")

P(
    "La strategia di identificazione si articola in quattro stadi "
    "sovrapposti, applicati al panel decennale ISTAT 1951–1991 dei "
    "3.242 comuni nelle otto regioni del corridoio A1. Il primo "
    "stadio consiste in un propensity score matching, stimato "
    "tramite logit sulle sole variabili pre-trattamento 1951 di "
    "natura non outcome — composizione familiare, istruzione "
    "(quota di analfabeti e di diplomati/laureati) e variabili "
    "abitative o di densità — seguito da abbinamento 1:3 nearest-"
    "neighbour con replacement. Vengono deliberatamente escluse "
    "dal modello tutte le variabili di outcome e le quote "
    "settoriali di occupazione, suscettibili di indurre post-"
    "treatment bias."
)
P(
    "Il secondo stadio applica l'estimatore di Callaway e "
    "Sant'Anna (2021) per difference-in-differences con timing "
    "scaglionato. La granularità decennale del censimento ISTAT "
    "colloca i quattro anni di apertura dei caselli A1 (1959, "
    "1960, 1962, 1964) in due distinte coorti censuarie: la "
    "coorte A (caselli aperti nel 1959-60, n = 19, primo "
    "censimento post-trattamento il 1961) e la coorte B (caselli "
    "aperti nel 1962-64, n = 34, primo censimento post-trattamento "
    "il 1971). Tale articolazione consente l'identificazione "
    "staggered: nello stesso anno calendario le due coorti si "
    "trovano in posizioni diverse dell'event time, e il confronto "
    "con il gruppo never-treated produce un ATT(g, t) "
    "cohort-time-specifico, aggregato in seguito ad effetti per "
    "tempo di esposizione. Il placebo a e = −2 verifica "
    "l'assunzione di parallel trends."
)
P(
    "Il terzo stadio recupera la dimensione spaziale dell'analisi "
    "secondo l'impostazione di Ciani e de Blasio (2022). I comuni "
    "vengono ripartiti in sei anelli concentrici sulla base del "
    "tempo di guida in minuti dal casello A1 più vicino: R0 host, "
    "R1 0-15 minuti, R2 15-30, R3 30-45, R4 45-60, R5 oltre i 60 "
    "minuti come riferimento. Anziché escludere via donut i comuni "
    "esposti a spillover, l'impostazione attribuisce a ciascun "
    "anello un coefficiente esplicito, di modo da misurare la "
    "fascia di spillover anziché presupporla. Il quarto stadio "
    "applica infine la decomposizione spillover-robust introdotta "
    "da Butts (2023): un test walk-inward identifica il primo "
    "anello in cui il coefficiente di anello cessa di essere "
    "statisticamente distinto da zero, definendo empiricamente il "
    "confine dello spillover; l'effetto complessivo viene "
    "decomposto in una componente diretta sui comuni host e in "
    "una componente indiretta sui comuni entro il confine, somma "
    "che costituisce l'effetto cumulato per casello aperto — la "
    "quantità rilevante dal punto di vista delle policy."
)

# ====================================================================
# 4. Posizionamento
# ====================================================================
H1("4. Posizionamento rispetto alla letteratura")

P(
    "La rassegna presente nella Sua bozza copre in modo "
    "esauriente i contributi teorici sull'accessibilità (Hansen "
    "1959, 1965; Aschauer 1989; Gramlich 1994; Banister e "
    "Berechman 2001), gli studi empirici sull'effetto delle "
    "autostrade in contesto comparato (Baum-Snow 2007; Duranton e "
    "Turner 2012; Garcia-López et al. 2015; Herranz-Loncán et al. "
    "2023) e la letteratura italiana sull'A3 (de Blasio, Poy e "
    "Ciani 2020) e su altre tratte (Percoco 2016; Cascetta et al. "
    "2020). L'estensione empirica qui proposta dialoga con quei "
    "contributi e se ne distacca in modo specifico, come "
    "sintetizzato nella tabella seguente."
)

table = doc.add_table(rows=1, cols=3)
table.style = "Light Grid Accent 1"
hdr = table.rows[0].cells
hdr[0].text = "Riferimento"
hdr[1].text = "Loro impostazione"
hdr[2].text = "Nostro distacco"
for cell in hdr:
    for p in cell.paragraphs:
        for r in p.runs:
            r.bold = True

rows = [
    ("Percoco (2016, JEG)",
     "Strumento basato sulla rete stradale romana; analisi "
     "cross-sezionale a livello nazionale.",
     "Strumento differente — il Piano Romita 1955 (Gazzetta "
     "Ufficiale n. 131 dell'8 giugno 1955) — temporalmente "
     "prossimo al trattamento e legato direttamente "
     "all'intenzione di pianificazione. Identificazione panel su "
     "40 anni; strumentazione anche del timing di apertura e "
     "della distanza continua."),

    ("de Blasio, Poy e Ciani (2020)",
     "DiD sull'A3 Salerno–Reggio Calabria, singola coorte di "
     "apertura, orizzonte 10-15 anni.",
     "Studio dell'A1, autostrada fondativa della rete italiana, "
     "con due coorti scaglionate e orizzonte 1951-1991."),

    ("Ciani e de Blasio (2022, JEG)",
     "Anelli di distanza con esclusione donut della fascia "
     "potenzialmente contaminata.",
     "Anelli con coefficienti espliciti, decomposizione di Butts "
     "(2023) e integrazione con disegno staggered."),

    ("Donaldson (2018), Faber (2014), Banerjee, Duflo e Qian (2020)",
     "Effetti di lungo periodo di infrastrutture in contesti "
     "coloniali ed emergenti.",
     "Prima applicazione lungo-periodo all'Italia del dopoguerra "
     "su panel ISTAT decennale, con misura continua di "
     "accessibilità interpretata nell'impianto market access di "
     "Donaldson e Hornbeck (2016)."),

    ("Letteratura aree interne (Barca, Casavola e Lucatelli 2014; "
     "Felice 2013, 2019; Trigilia 1994)",
     "Diagnosi e politica delle aree interne come si presentano "
     "oggi, in particolare dopo la SNAI 2014.",
     "Prima evidenza causale che lega le scelte infrastrutturali "
     "del 1955-1964 alla gerarchia di marginalità che la SNAI "
     "2014 oggi affronta. Tale legame costituisce il fulcro "
     "interpretativo del paper."),
]
for ref, theirs, ours in rows:
    row = table.add_row().cells
    row[0].text = ref
    row[1].text = theirs
    row[2].text = ours

doc.add_paragraph()

P(
    "L'innovazione metodologica del paper non risiede nelle "
    "singole tecniche, tutte disponibili nella letteratura "
    "applicata post-2021, bensì nella loro composizione su un "
    "caso storico-istituzionale finora trattato prevalentemente "
    "in chiave narrativa (Menduni 1999; Iori 2014; Maggi 2009). "
    "L'apporto sostantivo riguarda invece l'angolatura policy: "
    "nessuno studio precedente ha verificato in modo causale se "
    "le scelte di tracciato dell'Autostrada del Sole abbiano "
    "contribuito a costituire la gerarchia territoriale che la "
    "SNAI 2014 assume come dato."
)

# ====================================================================
# 5. Stato e risultati preliminari
# ====================================================================
H1("5. Stato di avanzamento e risultati preliminari")

P(
    "Al momento la pipeline di analisi è completamente costruita "
    "e funzionante, articolata in dieci script R interamente "
    "riproducibili, che producono quattordici figure e oltre "
    "venticinque tabelle. La bozza di paper è completa di tutte "
    "le sezioni richieste."
)
P(
    "I risultati principali, considerati come ordini di grandezza, "
    "sono i seguenti. La CS-DiD sul campione donut-30 restituisce "
    "un effetto al primo censimento post-trattamento dell'ordine "
    "di venti punti logaritmici sulla popolazione, quattordici "
    "sui local units e ventisette sugli addetti; gli effetti "
    "crescono fino a e = +2 (circa venti anni dopo l'apertura) e "
    "si stabilizzano in seguito. La decomposizione di Butts "
    "identifica il confine di spillover fra i trenta e i "
    "quarantacinque minuti per local units e addetti e fra i "
    "quarantacinque e i sessanta per la popolazione; la "
    "componente indiretta aggregata per casello è dell'ordine di "
    "dieci volte quella diretta, a illustrazione della rilevanza "
    "del controllo per spillover. Sul versante delle aree interne, "
    "soltanto 15 comuni dei 470 classificati come D, E o F nel "
    "campione tight ospitano un casello: 13 in D-Intermedio, 2 "
    "in E-Periferico (i due valichi appenninici di Castiglione "
    "dei Pepoli e San Benedetto Val di Sambro), nessuno in F-"
    "Ultraperiferico. Dove l'A1 ha raggiunto le aree interne ha "
    "innescato convergenza significativa; la marginalità delle "
    "aree non raggiunte, che oggi costituisce il principale "
    "target della SNAI 2014, risulta compatibile con un'eredità "
    "di scelte infrastrutturali del 1955-1964."
)

# ====================================================================
# 6. Valutazione critica
# ====================================================================
H1("6. Valutazione critica")

P(
    "Per onestà verso il progetto, ritengo opportuno esplicitare "
    "tre limiti che permangono nell'attuale impostazione. Il "
    "primo riguarda la selezione su variabili non osservabili: lo "
    "stadio di propensity score condiziona il confronto su "
    "covariate 1951 di tipo demografico, scolastico e abitativo, "
    "ma non può catturare le determinanti politico-istituzionali "
    "della localizzazione dei caselli — la curva di Fanfani da "
    "Lei documentata, l'inclusione di Cassino, l'arbitraggio "
    "interno IRI per la concessione Bologna-Firenze. La strategia "
    "strumentale basata sul Piano Romita 1955 (sezione 7) "
    "costituisce la via per chiudere questo gap. Il secondo "
    "limite riguarda la numerosità del campione trattato nello "
    "stratum aree interne: 15 comuni, di cui zero in F-"
    "Ultraperiferico, costituiscono una base statistica fragile; "
    "il messaggio policy principale rimane comunque valido in "
    "quanto si fonda sull'esiguità del trattamento, non sulla "
    "magnitudo degli effetti all'interno della categoria. Il "
    "terzo limite concerne l'anacronismo della classificazione "
    "SNAI 2014, costruita sulla rete dei servizi 2011-2013 e "
    "applicata retrospettivamente al periodo 1951-1991: la "
    "classificazione è tuttavia stabile nelle componenti "
    "geomorfologiche sottostanti, e nella stesura del paper "
    "questo punto verrà reso esplicito come ipotesi "
    "identificativa."
)

# ====================================================================
# 7. Sviluppi e impegno richiesto
# ====================================================================
H1("7. Sviluppi successivi e impegno richiesto")

P(
    "Lo stato attuale consente, dopo circa un mese di rifinitura "
    "redazionale, una sottomissione a riviste di campo di buona "
    "fascia internazionale (Regional Studies, Italian Economic "
    "Journal, Rivista di Economia e Statistica del Territorio). "
    "Per puntare a riviste di fascia superiore (Explorations in "
    "Economic History, Journal of Regional Science, "
    "Review of Economics and Statistics nella componente "
    "regionale) ritengo necessari ulteriori due mesi di lavoro, "
    "articolati in quattro voci ben delimitate: la costruzione "
    "dei centroidi comunali a partire dallo shapefile ISTAT "
    "Confini Amministrativi (tre-cinque giornate); la "
    "digitalizzazione del corridoio del Piano Romita 1955 a "
    "partire dalla cartografia del Gazzetta Ufficiale (una "
    "settimana); la stima 2SLS con lo strumento Romita e relative "
    "diagnostiche di first stage (una settimana); il calcolo "
    "degli standard error spaziali alla Conley (tre-quattro "
    "giorni). Una quinta voce, separata, riguarda la "
    "decomposizione settoriale con armonizzazione del codice "
    "ATECO sui censimenti 1971, 1981 e 1991 (tre-quattro "
    "settimane), rispondente al quarto sottoquesito della "
    "sezione 2. Investimenti più consistenti — un modello "
    "strutturale di equilibrio spaziale o un'estensione nazionale "
    "ad A2, A3, A4 e A14 — esulano dall'impegno qui proposto e "
    "costituirebbero, qualora ritenuti opportuni, un secondo "
    "paper di prosecuzione."
)

# ====================================================================
# 8. Conclusione
# ====================================================================
H1("8. Conclusione")

P(
    "Ritengo che il livello di sviluppo attuale dell'analisi "
    "giustifichi la prosecuzione del lavoro, e che l'impegno "
    "aggiuntivo di circa due mesi necessario per portare il paper "
    "a un livello competitivo presso riviste di fascia superiore "
    "sia proporzionato al valore marginale atteso, considerato "
    "che la pipeline analitica è già costruita e che "
    "l'investimento riguarda essenzialmente lavoro di "
    "geocodifica, una strategia strumentale specifica e una "
    "decomposizione settoriale, e non un ripensamento di "
    "fondamentali."
)
PP(
    "Resto a disposizione per qualsiasi chiarimento e La ringrazio "
    "per l'attenzione che vorrà dedicare alla proposta."
)

doc.add_paragraph()
sig = doc.add_paragraph()
sig.alignment = WD_ALIGN_PARAGRAPH.RIGHT
sig.add_run("Con i più cordiali saluti,")
sig2 = doc.add_paragraph()
sig2.alignment = WD_ALIGN_PARAGRAPH.RIGHT
sig2.add_run("[firma]")

doc.save("/home/user/Share/Schema_Lelo_empirical.docx")
print("Saved Schema_Lelo_empirical.docx (academic register, condensed).")
