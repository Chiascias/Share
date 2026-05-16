"""
Build a Word document (.docx) summarising the proposed empirical
analysis for the Autostrada del Sole paper. Intended for a senior
academic who must decide whether to fund the analysis.

Tone: honest, professional, concise. 2-3 pages.
"""
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH

doc = Document()

# Page margins
for section in doc.sections:
    section.top_margin = Cm(2.0)
    section.bottom_margin = Cm(2.0)
    section.left_margin = Cm(2.5)
    section.right_margin = Cm(2.5)

# Default font
style = doc.styles["Normal"]
style.font.name = "Calibri"
style.font.size = Pt(11)

def H1(text):
    p = doc.add_heading(text, level=1)
    p.style.font.size = Pt(14)

def H2(text):
    p = doc.add_heading(text, level=2)
    p.style.font.size = Pt(12)

def P(text, bold=False, italic=False):
    p = doc.add_paragraph()
    r = p.add_run(text)
    r.bold = bold
    r.italic = italic
    return p

def BULLET(text, level=0):
    p = doc.add_paragraph(text, style="List Bullet")
    p.paragraph_format.left_indent = Cm(0.6 + 0.6 * level)
    return p

# ====================================================================
# Title block
# ====================================================================
title = doc.add_paragraph()
title.alignment = WD_ALIGN_PARAGRAPH.CENTER
r = title.add_run("Scheda di valutazione — Analisi empirica")
r.bold = True
r.font.size = Pt(16)

sub = doc.add_paragraph()
sub.alignment = WD_ALIGN_PARAGRAPH.CENTER
r = sub.add_run(
    "Driving the change. The socio-economic impact of "
    "Autostrada del Sole, 1950–1990"
)
r.italic = True
r.font.size = Pt(12)

aut = doc.add_paragraph()
aut.alignment = WD_ALIGN_PARAGRAPH.CENTER
r = aut.add_run("Lelo & Tani — parte empirica del paper ECEHW 2026")
r.font.size = Pt(10)

doc.add_paragraph()  # blank

# ====================================================================
# Inquadramento
# ====================================================================
H1("1. Inquadramento")

P(
    "Questa scheda riassume in modo onesto il progetto di analisi "
    "empirica per il paper Lelo & Tani (2026, ECEHW). L'analisi "
    "qui descritta NON è un volume separato: costituisce la sezione "
    "dati-metodo-risultati-robustness del paper stesso, di cui Lelo "
    "& Tani hanno già scritto introduzione, contesto storico-"
    "istituzionale e literature review. Il deliverable è un paper "
    "unico, integrato."
)

# ====================================================================
# Research question
# ====================================================================
H1("2. Domanda di ricerca")

P(
    "Il paper di Lelo & Tani (2026) chiede: l'Autostrada del Sole "
    "ha causato crescita socio-economica nei comuni serviti dal "
    "casello? In che misura? Per chi? L'autore stesso (p. 17) "
    "dichiara che la sezione descrittiva del draft attuale non "
    "permette di rispondere causalmente alla domanda e chiede "
    "esplicitamente «advanced econometric approaches»."
)

P("L'analisi empirica risponde a quattro sotto-domande:")

BULLET(
    "RQ1. Effetto causale: i 53 comuni con casello A1 hanno avuto "
    "una crescita demografica, imprenditoriale e occupazionale "
    "superiore al controllo, dopo aver controllato per selezione "
    "su variabili osservabili e per spillovers spaziali sui comuni "
    "limitrofi?"
)
BULLET(
    "RQ2. Decadimento spaziale: a quale distanza dal casello "
    "(in minuti di guida) lo spillover svanisce? Dove finisce la "
    "fascia contaminata e inizia il controllo pulito?"
)
BULLET(
    "RQ3. Aree interne (originale): l'A1 ha raggiunto le aree "
    "interne SNAI 2014 (categorie D-Intermedio, E-Periferico, "
    "F-Ultraperiferico)? Dove le ha raggiunte, ha innescato "
    "convergenza? Dove non le ha raggiunte, ha consolidato il "
    "divario di marginalità che la SNAI 2014 sta oggi cercando di "
    "correggere?"
)
BULLET(
    "RQ4. Eterogeneità settoriale: il guadagno è concentrato nel "
    "manifatturiero, nei servizi, nell'agricoltura?"
)

# ====================================================================
# Approccio metodologico
# ====================================================================
H1("3. Approccio metodologico")

P(
    "Quattro stage di identificazione, applicati a un panel "
    "decennale ISTAT 1951-1991 di 3.242 comuni nelle 8 regioni "
    "del corridoio A1."
)

H2("Stage 1 — Propensity score matching")
P(
    "Logit della probabilità di trattamento (K0 = 1) su covariate "
    "1951 NON-outcome: composizione familiare (F1, A1, F1_1, F3_1), "
    "istruzione (I4 analfabeti, SS4 diplomati/laureati), abitazioni/"
    "densità (DensU, Shape_Area). Matching 1:3 nearest-neighbour "
    "con replacement, caliper 0.25. Verifica balance (|SMD| < 0.10 "
    "post-matching)."
)

H2("Stage 2 — Callaway-Sant'Anna staggered DiD")
P(
    "L'A1 si è aperta in 4 anni (1959, 1960, 1962, 1964). La "
    "granularità decennale del censimento le collassa in 2 coorti:"
)
BULLET("Coorte A (n=19, K7 = 1959-60): primo post-censimento 1961")
BULLET("Coorte B (n=34, K7 = 1962-64): primo post-censimento 1971")
P(
    "ATT(g,t) = E[Δy | G=g] − E[Δy | Never], con never-treated "
    "come gruppo di controllo. Aggregazione a ATT(e) ponderata per "
    "dimensione di coorte. Test di parallel-trends al placebo "
    "e = −2 (coorte B nel 1951)."
)

H2("Stage 3 — Spatial component: anelli alla Ciani-de Blasio")
P(
    "Sei anelli concentrici di distanza dal casello più vicino "
    "(W2 in minuti di guida): R0 = host, R1 = 0-15 min (spillover), "
    "R2 = 15-30 min (spillover), R3 = 30-45 min (near control), "
    "R4 = 45-60 min (control), R5 = 60+ min (far reference). "
    "I coefficienti R1 e R2 MISURANO lo spillover (non lo escludono "
    "via donut). Spec parallela continua su W2 (Donaldson-Hornbeck "
    "2016 market access)."
)

H2("Stage 4 — Butts (2023) spillover-robust decomposition")
P(
    "Test del «spillover boundary»: il primo anello in cui β_r è "
    "statisticamente zero. Beyond di questo anello, i comuni "
    "formano il controllo pulito. Decomposizione: effetto diretto "
    "(R0) + effetto indiretto/spillover (somma pesata su R1..R_b) "
    "= effetto totale per casello. È la quantità policy-rilevante."
)

# ====================================================================
# Differenziazione
# ====================================================================
H1("4. Differenziazione dalla letteratura esistente")

P(
    "La literature review è completamente in Lelo & Tani (2026) — "
    "non viene riscritta qui. La tabella seguente sintetizza dove "
    "il presente paper si stacca dai benchmark."
)

# Differentiation table
table = doc.add_table(rows=1, cols=3)
table.style = "Light Grid Accent 1"
hdr = table.rows[0].cells
hdr[0].text = "Benchmark"
hdr[1].text = "Cosa fanno loro"
hdr[2].text = "Cosa aggiungiamo noi"

rows = [
    ("Percoco 2016 (J Econ Geog)",
     "IV con vie romane come strumento per la presenza del casello, "
     "cross-section nazionale 2001.",
     "IV con il Piano Romita 1955 (NON le vie romane): più "
     "vicino temporalmente, esclusione più solida. Panel 40 anni, "
     "instrumentiamo K0 + K7 (timing) + W2 (distanza)."),

    ("de Blasio-Poy-Ciani 2020",
     "DiD su A3 Salerno-Reggio Calabria, una sola apertura, "
     "orizzonte 10-15 anni.",
     "A1 = autostrada FONDATIVA, 2 coorti staggered, orizzonte 40 anni."),

    ("Ciani-de Blasio 2022 (J Econ Geog)",
     "Anelli di distanza con donut, single cohort.",
     "Anelli con spillover ESPLICITO (Butts 2023), CS-DiD su 2 "
     "coorti, PS-matching pre-stage."),

    ("Donaldson 2018, Faber 2014, "
     "Banerjee-Duflo-Qian 2020",
     "Long-run infrastructure su contesti coloniali / Cina rurale.",
     "Prima applicazione long-run all'Italia del dopoguerra con "
     "panel ISTAT granulare."),

    ("Letteratura SNAI / aree interne "
     "(Barca-Casavola-Lucatelli 2014; SVIMEZ)",
     "Diagnosi e politica delle aree interne attuali (post-2014).",
     "Prima evidenza causale che le infrastrutture del dopoguerra "
     "(scelte 1955-1964) hanno contribuito a consolidare la "
     "gerarchia che la SNAI 2014 oggi cerca di correggere."),

    ("Lelo & Tani 2026 (questo paper)",
     "Storia istituzionale + analisi descrittiva 1961-1991.",
     "L'identificazione causale: PS + CS-DiD + anelli + Butts. "
     "Il punto policy aree interne diventa il messaggio principale "
     "del paper."),
]

for benchmark, theirs, ours in rows:
    row = table.add_row().cells
    row[0].text = benchmark
    row[1].text = theirs
    row[2].text = ours

doc.add_paragraph()

# ====================================================================
# Innovazione e limiti
# ====================================================================
H1("5. Valutazione onesta: cosa innova e cosa no")

H2("Cosa innova")
BULLET(
    "Strumento IV nuovo (Piano Romita 1955) — mai usato in letteratura. "
    "Cita direttamente la Fig.1 di Lelo & Tani."
)
BULLET(
    "Prima applicazione di Butts (2023) spillover-robust DiD a un "
    "contesto storico italiano."
)
BULLET(
    "Combinazione PS + CS-DiD + anelli — non rivoluzionaria "
    "metodologicamente (le tre tecniche esistono separatamente) "
    "ma è applied novelty solida."
)
BULLET(
    "Focus aree interne SNAI 2014 — angolo policy originale; "
    "nessuno ha mai chiesto se l'A1 abbia contribuito a CREARE la "
    "gerarchia delle aree interne."
)
BULLET(
    "Panel 40 anni — più lungo della letteratura italiana sull'A1 "
    "(in media 10-15 anni)."
)

H2("Cosa NON innova")
BULLET(
    "Le metodologie singole (CS-DiD, PS, rings) sono off-the-shelf "
    "post-2021."
)
BULLET(
    "Il finding di base («le infrastrutture aiutano le zone "
    "connesse») è coerente con decenni di letteratura, non sovverte "
    "nulla."
)
BULLET(
    "Non c'è modello strutturale o welfare quantification — non "
    "è un paper di frontiera in spatial economics."
)

H2("Limiti onesti")
BULLET(
    "Selection on unobservables: il PS condiziona su variabili "
    "osservabili 1951 (famiglie/istruzione/abitazioni), ma non "
    "cattura le scelte politiche (curva di Fanfani, scelta di "
    "Cassino, ecc.). L'IV Romita 1955 risolve gran parte di questo, "
    "ma richiede 1-2 settimane di GIS work."
)
BULLET(
    "Campione aree interne piccolo: 15 trattati su 470. Zero in "
    "F-Ultraperiferico. Il messaggio policy regge ma è case-based, "
    "non statistical."
)
BULLET(
    "Granularità decennale: le 4 sotto-coorti dell'apertura A1 "
    "(1959/60/62/64) collassano in 2 coorti censuarie. Non risolvibile."
)
BULLET(
    "8 regioni, non tutta Italia: il paper non claima nazionale "
    "(Lelo già non lo fa)."
)
BULLET(
    "Classificazione SNAI 2014 anacronistica per il periodo 1951-"
    "1991: la usiamo come proxy geomorfologica (terreno, altitudine), "
    "non come network di servizi."
)

# ====================================================================
# Risorse e timeline
# ====================================================================
H1("6. Risorse e tempistica")

P("Stato attuale (al " + "16 maggio 2026" + "):")
BULLET(
    "Pipeline R completa funzionante: 10 script (R/01..R/10 + main), "
    "tutti girano end-to-end."
)
BULLET(
    "14 figure prodotte (PNG + PDF), 25+ tabelle (CSV + LaTeX)."
)
BULLET("Bozza paper completa (PAPER.md, ~6500 parole).")
BULLET(
    "Documentazione metodologica (RATIONALE.md) e research "
    "proposal con feasibility assessment (PROPOSAL.md)."
)
BULLET("Repository GitHub con storico commit (Chiascias/Share branch claude/spatial-did-highways-7Ih0r).")

P("Ulteriore investimento richiesto per Tier 2 (Regional Studies, "
  "Italian Economic Journal, REST Tier-2):", bold=True)
BULLET("Rifinitura prose + risposta a referee anticipati: ~1 mese")
BULLET("Submission e revisione: 6-12 mesi processo")
P("Totale: 6-12 mesi al working paper finale, 12-18 mesi alla pubblicazione.")

P("Ulteriore investimento per Tier 1.5 (EEH, JRS, REStat):", bold=True)
BULLET(
    "Costruzione comune centroids da shapefile ISTAT "
    "(Confini Amministrativi): ~3-5 giorni di GIS work."
)
BULLET(
    "Digitalizzazione del corridoio Romita 1955 dal Gazzetta "
    "Ufficiale n.131 dell'8/6/1955: ~1 settimana."
)
BULLET(
    "IV-2SLS Romita 1955 sui moments K0, K7, W2 + first-stage "
    "diagnostics: ~1 settimana."
)
BULLET(
    "Conley spatial-HAC SE (richiede centroidi): ~3-4 giorni."
)
BULLET("Settore decomposition (RQ4) con crosswalk ATECO 1971/1981/1991: ~3-4 settimane.")
P("Totale add-on per puntare a Tier 1.5: ~2 mesi di lavoro aggiuntivo "
  "rispetto allo stato attuale.")

P("Investimenti NON proposti (sarebbero un follow-up paper separato):", bold=True)
BULLET("Modello strutturale spatial equilibrium (Allen-Arkolakis): 6-12 mesi.")
BULLET("Welfare quantification (Donaldson-Hornbeck market access): 3-6 mesi.")
BULLET("Estensione a A2/A3/A4/A14 nazionale: 4-6 mesi.")

# ====================================================================
# Bottom line
# ====================================================================
H1("7. Bottom line")

P(
    "Lo stato attuale è già pubblicabile a Tier 2 (Regional "
    "Studies, Italian Economic Journal, Rivista di Economia e "
    "Statistica del Territorio) con probabilità di accettazione "
    "alta, dopo rifinitura."
)
P(
    "Con ~2 mesi di lavoro aggiuntivo (centroidi + Romita IV + "
    "Conley SE + settori) diventa competitivo a Tier 1.5 "
    "(Explorations in Economic History, Journal of Regional Science, "
    "Review of Economics and Statistics field), con probabilità "
    "di R&R stimata al 35-45%."
)
P(
    "Per Tier 1 generalista (QJE, AER, ECMA) servirebbero "
    "investimenti molto più consistenti (modello strutturale, "
    "scope nazionale) non inclusi in questa proposta — "
    "diventerebbe un follow-up separato."
)
P(
    "La raccomandazione onesta: investire i ~2 mesi per portare il "
    "paper a Tier 1.5. Il valore marginale è alto perché la "
    "pipeline è già costruita e funzionante; l'aggiunta riguarda "
    "GIS work e una specifica IV, non un re-design metodologico.",
    bold=True
)

# Save
doc.save("/home/user/Share/Schema_Lelo_empirical.docx")
print("Saved Schema_Lelo_empirical.docx")
