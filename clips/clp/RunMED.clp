(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)

(deftemplate sintomo
   (slot nome
    (type SYMBOL))
   (slot risposta
    (type SYMBOL))
   (slot valori
	(type SYMBOL)))

(deftemplate trauma
	(slot nome (type SYMBOL))
	(slot mb (type FLOAT) (default 0.0))
	(slot md (type FLOAT) (default 0.0))
	(slot cf (type FLOAT) (default 0.0)))	

; RITRATTARE DIAGNOSI

(deffunction get-all-facts-by-names ($?template-names)
  (bind ?facts (create$))
  (progn$ (?f (get-fact-list))
	   (if (member$ (fact-relation ?f) $?template-names)
	       then (bind ?facts (create$ ?facts ?f))))
  ?facts)

(deffunction print-all-symptoms-status ()
  (bind ?i 1)
  (progn$ (?f (get-all-facts-by-names sintomo))
	  (format t "(%d) %s: %s  (%s)%n" ?i (fact-slot-value ?f nome) (fact-slot-value ?f risposta) (fact-slot-value ?f valori))
	  (bind ?i (+ ?i 1))))
      	  
(deffunction cambia-diagnosi ()
   (printout t crlf " OSSERVAZIONE RIEPILOGO " crlf crlf)
   (print-all-symptoms-status)
   (printout t crlf "   Inserisci il numero della risposta : ")
   (bind ?num (read))
   (bind ?f (nth ?num (get-all-facts-by-names sintomo)))
   (if(= ?num 1)
   then (retract ?f))
   (if(= ?num 2)
   then (retract ?f)) 
   (if(= ?num 3)
   then (retract ?f))
   (assert (revisione)))     
	  	 
; BANNER INIZIALE

(defrule starting-rule
  (declare (salience ?*highest-priority*))
  =>
  (printout t crlf crlf)
  (printout t "   *** RunMED ***" crlf crlf)
  (printout t "   * Dopo aver consultato il sistema, e' fortemente consigliato rivolgersi ad uno specialista per una diagnosi piu' accurata. " crlf)
  (printout t crlf "   * Help: Inserisci il valore ? per comprendere il motivo della domanda o per avere chiarimenti sulla terminologia medica." crlf crlf)
  (assert (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata)))
  (assert (trauma (nome crampo)))
  (assert (trauma (nome contrattura)))
  (assert (trauma (nome elongazione)))
  (assert (trauma (nome distrazione)))
  (assert (trauma (nome strappo-II-grado)))
  (assert (trauma (nome strappo-III-grado))))
  
; CARTELLA CLINICA

(defrule cartella-paziente
   (declare (salience ?*high-priority*))
   =>
   (printout t crlf "   * RunMED consente la gestione della cartella clinica per i pazienti che consultano il sistema. " crlf)
   (printout t "     Se e' la prima volta che consulti il sistema, e' necessario creare la tua cartella clinica. " crlf)
   (printout t crlf "   * Se e' la prima volta che consulti il sistema o vuoi aggiornare la cartella clinica attuale ")
   (printout t crlf "     digita si, altrimenti digita no : ")
   (bind ?cartella (read))
   (if (eq ?cartella no)
      then (printout t crlf "   Inserisci il tuo nome (lettera iniziale in maiuscolo): ")
	       (bind ?nome (read))
		   (assert (nome ?nome))
		   (printout t crlf "   Inserisci il tuo cognome (lettera iniziale in maiuscolo): ")
		   (bind ?cognome (read))
		   (assert (cognome ?cognome))
		   (load* (str-cat "C:/Users/Costa/ICSE-C/clips/clp/" ?nome ?cognome ".clp")))
   (if (eq ?cartella si)
       then (printout t crlf "   Inserisci il tuo nome (lettera iniziale in maiuscolo): ")
	        (bind ?nome (read))
			(assert (nome ?nome))
		    (printout t crlf "   Inserisci il tuo cognome (lettera iniziale in maiuscolo): ")
		    (bind ?cognome (read))
            (assert (cognome ?cognome))
			(printout t crlf crlf "   ANAMNESI GENERALE PAZIENTE " crlf crlf)
			(set-strategy random)))			
        
; ANAMNESI GENERALE

(defrule paziente-genere
   (not (genere ?))  
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Sei un uomo o una donna ?: ")
   (bind ?genere (read))
   (if (eq ?genere "?")
    then (printout t crlf "   * La conformazione del bacino e la meccanica di corsa variano a seconda che il soggetto sia un uomo o una donna." crlf)	     
    else (assert (genere ?genere))
	     (break))))

(defrule paziente-eta
   (not (eta ?)) 
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Qual e' la tua eta' ?: ")
   (bind ?eta (read))
   (if (eq ?eta "?")
   then (printout t crlf "   * La fascia di eta' del soggetto e' fondamentale per stimare il grado di attendibilita' della diagnosi." crlf)
   else (assert (eta ?eta))
        (break))))

(defrule paziente-peso
   (not (peso ?))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Qual e' il tuo peso corporeo? (espresso in kg): ")
   (bind ?peso (read))
   (if (eq ?peso "?")
   then (printout t crlf "   * Il tuo peso e' fondamentale per stimare l'indice di massa corporea." crlf)
   else (assert (peso ?peso))
        (break))))

(defrule paziente-altezza 
   (not (altezza ?))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Qual e' la tua altezza? (espressa in metri): ")
   (bind ?altezza (read))
   (if (eq ?altezza "?")
   then (printout t crlf "   * La tua altezza e' fondamentale per stimare l'indice di massa corporea." crlf)
   else (assert (altezza ?altezza))
        (break))))

(defrule paziente-condizione
   (altezza ?altezza&:(> ?altezza 0))
   (peso ?peso&:(> ?peso 0))
   =>   
   (if (< (imc ?peso ?altezza) 19)
       then (assert (condizione sottopeso)))
   (if (> (imc ?peso ?altezza) 25)
       then (assert (condizione sovrappeso)))
   (if (and (> (imc ?peso ?altezza) 19.0001) (< (imc ?peso ?altezza) 24.9999))
       then (assert (condizione normopeso))))   
	   	   
(defrule paziente-frequenza-riposo 
   (not (frequenza-riposo ?))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Inserisci la tua frequenza cardiaca a riposo (espressa in bpm): ")
   (bind ?frequenza-riposo (read))
   (if (eq ?frequenza-riposo "?")
   then (printout t crlf "   * Voglio comprendere se la tua frequenza cardiaca e' soggetta a variazioni patologiche." crlf)
   else (assert (frequenza-riposo ?frequenza-riposo))
        (break))))

(defrule interpretazione-frequenza
   (declare (salience ?*high-priority*))
   (frequenza-riposo ?frequenza-riposo)
   =>
   (if (and (< ?frequenza-riposo 60) (> ?frequenza-riposo 0))
       then (assert (frequenza-cardiaca bradicardia)))
   (if (> ?frequenza-riposo 100) 
       then (assert (frequenza-cardiaca tachicardia)))
   (if (and (> ?frequenza-riposo 59) (< ?frequenza-riposo 101)) 
       then (assert (frequenza-cardiaca normale))))   
			
(defrule paziente-patologia-vascolare
   (not (patologia ?))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Indica se in passato ti e' stata diagnosticata una delle seguenti patologie vascolari: " crlf)
   (printout t "(1) Insufficienza venosa" crlf)
   (printout t "(2) Insufficienza arteriosa" crlf)
   (printout t "(3) Vasculopatia periferica" crlf)
   (printout t "(4) Sindrome di Raynaud" crlf)
   (printout t "(5) Nessuna delle seguenti" crlf)
   (printout t "   Inserisci il numero : ")
   (bind ?patologia (read))
   (if (eq ?patologia 1)
       then (assert (patologia insufficienza-venosa))
	        (break))
   (if (eq ?patologia 2)
       then (assert (patologia insufficienza-arteriosa))
	        (break))
   (if (eq ?patologia 3)
       then (assert (patologia vasculopatia-periferica))
	        (break))
   (if (eq ?patologia 4)
       then (assert (patologia sindrome-raynaud))
	        (break))
   (if (eq ?patologia 5)
       then (assert (patologia nessuna))
	        (break))
   (if (eq ?patologia "?")
       then (printout t crlf "   * Le patologie vascolari possono causare un'alterazione della circolazione sanguigna." crlf))))   
    	   
(defrule paziente-fascia-eta
   (eta ?eta&:(> ?eta 0))
   =>
   (if (< ?eta 31)
       then (assert (fascia-eta giovane)))
   (if (and (> ?eta 30) (< ?eta 41))
       then (assert (fascia-eta over-30)))
   (if (> ?eta 40)
       then (assert (fascia-eta over-40))))	   
   
; FINE ANAMNESI GENERALE

(defrule creazione-cartella
   (declare (salience ?*lowest-priority*))
   (nome ?nome)
   (cognome ?cognome)
   (genere ?genere)
   (eta ?eta)
   (frequenza-riposo ?fr)
   (peso ?peso)
   (altezza ?altezza)
   (patologia ?pat)
   =>
   (creacartella ?nome ?cognome ?genere ?eta ?fr ?peso ?altezza ?pat))

(defrule anamnesi-completata
   (genere ?gen)
   (eta ?eta)
   (fascia-eta ?fascia)
   (condizione ?cond)
   (frequenza-cardiaca ?freq)  
   (patologia ?pa)
   (not (anamnesi-generale completata))
   =>
   (printout t "" crlf crlf)
   (printout t "   ANAMNESI GENERALE PAZIENTE COMPLETATA" crlf crlf)
   (printout t "   GENERE : " ?gen crlf)
   (printout t "   FASCIA DI ETA' : " ?fascia crlf)
   (printout t "   CONDIZIONE PESO CORPOREO : " ?cond crlf)
   (printout t "   INTERPRETAZIONE FREQUENZA CARDIACA : " ?freq crlf)
   (printout t "   PATOLOGIA VASCOLARE PASSATA : " ?pa crlf)   
   (assert (anamnesi-generale completata))
   (set-strategy depth)
   (printout t crlf crlf "   ANAMNESI SPECIFICA DI INTERAZIONE " crlf crlf))

; ANAMNESI SPECIFICA

(defrule chiedi-zona-dolore
   (anamnesi-generale completata)
   (not (zona-dolore ?))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Bene, indica la zona muscolare dove accusi dolore (coscia/gamba): ")   
   (bind ?dolore (read))
   (if (eq ?dolore "?")
   then (printout t crlf "   * RunMED e' un sistema specializzato sulla corsa, per cui e' in grado di fornire diagnosi solo sui" crlf)
        (printout t "     muscoli che vengono coinvolti maggiormente nel gesto atletico della corsa, ovvero i muscoli degli arti inferiori." crlf)
		(printout t crlf "   * Coscia: distretto muscolare superiore dell'arto inferiore. " crlf)
		(printout t crlf "   * Gamba: distretto muscolare che va dal ginocchio alla caviglia. " crlf)
   else	(assert (zona-dolore ?dolore))
        (break))))

(defrule chiedi-zona-dolore-specifica
   (declare (salience ?*highest-priority*))
   (anamnesi-generale completata)
   (zona-dolore coscia|gamba)
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Indica la localizzazione : " crlf)
   (printout t "(1) Anteriore " crlf)
   (printout t "(2) Posteriore " crlf)
   (printout t "(3) Laterale " crlf)
   (printout t "(4) Interno " crlf)
   (printout t "   Inserisci il numero : ")
   (bind ?local (read))
   (if (eq ?local "?")
        then (printout t crlf "   * Per una maggiore accuratezza, il sistema ti chiede di individuale la zona muscolare in cui provi dolore. " crlf)
		else (break))))

(defrule chiedi-num-km
   (declare (salience ?*high-priority*))
   (anamnesi-generale completata)
   (zona-dolore coscia|gamba)
   =>  
   (while (not (eq 1 0))   
   (printout t crlf "   Inserisci il numero di kilometri che hai corso nell'ultima settimana: ")
   (bind ?num (read))
   (if (eq ?num "?")
        then (printout t crlf "   * Il numero dei chilometri corsi settimanalmente dal paziente, aiuta a definire una predisposizione a " crlf)
             (printout t "     determinati infortuni muscolari. " crlf)		
        else (assert (num-km ?num))
		     (assert (anamnesi-specifica completata))
             (printout t crlf crlf "   OSSERVAZIONE PAZIENTE" crlf crlf)
             (break))))			 
   
; OSSERVAZIONE MUSCOLARE  

(defrule chiedi-insorgenza
   (anamnesi-specifica completata)
   (zona-dolore coscia|gamba)
   (not (sintomo (nome insorgenza) (risposta durante-allenamento|giorno-dopo|dopo-allenamento)))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Quando hai notato l'insorgenza del dolore per la prima volta ? " crlf)
   (printout t "(1) Durante l'allenamento" crlf)
   (printout t "(2) Il giorno dopo l'allenamento" crlf)
   (printout t "(3) Subito dopo l'allenamento" crlf)
   (printout t "   Inserisci il numero : ")
   (bind ?risp (read))
   (if (eq ?risp 1)
       then (assert (sintomo (nome insorgenza) (risposta durante-allenamento) (valori durante-allenamento/giorno-dopo/dopo-allenamento)))
	        (break))
   (if (eq ?risp 2)
       then (assert (sintomo (nome insorgenza) (risposta giorno-dopo) (valori giorno-dopo/durante-allenamento/dopo-allenamento)))
	        (break))
   (if (eq ?risp 3)
       then (assert (sintomo (nome insorgenza) (risposta dopo-allenamento) (valori dopo-allenamento/giorno-dopo/durante-allenamento)))
	        (break))
   (if (eq ?risp "?")
       then (printout t crlf "   * L'insorgenza e' utile per far chiarezza sulla dinamica dell'infortunio." crlf))))   
   
(defrule chiedi-dolore-durante
   (declare (salience ?*high-priority*))
   (anamnesi-specifica completata)
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   (not (sintomo (nome dolore) (risposta pulsorio|fitta|bruciore)))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Descrivi che tipo di dolore hai provato nel momento dell'insorgenza durante l'allenamento: " crlf)
   (printout t "(1) Pulsorio " crlf)
   (printout t "(2) Fitta " crlf)
   (printout t "(3) Bruciore " crlf)
   (printout t "   Inserisci il numero : ")
   (bind ?risp (read))
   (if (eq ?risp 1)
        then (assert (sintomo (nome dolore) (risposta pulsorio) (valori pulsorio/fitta/bruciore)))
		     (break))
   (if (eq ?risp 2)
        then (assert (sintomo (nome dolore) (risposta fitta) (valori fitta/pulsorio/bruciore)))
             (break))
   (if (eq ?risp 3)
        then (assert (sintomo (nome dolore) (risposta bruciore) (valori bruciore/pulsorio/fitta)))
             (break))		
   (if (eq ?risp "?")
        then (printout t crlf "   * Pulsorio: sensazione di dolore che compare e scompare a cadenza regolare,")
		     (printout t crlf "               simile ad una pulsazione cardiaca. " )
			 (printout t crlf "   * Fitta: sensazione di una scossa avvertita improvvisamente. " )
			 (printout t crlf "   * Bruciore: sensazione di bruciore costante avvertita nel distretto muscolare soggetto. " crlf))))
   
(defrule chiedi-dolore-subito-dopo
   (declare (salience ?*high-priority*))
   (anamnesi-specifica completata)
   (sintomo (nome insorgenza) (risposta dopo-allenamento))
   (not (sintomo (nome dolore) (risposta puntorio|non-allungabilita)))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Descrivi il tipo di dolore che hai provato nel momento dell'insorgenza : " crlf)
   (printout t "(1) Puntorio " crlf)
   (printout t "(2) Non allungabilita' della muscolatura soggetta " crlf)
   (printout t "   Inserisci il numero : ")
   (bind ?risp (read))
   (if (eq ?risp 1)
        then (assert (sintomo (nome dolore) (risposta puntorio) (valori puntorio/non-allungabilita)))
		     (break))
   (if (eq ?risp 2)
        then (assert (sintomo (nome dolore) (risposta non-allungabilita) (valori non-allungabilita/puntorio)))
		     (break))
   (if (eq ?risp "?")
   then (printout t crlf "   * Puntorio: sensazione di dolore simile ad una puntura avvertita improvvisamente. ")
        (printout t crlf "   * Non allungabilita': sensazione di rigidita' muscolare." crlf))))

(defrule chiedi-evento
   (declare (salience ?*high-priority*))
   (anamnesi-specifica completata)
   (sintomo (nome insorgenza) (risposta giorno-dopo))
   (not (sintomo (nome evento) (risposta traumatico|non-traumatico))) 
   =>
   (while (not (eq 1 0))
   (printout t crlf "   L'evento che ha causato la comparsa del dolore e' di natura traumatico o non traumatico ? (traumatico/non-traumatico): ")
   (bind ?risp (read))
   (if (eq ?risp "?")
    then (printout t crlf "   * Se il paziente ha dovuto interrompere l'attività motoria, e' molto probabile che si tratti di un evento traumatico. " crlf)
    else (assert (sintomo (nome evento) (risposta ?risp) (valori traumatico/non-traumatico)))
	     (break))))

(defrule chiedi-punto-dolore
   (declare (salience ?*high-priority*))
   (anamnesi-specifica completata)
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   (sintomo (nome dolore) (risposta fitta))
   (not (sintomo (nome punto-dolore) (risposta localizzato|distribuito)))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Il dolore da te percepito a riposo e' ben localizzato in un determinato punto " crlf)
   (printout t "   oppure distruibuito su un distretto muscolare ? (localizzato/distribuito): ")
   (bind ?risp (read))
   (if (eq ?risp "?")
    then (printout t crlf "   * Il dolore localizzato potrebbe favorire la presenza di una lesione muscolare." crlf)
    else (assert (sintomo (nome punto-dolore) (risposta ?risp) (valori localizzato/distribuito)))
	     (break))))

(defrule chiedi-lesioni-strutturali
   (declare (salience ?*high-priority*))
   (anamnesi-specifica completata)
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   (sintomo (nome dolore) (risposta pulsorio))
   (not (sintomo (nome lesioni-strutturali) (risposta ematoma|ecchimosi|nessuna)))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Sono visibili lesioni strutturali sulla muscolatura soggetta ? " crlf)
   (printout t "(1) Ematoma" crlf)
   (printout t "(2) Ecchimosi" crlf)
   (printout t "(3) Nessuna" crlf)
   (printout t "   Inserisci il numero : ")
   (bind ?risp (read))
   (if (eq ?risp 1)
       then (assert (sintomo (nome lesioni-strutturali) (risposta ematoma) (valori ematoma/ecchimosi/nessuna)))
	        (break))
   (if (eq ?risp 2)
       then (assert (sintomo (nome lesioni-strutturali) (risposta ecchimosi) (valori ecchimosi/ematoma/nessuna)))
	        (break))
   (if (eq ?risp 3)
       then (assert (sintomo (nome lesioni-strutturali) (risposta nessuna) (valori nessuna/ematoma/ecchimosi)))
	        (break))
   (if (eq ?risp "?")
       then (printout t crlf "   * Le lesioni strutturali sono indice di un versamento ematico nel distretto muscolare." crlf)
            (printout t crlf "   * Ecchimosi: L'ecchimosi e' caratterizzata da un travaso ematico molto limitato a livello sottocutaneo.")
			(printout t crlf "   * Ematoma: L'ematoma e' caratterizzata da un travaso ematico maggiore che va a localizzarsi in un tessuto."))))

(defrule chiedi-perdita-funzionale
   (declare (salience ?*high-priority*))
   (anamnesi-specifica completata)
   (sintomo (nome insorgenza) (risposta durante-allenamento|dopo-allenamento))
   (sintomo (nome dolore) (risposta bruciore|puntorio))
   (not (sintomo (nome perdita-funzionale) (risposta si|no)))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Hai notato una perdita funzionale nel momento dell'insorgenza del dolore ? (si/no): ")
   (bind ?risp (read))
   (if (eq ?risp "?")
    then (printout t crlf "   * Per perdita funzionale si intende la temporanea perdita della funzione motoria della muscolatura soggetta." crlf)
    else (assert (sintomo (nome perdita-funzionale) (risposta ?risp) (valori si/no)))
	     (break))))

(defrule chiedi-diminuisce-stretching
   (declare (salience ?*high-priority*))
   (anamnesi-specifica completata)
   (sintomo (nome insorgenza) (risposta dopo-allenamento))
   (sintomo (nome dolore) (risposta non-allungabilita))
   (not (sintomo (nome diminuisce-stretching) (risposta si|no|parzialmente)))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Il dolore percepito a riposo diminuisce effettuando una seduta di stretching ? (si/no/parzialmente) : ")
   (bind ?risp (read))
   (if (eq ?risp "?")
    then (printout t crlf "   * Se il dolore persiste anche dopo lo stretching, potrebbe trattarsi di una lesione muscolare. " crlf)
    else (assert (sintomo (nome diminuisce-stretching) (risposta ?risp) (valori si/no/parzialmente)))
	     (break))))

(defrule problema-deambulazione
   (declare (salience ?*high-priority*))
   (anamnesi-specifica completata)
   (sintomo (nome insorgenza) (risposta giorno-dopo))
   (sintomo (nome evento) (risposta traumatico))
   (not (sintomo (nome problema-deambulazione) (risposta si|no)))
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Il dolore percepito influisce nella deambulazione ? (si/no): ")
   (bind ?risp (read))
   (if (eq ?risp "?")
    then (printout t crlf "   * Se il paziente non riesce a camminare, potrebbe trattarsi di una lesione muscolare. " crlf)
    else (assert (sintomo (nome problema-deambulazione) (risposta ?risp) (valori si/no)))
	     (break))))				
       
; DIAGNOSI 
   
(defrule diagnosi-trauma
   (declare (salience ?*low-priority*))
   ?tra <- (trauma (nome ?n) (mb ?mb) (md ?md) (cf ?cf))
   (not (diagnosi-trauma ?n))
   =>	
   (bind ?newcf (certezza ?mb ?md))
   (if (< ?newcf 0.0)
       then (modify ?tra (cf 0.0))
	        (bind ?newcf 0.0)
       else (modify ?tra (cf ?newcf)))
   (if (> ?newcf 0.35)
   then   
   (printout t "" crlf)
   (printout t "   DIAGNOSI : " ?n crlf)
   (printout t "   CERTEZZA : " ?newcf crlf)
   (if (eq ?n indolenzimento-muscolare-a-insorgenza-ritardata) then (printout t "   PROGRAMMA RIABILITATIVO : " crlf) (printout t "   Terapia del caldo-freddo, crioterapia, stretching. Tempo di recupero 2gg circa." crlf))
   (if (eq ?n crampo) then (printout t "   PROGRAMMA RIABILITATIVO : " crlf) (printout t "   Stretching, massaggio profondo, tecniche di restrain, compressione del distretto muscolare, riposo. " crlf))
   (if (eq ?n contrattura) then (printout t "   PROGRAMMA RIABILITATIVO : " crlf) (printout t "   Tests per verificare la localizzazione, trattamento dei trigger points, tecniche di energia muscolare, stretching, stretch and spray." crlf))
   (if (eq ?n elongazione) then (printout t "   PROGRAMMA RIABILITATIVO : " crlf) (printout t "   Tests per verificare la localizzazione, riposo, crioterapia, stretching leggero, tecniche di jones, normalizzazione del tono muscolare, stretch and spray." crlf))
   (if (eq ?n distrazione) then (printout t "   PROGRAMMA RIABILITATIVO : " crlf) (printout t "   Riposo, compressione/elevazione del distretto muscolare, crioterapia. Tempo di recupero 8-15gg circa." crlf))
   (if (eq ?n strappo-II-grado) then (printout t "   PROGRAMMA RIABILITATIVO : " crlf) (printout t "   Protocollo PRICE, ghiaccio a riposo nelle prime 2 settimane, stretching molto leggero, tecarterapia, laserterapia. Tempo di recupero 2-4sett o piu' circa." crlf))
   (if (eq ?n strappo-III-grado) then (printout t "   PROGRAMMA RIABILITATIVO : " crlf) (printout t "   Protocollo PRICE, crioterapia, riposo, distretto muscolare in compressione" crlf) (printout t "   Dopo almeno 3/4 settimane: tecarterapia in atermia, laserterapia, normalizzazione tono muscolare e rielasticizzazione muscolare." crlf) (printout t "   Tempo di recupero 5-8sett. circa. (potrebbe essere richiesto intervento chirurgico.)" crlf)))		
   (assert (diagnosi-trauma ?n)))   
	
(defrule diagnosi-revisione
   (declare (salience ?*low-priority*))
   (not (revisione))
   ?tra-uno <- (diagnosi-trauma indolenzimento-muscolare-a-insorgenza-ritardata)
   ?tra-due <- (diagnosi-trauma crampo)
   ?tra-tre <- (diagnosi-trauma contrattura)
   ?tra-quattro <- (diagnosi-trauma elongazione)
   ?tra-cinque <- (diagnosi-trauma distrazione)
   ?tra-sei <- (diagnosi-trauma strappo-II-grado)
   ?tra-sette <- (diagnosi-trauma strappo-III-grado)
   =>
   (while (not (eq 1 0))
   (printout t crlf "   Vorresti ritrattare le diagnosi ? (si/no): ")
   (bind ?r (read))
   (if (eq ?r si)
   then (retract ?tra-uno ?tra-due ?tra-tre ?tra-quattro ?tra-cinque ?tra-sei ?tra-sette)
        (cambia-diagnosi)
		(break))
   (if (eq ?r no) then (break))
   (if (eq ?r "?") then (printout t crlf "   * RunMED consente la ritrattazione delle diagnosi, ovvero la modifica delle risposte" crlf)
                        (printout t "     fornite in precedenza al sistema, per formulare nuove diagnosi." crlf))))

; DOMS CERTEZZA

(defrule doms-num-km
   (not (doms-num-km))
   (num-km ?num&:(< ?num 30))
   ?tra <- (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.214)))
   (assert (doms-num-km)))
   
(defrule doms-insorgenza-giorno-dopo
   (not (doms-insorgenza-giorno-dopo))
   (sintomo (nome insorgenza) (risposta giorno-dopo))
   ?tra <- (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.403)))
   (assert (doms-insorgenza-giorno-dopo)))   
      
(defrule doms-dolore-non-allungabilita
   (not (doms-dolore-non-allungabilita))
   (sintomo (nome dolore) (risposta non-allungabilita))
   ?tra <- (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.256)))
   (assert (doms-dolore-non-allungabilita)))  
   
(defrule doms-causa-non-traumatico
   (not (doms-causa-non-traumatico))
   (sintomo (nome evento) (risposta non-traumatico))
   ?tra <- (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.201)))
   (assert (doms-causa-non-traumatico)))  
   
(defrule doms-dolore-distribuito
   (not (doms-dolore-distribuito))
   (sintomo (nome punto-dolore) (risposta distribuito))
   ?tra <- (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.211)))
   (assert (doms-dolore-distribuito))) 
   
(defrule doms-lesioni-nessuna
   (not (doms-lesioni-nessuna))
   (sintomo (nome lesioni-strutturali) (risposta nessuna))
   ?tra <- (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.105)))
   (assert (doms-lesioni-nessuna))) 

(defrule doms-perdita-funzionale-no
   (not (doms-perdita-funzionale-no))
   (sintomo (nome perdita-funzionale) (risposta no))
   ?tra <- (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.216)))
   (assert (doms-perdita-funzionale-no)))
   
(defrule doms-prob-deamb-no
   (not (doms-prob-deamb-no))   
   (sintomo (nome problema-deambulazione) (risposta no))
   ?tra <- (trauma (nome indolenzimento-muscolare-a-insorgenza-ritardata) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.108)))
   (assert (doms-prob-deamb-no)))  
   
; CRAMPO CERTEZZA

(defrule crampo-patologie-vascolari
   (not (crampo-patologie-vascolari))
   (patologia ~nessuna)
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.206)))
   (assert (crampo-patologie-vascolari)))
   
(defrule crampo-insorgenza-durante
   (not (crampo-insorgenza-durante))
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.213)))
   (assert (crampo-insorgenza-durante)))

(defrule crampo-dolore-bruciore   
   (not (crampo-dolore-bruciore))
   (sintomo (nome dolore) (risposta bruciore))
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.209)))
   (assert (crampo-dolore-bruciore)))

(defrule crampo-dolore-non-allungabilita
   (not (crampo-dolore-non-allungabilita))
   (sintomo (nome dolore) (risposta non-allungabilita))
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.211)))
   (assert (crampo-dolore-non-allungabilita)))
   
(defrule crampo-causa-non-traumatico 
   (not (crampo-causa-non-traumatico))
   (sintomo (nome evento) (risposta non-traumatico))
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.213)))
   (assert (crampo-causa-non-traumatico))) 

(defrule crampo-dolore-distribuito
   (not (crampo-dolore-distribuito))
   (sintomo (nome punto-dolore) (risposta distribuito))
   ?tra <- (trauma (nome campo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.202)))
   (assert (crampo-dolore-distribuito)))
   
(defrule crampo-lesioni-nessuna
   (not (crampo-lesioni-nessuna))
   (sintomo (nome lesioni-strutturali) (risposta nessuna))
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.106)))
   (assert (crampo-lesioni-nessuna)))
   
(defrule crampo-perdita-funzionale-no
   (not (crampo-perdita-funzionale-no))
   (sintomo (nome perdita-funzionale) (risposta no))
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.212)))
   (assert (crampo-perdita-funzionale-no)))

(defrule crampo-dim-stretching-si
   (not (crampo-dim-stretching-si))
   (sintomo (nome diminuisce-stretching) (risposta si))
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.218)))
   (assert (crampo-dim-stretching-si)))
   
(defrule crampo-prob-deamb-no
   (not (crampo-prob-deamb-no))   
   (sintomo (nome problema-deambulazione) (risposta no))
   ?tra <- (trauma (nome crampo) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.156)))
   (assert (crampo-prob-deamb-no)))

; CONTRATTURA CERTEZZA

(defrule contrattura-num-km
   (not (contrattura-num-km))
   (num-km ?num&:(> ?num 60))
   (condizione normopeso|sovrappeso)
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.201)))
   (assert (contrattura-num-km)))
   
(defrule contrattura-insorgenza-durante
   (not (contrattura-insorgenza-durante))
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.108)))
   (assert (contrattura-insorgenza-durante)))

(defrule contrattura-insorgenza-dopo
   (not (contrattura-insorgenza-dopo))
   (sintomo (nome insorgenza) (risposta dopo-allenamento))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.310)))
   (assert (contrattura-insorgenza-dopo)))
   
(defrule contrattura-dolore-puntorio
   (not (contrattura-dolore-puntorio))
   (sintomo (nome dolore) (risposta puntorio))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.201)))
   (assert (contrattura-dolore-puntorio)))

(defrule contrattura-dolore-non-allungabilita
   (not (contrattura-dolore-non-allungabilita))
   (sintomo (nome dolore) (risposta non-allungabilita))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.100)))
   (assert (contrattura-dolore-non-allungabilita)))
   
(defrule contrattura-causa-non-traumatico 
   (not (contrattura-causa-non-traumatico))
   (sintomo (nome evento) (risposta non-traumatico))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.214)))
   (assert (contrattura-causa-non-traumatico)))
   
(defrule contrattura-dolore-distribuito
   (not (contrattura-dolore-distribuito))
   (sintomo (nome punto-dolore) (risposta distribuito))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.209)))
   (assert (contrattura-dolore-distribuito)))
   
(defrule contrattura-lesioni-nessuna
   (not (contrattura-lesioni-nessuna))
   (sintomo (nome lesioni-strutturali) (risposta nessuna))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.108)))
   (assert (contrattura-lesioni-nessuna)))
   
(defrule contrattura-perdita-funzionale-si
   (not (contrattura-perdita-funzionale-si))
   (sintomo (nome perdita-funzionale) (risposta si))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.209)))
   (assert (contrattura-perdita-funzionale-si)))
   
(defrule contrattura-dim-stretching-si
   (not (contrattura-dim-stretching-si))
   (sintomo (nome diminuisce-stretching) (risposta si))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.203)))
   (assert (contrattura-dim-stretching-si))) 

(defrule contrattura-dim-stretching-parz
   (not (contrattura-dim-stretching-parz))
   (sintomo (nome diminuisce-stretching) (risposta parzialmente))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.204)))
   (assert (contrattura-dim-stretching-parz)))
   
(defrule contrattura-prob-deamb-no
   (not (contrattura-prob-deamb-no))   
   (sintomo (nome problema-deambulazione) (risposta no))
   ?tra <- (trauma (nome contrattura) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.124)))
   (assert (contrattura-prob-deamb-no)))

; ELONGAZIONE CERTEZZA

(defrule elongazione-eta
   (not (elongazione-eta))
   (fascia-eta over-40)
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.109)))
   (assert (elongazione-eta)))
   
(defrule elongazione-insorgenza-giorno-dopo
   (not (elongazione-insorgenza-giorno-dopo))
   (sintomo (nome insorgenza) (risposta giorno-dopo))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.115)))
   (assert (elongazione-insorgenza-giorno-dopo)))
   
(defrule elongazione-insorgenza-durante
   (not (elongazione-insorgenza-durante))
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.203)))
   (assert (elongazione-insorgenza-durante)))
   
(defrule elongazione-dolore-fitta
   (not (elongazione-dolore-fitta))
   (sintomo (nome dolore) (risposta fitta))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.206)))
   (assert (elongazione-dolore-fitta)))
   
(defrule elongazione-causa-non-traumatico 
   (not (elongazione-causa-non-traumatico))
   (sintomo (nome evento) (risposta non-traumatico))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.213)))
   (assert (elongazione-causa-non-traumatico)))
   
(defrule elongazione-dolore-distribuito
   (not (elongazione-dolore-distribuito))
   (sintomo (nome punto-dolore) (risposta distribuito))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.216)))
   (assert (elongazione-dolore-distribuito)))
   
(defrule elongazione-lesioni-nessuna
   (not (elongazione-lesioni-nessuna))
   (sintomo (nome lesioni-strutturali) (risposta nessuna))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.108)))
   (assert (elongazione-lesioni-nessuna)))
   
(defrule elongazione-perdita-funzionale-si
   (not (elongazione-perdita-funzionale-si))
   (sintomo (nome perdita-funzionale) (risposta si))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.204)))
   (assert (elongazione-perdita-funzionale-si)))
   
(defrule elongazione-dim-stretching-parz
   (not (elongazione-dim-stretching-parz))
   (sintomo (nome diminuisce-stretching) (risposta parzialmente))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.209)))
   (assert (elongazione-dim-stretching-parz)))

(defrule elongazione-prob-deamb-no
   (not (elongazione-prob-deamb-no))   
   (sintomo (nome problema-deambulazione) (risposta no))
   ?tra <- (trauma (nome elongazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.113)))
   (assert (elongazione-prob-deamb-no)))

; DISTRAZIONE CERTEZZA   

(defrule distrazione-eta
   (not (distrazione-eta))
   (fascia-eta over-40)
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.104)))
   (assert (distrazione-eta)))
   
(defrule distrazione-num-km
   (not (distrazione-num-km))
   (num-km ?num&:(> ?num 60))
   (condizione normopeso|sovrappeso)
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.206)))
   (assert (distrazione-num-km)))
   
(defrule distrazione-insorgenza-durante
   (not (distrazione-insorgenza-durante))
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.205)))
   (assert (distrazione-insorgenza-durante)))
   
(defrule distrazione-dolore-fitta
   (not (distrazione-dolore-fitta))
   (sintomo (nome dolore) (risposta fitta))
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.217)))
   (assert (distrazione-dolore-fitta)))
   
(defrule distrazione-causa-traumatico 
   (not (distrazione-causa-traumatico))
   (sintomo (nome evento) (risposta traumatico))
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.221)))
   (assert (distrazione-causa-traumatico)))
   
(defrule distrazione-dolore-localizzato
   (not (distrazione-dolore-localizzato))
   (sintomo (nome punto-dolore) (risposta localizzato))
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.209)))
   (assert (distrazione-dolore-localizzato)))

(defrule distrazione-dolore-distribuito
   (not (distrazione-dolore-distribuito))
   (sintomo (nome punto-dolore) (risposta distribuito))
   ?tra <- (trauma (nome distrazione) (md ?md))
   =>
   (modify ?tra (md (incr_md ?md 0.143)))
   (assert (distrazione-dolore-distribuito)))

(defrule distrazione-lesione-ecchimosi
   (not (distrazione-lesione-ecchimosi))
   (sintomo (nome lesioni-strutturali) (risposta ecchimosi))
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.207)))
   (assert (distrazione-lesione-ecchimosi)))

(defrule distrazione-perdita-funzionale-si
   (not (distrazione-perdita-funzionale-si))
   (sintomo (nome perdita-funzionale) (risposta si))
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.213)))
   (assert (distrazione-perdita-funzionale-si)))
   
(defrule distrazione-dim-stretching-no
   (not (distrazione-dim-stretching-no))
   (sintomo (nome diminuisce-stretching) (risposta no))
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.208)))
   (assert (distrazione-dim-stretching-no)))
   
(defrule distrazione-prob-deamb-si
   (not (distrazione-prob-deamb-si))   
   (sintomo (nome problema-deambulazione) (risposta si))
   ?tra <- (trauma (nome distrazione) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.221)))
   (assert (distrazione-prob-deamb-si)))

; STRAPPO-II-GRADO CERTEZZA

(defrule strappo-II-grado-eta
   (not (strappo-II-grado-eta))
   (fascia-eta over-40)
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.101)))
   (assert (strappo-II-grado-eta)))
   
(defrule strappo-II-grado-insorgenza-durante
   (not (strappo-II-grado-insorgenza-durante))
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.206)))
   (assert (strappo-II-grado-insorgenza-durante)))
   
(defrule strappo-II-grado-dolore-pulsorio
   (not (strappo-II-grado-dolore-pulsorio))
   (sintomo (nome dolore) (risposta pulsorio))
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.203)))
   (assert (strappo-II-grado-dolore-pulsorio)))
   
(defrule strappo-II-grado-causa-traumatico 
   (not (strappo-II-grado-causa-traumatico))
   (sintomo (nome evento) (risposta traumatico))
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.214)))
   (assert (strappo-II-grado-causa-traumatico)))
   
(defrule strappo-II-grado-dolore-localizzato
   (not (strappo-II-grado-dolore-localizzato))
   (sintomo (nome punto-dolore) (risposta localizzato))
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.212)))
   (assert (strappo-II-grado-dolore-localizzato)))
   
(defrule strappo-II-grado-lesione-ecchimosi
   (not (strappo-II-grado-lesione-ecchimosi))
   (sintomo (nome lesioni-strutturali) (risposta ecchimosi))
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.409)))
   (assert (strappo-II-grado-lesione-ecchimosi)))
   
(defrule strappo-II-grado-lesione-nessuna
   (not (strappo-II-grado-lesione-nessuna))
   (sintomo (nome lesioni-strutturali) (risposta nessuna))
   ?tra <- (trauma (nome strappo-II-grado) (md ?md))
   =>
   (modify ?tra (md (incr_md ?md 0.113)))
   (assert (strappo-II-grado-lesione-nessuna)))
   
(defrule strappo-II-grado-perdita-funzionale-si
   (not (strappo-II-grado-perdita-funzionale-si))
   (sintomo (nome perdita-funzionale) (risposta si))
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.204)))
   (assert (strappo-II-grado-perdita-funzionale-si)))
   
(defrule strappo-II-grado-dim-stretching-no
   (not (strappo-II-grado-dim-stretching-no))
   (sintomo (nome diminuisce-stretching) (risposta no))
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.203)))
   (assert (strappo-II-grado-dim-stretching-no)))
   
(defrule strappo-II-grado-prob-deamb-si
   (not (strappo-II-grado-prob-deamb-si))   
   (sintomo (nome problema-deambulazione) (risposta si))
   ?tra <- (trauma (nome strappo-II-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.204)))
   (assert (strappo-II-grado-prob-deamb-si)))

; STRAPPO-III-GRADO CERTEZZA

(defrule strappo-III-grado-eta
   (not (strappo-III-grado-eta))
   (fascia-eta over-40)
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.110)))
   (assert (strappo-III-grado-eta)))
   
(defrule strappo-III-grado-insorgenza-durante
   (not (strappo-III-grado-insorgenza-durante))
   (sintomo (nome insorgenza) (risposta durante-allenamento))
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.204)))
   (assert (strappo-III-grado-insorgenza-durante)))
   
(defrule strappo-III-grado-dolore-pulsorio
   (not (strappo-III-grado-dolore-pulsorio))
   (sintomo (nome dolore) (risposta pulsorio))
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.207)))
   (assert (strappo-III-grado-dolore-pulsorio)))
   
(defrule strappo-III-grado-causa-traumatico 
   (not (strappo-III-grado-causa-traumatico))
   (sintomo (nome evento) (risposta traumatico))
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.214)))
   (assert (strappo-III-grado-causa-traumatico)))
   
(defrule strappo-III-grado-dolore-localizzato
   (not (strappo-III-grado-dolore-localizzato))
   (sintomo (nome punto-dolore) (risposta localizzato))
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.221)))
   (assert (strappo-III-grado-dolore-localizzato)))
   
(defrule strappo-III-grado-lesione-ematoma
   (not (strappo-III-grado-lesione-ematoma))
   (sintomo (nome lesioni-strutturali) (risposta ematoma))
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.403)))
   (assert (strappo-III-grado-lesione-ematoma)))
   
(defrule strappo-III-grado-lesione-nessuna
   (not (strappo-III-grado-lesione-nessuna))
   (sintomo (nome lesioni-strutturali) (risposta nessuna))
   ?tra <- (trauma (nome strappo-III-grado) (md ?md))
   =>
   (modify ?tra (md (incr_md ?md 0.114)))
   (assert (strappo-III-grado-lesione-nessuna)))
   
(defrule strappo-III-grado-perdita-funzionale-si
   (not (strappo-III-grado-perdita-funzionale-si))
   (sintomo (nome perdita-funzionale) (risposta si))
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.204)))
   (assert (strappo-III-grado-perdita-funzionale-si)))
   
(defrule strappo-III-grado-dim-stretching-no
   (not (strappo-III-grado-dim-stretching-no))
   (sintomo (nome diminuisce-stretching) (risposta no))
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.214)))
   (assert (strappo-III-grado-dim-stretching-no)))
   
(defrule strappo-III-grado-prob-deamb-si
   (not (strappo-III-grado-prob-deamb-si))   
   (sintomo (nome problema-deambulazione) (risposta si))
   ?tra <- (trauma (nome strappo-III-grado) (mb ?mb))
   =>
   (modify ?tra (mb (incr_mb ?mb 0.205)))
   (assert (strappo-III-grado-prob-deamb-si)))

   

   



   

   
   
   

   

   
   
   

   
   
   

   
   
   

   
      

   

   

   

   
   
   

   

   

   

   

   

   

   
   
   
   
   

   

   

   

   

   

   

   

   
   
   
   

   

   

   
   
   
   
      

   
   
   

   

   
      
   
						
