      * exercice assurance commenté
      * On ouvre le fichier assurances.csv,
      * représenté dans le programme par le nom logique ASSURANCE-INPUT.

      * On lit chaque ligne du fichier une par une.
      * Chaque ligne contient : code contrat, nom, produit, client, statut, dates, montant, devise…

      * on cree variable tableau ds WS (stock memoire temporaire)
      * On copie cette ligne dans une case du tableau en mémoire appelé WS-ASR-TBL.
      * Chaque ligne du tableau est appelée WS-ASR-RCD(WS-IDX).

      * On copie les champs un par un :
      * MOVE ASR-IN-CONTRACT-CODE TO WS-ASR-CONTRACT-CODE(WS-IDX)
      * (et ainsi de suite pour tous les champs)

      * On répète l’opération pour chaque ligne du fichier.
      * À chaque lecture, on avance d'une case dans le tableau :
      * ADD 1 TO WS-IDX

      * À la fin, on se retrouve avec un tableau rempli en mémoire
      * avec jusqu’à 100 lignes de contrats.

      * Ensuite, on peut afficher, trier, filtrer ou analyser les données du tableau.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. assur.                          
       AUTHOR. BernadetteC&Leocrabe225.           
       DATE-WRITTEN. 15-05-2025 (fr).             
       DATE-COMPILED. null.                       

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Renommage temporaire de assurance.csv en (alias)ASSURANCE-INPUT
      * C'est le fichier que l'on veut lire, on utilisera cet alias ds tt le programme cobol.
           SELECT ASSURANCE-INPUT
               ASSIGN TO "assurances.csv"         
      * Lecture ligne par ligne (texte, pas binaire)         
               ORGANIZATION IS LINE SEQUENTIAL.   

       DATA DIVISION.

       FILE SECTION.
      * Description du fichier d'entrée (CSV)
       FD ASSURANCE-INPUT.                        
       01 ASR-IN-RCD.                             *> Une LIGNE du fichier CSV =
           05 ASR-IN-CONTRACT-CODE         PIC 9(08).      *> Code du contrat (8 chiffres)
           05 FILLER                       PIC X(01).      *> Caractère séparateur (comme une virgule ou point-virgule)
           05 ASR-IN-CONTRACT-NAME         PIC X(14).      *> Nom du contrat
           05 FILLER                       PIC X(01).      *> Séparateur
           05 ASR-IN-PRODUCT-NAME          PIC X(14).      *> Nom du produit d'assurance
           05 FILLER                       PIC X(01).      *> Séparateur
           05 ASR-IN-CLIENT-NAME           PIC X(41).      *> Nom du client
           05 FILLER                       PIC X(01).      *> Séparateur
           05 ASR-IN-CONTRACT-STATUS       PIC X(08).      *> Statut du contrat (ex : actif)
           05 FILLER                       PIC X(01).      *> Séparateur
           05 ASR-IN-START-DATE.                          *> Date de début du contrat
               10 ASR-IN-START-YEAR        PIC 9(04).      *> Année (ex : 2023)
               10 ASR-IN-START-MONTH       PIC 9(02).      *> Mois (ex : 05)
               10 ASR-IN-START-DAY         PIC 9(02).      *> Jour (ex : 01)
           05 FILLER                       PIC X(01).      *> Séparateur
           05 ASR-IN-END-DATE.                            *> Date de fin du contrat
               10 ASR-IN-END-YEAR          PIC 9(04).
               10 ASR-IN-END-MONTH         PIC 9(02).
               10 ASR-IN-END-DAY           PIC 9(02).
           05 FILLER                       PIC X(01).      *> Séparateur
           05 ASR-IN-AMOUNT                PIC 9(07)V9(02). *> Montant du contrat (valeur avec 2 décimales)
           05 FILLER                       PIC X(01).      *> Séparateur
           05 ASR-IN-CURRENCY              PIC X(03).      *> Devise (ex : EUR, USD)

       WORKING-STORAGE SECTION.                     *> Zone mémoire temporaire
      



       01 WS-ASR-TBL.
           05 WS-ASR-RCD OCCURS 100 TIMES.          *> Tableau de 100 lignes maximum
               10 WS-ASR-CONTRACT-CODE         PIC 9(08).
               10 WS-ASR-CONTRACT-NAME         PIC X(14).
               10 WS-ASR-PRODUCT-NAME          PIC X(14).
               10 WS-ASR-CLIENT-NAME           PIC X(41).
               10 WS-ASR-CONTRACT-STATUS       PIC X(08).
               10 WS-ASR-START-DATE.
                   15 WS-ASR-START-YEAR        PIC 9(04).
                   15 WS-ASR-START-MONTH       PIC 9(02).
                   15 WS-ASR-START-DAY         PIC 9(02).
               10 WS-ASR-END-DATE.
                   15 WS-ASR-END-YEAR          PIC 9(04).
                   15 WS-ASR-END-MONTH         PIC 9(02).
                   15 WS-ASR-END-DAY           PIC 9(02).
               10 WS-ASR-AMOUNT                PIC 9(07)V9(02).
               10 WS-ASR-CURRENCY              PIC X(03).

       77 WS-IDX                               PIC 9(03).   *> Index utilisé pour le tableau (comme une position)
       77 WS-TBL-SIZE                          PIC 9(03).   *> Nombre total de lignes lues

       01 WS-EOF                               PIC 9(01).   *> Variable qui indique si on a atteint la fin du fichier
           88 WS-EOF-TRUE                      VALUE 1.     *> VRAI si fin du fichier atteinte
           88 WS-EOF-FALSE                     VALUE 0.     *> FAUX sinon

       PROCEDURE DIVISION.                              *> Cœur du programme

           PERFORM 0100-READ-FILE-BEGIN               *> Étape 1 : lire les données du fichier
              THRU 0100-READ-FILE-END.

           PERFORM 0200-WRITE-TABLE-BEGIN             *> Étape 2 : afficher les données lues
              THRU 0200-WRITE-TABLE-END.

           STOP RUN.                                  *> Fin du programme

       0100-READ-FILE-BEGIN.
           MOVE 0 TO WS-IDX.                          *> On commence à l'index 0
           SET WS-EOF-FALSE TO TRUE.                  *> On indique que le fichier n'est pas encore terminé
           OPEN INPUT ASSURANCE-INPUT.                *> On ouvre le fichier CSV en lecture

           PERFORM UNTIL WS-EOF-TRUE                  *> Tant qu'on n'est pas à la fin du fichier
               READ ASSURANCE-INPUT                   *> On lit une ligne
                   AT END
                       SET WS-EOF-TRUE TO TRUE        *> Si on atteint la fin du fichier
                   NOT AT END
                       ADD 1 TO WS-IDX                *> On augmente l'index pour passer à la prochaine ligne
                       MOVE ASR-IN-CONTRACT-CODE
                         TO WS-ASR-CONTRACT-CODE(WS-IDX)
                       MOVE ASR-IN-CONTRACT-NAME
                         TO WS-ASR-CONTRACT-NAME(WS-IDX)
                       MOVE ASR-IN-PRODUCT-NAME
                         TO WS-ASR-PRODUCT-NAME(WS-IDX)
                       MOVE ASR-IN-CLIENT-NAME
                         TO WS-ASR-CLIENT-NAME(WS-IDX)
                       MOVE ASR-IN-CONTRACT-STATUS
                         TO WS-ASR-CONTRACT-STATUS(WS-IDX)
                       MOVE ASR-IN-START-DATE
                         TO WS-ASR-START-DATE(WS-IDX)
                       MOVE ASR-IN-END-DATE
                         TO WS-ASR-END-DATE(WS-IDX)
                       MOVE ASR-IN-AMOUNT
                         TO WS-ASR-AMOUNT(WS-IDX)
                       MOVE ASR-IN-CURRENCY
                         TO WS-ASR-CURRENCY(WS-IDX)
               END-READ
           END-PERFORM.

           CLOSE ASSURANCE-INPUT.                     *> On ferme le fichier
           MOVE WS-IDX TO WS-TBL-SIZE.                *> On stocke le nombre total de lignes lues
       0100-READ-FILE-END.

       0200-WRITE-TABLE-BEGIN.
           MOVE 1 TO WS-IDX.                          *> On commence à la première ligne
           PERFORM VARYING WS-IDX FROM 1 BY 1         *> On va afficher ligne par ligne
                   UNTIL WS-IDX > WS-TBL-SIZE         *> Jusqu'à ce qu'on ait tout affiché
               DISPLAY WS-ASR-RCD(WS-IDX)             *> Affiche la ligne entière en console
           END-PERFORM.
       0200-WRITE-TABLE-END.

       0300-AFFICHE-LIGNE-BEGIN.
           DISPLAY ""                                 *> Ligne vide (non utilisée)
       0300-AFFICHE-LIGNE-END.
