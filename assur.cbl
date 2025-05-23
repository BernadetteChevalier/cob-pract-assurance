       IDENTIFICATION DIVISION.
       PROGRAM-ID. assur.
       AUTHOR. BernadetteC&Leocrabe225.
       DATE-WRITTEN. 15-05-2025 (fr).
       DATE-COMPILED. null.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSURANCE-INPUT
               ASSIGN TO "assurances.csv"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD ASSURANCE-INPUT.
       01 ASR-IN-RCD.
           05 ASR-IN-CONTRACT-CODE         PIC 9(08).
           05 FILLER                       PIC X(01).
           05 ASR-IN-CONTRACT-NAME         PIC X(14).
           05 FILLER                       PIC X(01).
           05 ASR-IN-PRODUCT-NAME          PIC X(14).
           05 FILLER                       PIC X(01).
           05 ASR-IN-CLIENT-NAME           PIC X(41).
           05 FILLER                       PIC X(01).
           05 ASR-IN-CONTRACT-STATUS       PIC X(08).
           05 FILLER                       PIC X(01).
           05 ASR-IN-START-DATE.
               10 ASR-IN-START-YEAR        PIC 9(04).
               10 ASR-IN-START-MONTH       PIC 9(02).
               10 ASR-IN-START-DAY         PIC 9(02).
           05 FILLER                       PIC X(01).
           05 ASR-IN-END-DATE.
               10 ASR-IN-END-YEAR          PIC 9(04).
               10 ASR-IN-END-MONTH         PIC 9(02).
               10 ASR-IN-END-DAY           PIC 9(02).
           05 FILLER                       PIC X(01).
           05 ASR-IN-AMOUNT                PIC 9(07)V9(02).
           05 FILLER                       PIC X(01).
           05 ASR-IN-CURRENCY              PIC X(03).
       
       WORKING-STORAGE SECTION.
       01 WS-ASR-TBL.
           05 WS-ASR-RCD OCCURS 100 TIMES.
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

       77 WS-IDX                               PIC 9(03).
       
       77 WS-TBL-SIZE                          PIC 9(03).

       01 WS-EOF                               PIC 9(01).
           88 WS-EOF-TRUE                                VALUE 1.
           88 WS-EOF-FALSE                               VALUE 0.
       PROCEDURE DIVISION.
           
           PERFORM 0100-READ-FILE-BEGIN
              THRU 0100-READ-FILE-END.

           PERFORM 0200-WRITE-TABLE-BEGIN
              THRU 0200-WRITE-TABLE-END.

           STOP RUN.

       0100-READ-FILE-BEGIN.
           MOVE 0 TO WS-IDX.
           SET WS-EOF-FALSE TO TRUE.
           OPEN INPUT ASSURANCE-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ ASSURANCE-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX
                       MOVE ASR-IN-CONTRACT-CODE
                         TO WS-ASR-CONTRACT-CODE(WS-IDX)
                       MOVE ASR-IN-CONTRACT-NAME
                         TO WS-ASR-CONTRACT-NAME(WS-IDX)
                       MOVE ASR-IN-PRODUCT-NAME
                         TO WS-ASR-PRODUCT-NAME (WS-IDX)
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
           CLOSE ASSURANCE-INPUT.
           MOVE WS-IDX TO WS-TBL-SIZE.
       0100-READ-FILE-END.

       0200-WRITE-TABLE-BEGIN.
           MOVE 1 TO WS-IDX.
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-TBL-SIZE
               DISPLAY WS-ASR-RCD(WS-IDX)
           END-PERFORM.
       0200-WRITE-TABLE-END.

       0300-AFFICHE-LIGNE-BEGIN.
           DISPLAY ""

       0300-AFFICHE-LIGNE-END.