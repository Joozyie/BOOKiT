       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOKIT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOKS-FILE ASSIGN TO "BOOKS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-BOOKS.

           SELECT USERINFO-FILE ASSIGN TO "USERINFO.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-USERINFO.

           SELECT BORROWED-BOOKS-FILE ASSIGN TO "BORROWEDBOOKS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-BORROWED-BOOKS.

       DATA DIVISION.
       FILE SECTION.
       FD BOOKS-FILE.
       01 BOOKS-RECORD.
           02 BOOK-ID PIC 9(4).
           02 FILLER PIC X(2).
           02 BOOK-TITLE PIC X(30).
           02 FILLER PIC X(2).
           02 BOOK-AUTHOR PIC X(30).
           02 FILLER PIC X(2).
           02 BOOK-STATUS PIC X(1).

       FD USERINFO-FILE.
       01 USERINFO-RECORD.
           02 USER-ID PIC 9(4).
           02 FILLER PIC X(2).
           02 USER-NAME PIC X(30).
           02 FILLER PIC X(2).
           02 USER-ADDRESS PIC X(50).
           02 FILLER PIC X(2).
           02 USER-CONTACT PIC X(15).

       FD BORROWED-BOOKS-FILE.
       01 BORROWED-BOOKS-RECORD.
          02 BORROWED-USER-ID PIC 9(4).
          02 FILLER          PIC X(2).
          02 BORROWED-BOOK   PIC 9(4).
          02 FILLER          PIC X(2).
          02 DATE-BORROWED   PIC X(10).
       

       WORKING-STORAGE SECTION.
       01 EOF-FLAG PIC X(1) VALUE 'N'.
       01 FS-BOOKS PIC 9(2).
       01 FS-USERINFO PIC 9(2).
       01 FS-BORROWED-BOOKS PIC 9(2).

       01 USER-OPTION PIC 9(1).
       01 USER-SUBOPTION PIC 9(1).
       01 BOOKS-COUNT PIC 9(2).
       01 BORROW-LIMIT PIC 9(2) VALUE 2.

       01 BOOKS-RECORDS OCCURS 100 TIMES.
           02 ID-BOOK-ARR PIC 9(4).
           02 ARR-BOOK-TITLE PIC X(30).
           02 ARR-BOOK-AUTHOR PIC X(30).
           02 ARR-BOOK-STATUS PIC X(1).

       PROCEDURE DIVISION.
           PERFORM Display-Menu.
           ACCEPT USER-OPTION.

           EVALUATE USER-OPTION
               WHEN 1 PERFORM Borrow-Menu
               WHEN 2 PERFORM Return-Menu
               WHEN OTHER
                   DISPLAY "Invalid option. Exiting program."
           END-EVALUATE.

           STOP RUN.

       Display-Menu.
           DISPLAY "Menu:"
           DISPLAY "1. Borrow"
           DISPLAY "2. Return"
           DISPLAY "Choose option (1-2): " WITH NO ADVANCING.

       Borrow-Menu.
           PERFORM Display-Borrow-Options.
           ACCEPT USER-SUBOPTION.

           EVALUATE USER-SUBOPTION
               WHEN 1 PERFORM Display-Genres
               WHEN 2 PERFORM Borrow-Books
               WHEN 3 PERFORM Display-Borrowed-Books
               WHEN OTHER
                   DISPLAY "Invalid suboption."
           END-EVALUATE.

       Return-Menu.
           PERFORM Display-User-Info.
           PERFORM Display-Return-Options.
           ACCEPT USER-SUBOPTION.

           EVALUATE USER-SUBOPTION
               WHEN 1 PERFORM Display-Borrowed-Books
               WHEN OTHER
                   DISPLAY "Invalid suboption."
           END-EVALUATE.

       Display-Borrow-Options.
           DISPLAY "Borrow Options:"
           DISPLAY "1. Browse genres"
           DISPLAY "2. Borrow books"
           DISPLAY "3. Display borrowed books"
           DISPLAY "Choose suboption (1-3): " WITH NO ADVANCING.

       Display-Return-Options.
           DISPLAY "Return Options:"
           DISPLAY "1. Display borrowed books"
           DISPLAY "Choose suboption (1): " WITH NO ADVANCING.

       Display-Genres.
           DISPLAY "Genres:"
           DISPLAY "1. Fiction"
           DISPLAY "2. Non-fiction"
           DISPLAY "3. Educational"
           DISPLAY "4. Romance/Drama"
           DISPLAY "5. Horror"
           DISPLAY "Choose genre (1-5): " WITH NO ADVANCING.

           ACCEPT USER-SUBOPTION.

           PERFORM Borrow-Books.

       Borrow-Books.
           PERFORM Read-Books-File
           MOVE 0 TO BOOKS-COUNT.
       
           PERFORM VARYING BOOKS-COUNT FROM 1 BY 1 UNTIL BOOKS-COUNT > 5
               DISPLAY "Book ID: " 
               ID-BOOK-ARR(BOOKS-COUNT)
               DISPLAY "Title: " 
               ARR-BOOK-TITLE(BOOKS-COUNT)
               DISPLAY "Author: " 
               ARR-BOOK-AUTHOR(BOOKS-COUNT)
               DISPLAY "Status: " 
               ARR-BOOK-STATUS(BOOKS-COUNT)
               DISPLAY "------------------------"
           END-PERFORM.
       
           PERFORM Borrow-Confirmation
           .
       
       Borrow-Confirmation.
           DISPLAY "Will you Borrow these? (Y/N): " WITH NO ADVANCING.
           ACCEPT USER-SUBOPTION.
       
           IF USER-SUBOPTION = 'Y'
               PERFORM Record-Borrowed-Books
           ELSE
               DISPLAY "Borrowing canceled."
           END-IF.
           
       
       Record-Borrowed-Books.
           MOVE USER-ID TO BORROWED-BOOKS-RECORD.
           MOVE FUNCTION CURRENT-DATE TO BORROWED-BOOKS-RECORD.
       
           PERFORM VARYING BOOKS-COUNT FROM 1 BY 1 UNTIL BOOKS-COUNT > 5
               IF ARR-BOOK-STATUS(BOOKS-COUNT) = 'A'
                   MOVE ID-BOOK-ARR(BOOKS-COUNT) 
                       TO BORROWED-BOOKS-RECORD.BORROWED-BOOK.
                   WRITE BORROWED-BOOKS-RECORD
                       AT END
                           DISPLAY "Error writing to BORROWEDBOOKS.TXT."
                           MOVE 1 TO FS-BORROWED-BOOKS
                           EXIT PROGRAM
                       NOT INVALID KEY
                           DISPLAY "Book ID " ID-BOOK-ARR(BOOKS-COUNT)
                   END-WRITE.
               END-IF
           END-PERFORM.
       
                     
       Display-Borrowed-Books.
           DISPLAY "Borrowed Books:"
           PERFORM Read-Borrowed-Books.

           PERFORM VARYING BOOKS-COUNT FROM 1 BY 1 UNTIL BOOKS-COUNT > 5
               DISPLAY "Book ID: " 
               ID-BOOK-ARR(BOOKS-COUNT)
               DISPLAY "Title: " 
               ARR-BOOK-TITLE(BOOKS-COUNT)
               DISPLAY "Author: " 
               ARR-BOOK-AUTHOR(BOOKS-COUNT)
               DISPLAY "Status: " 
               ARR-BOOK-STATUS(BOOKS-COUNT)
               DISPLAY "------------------------"
           END-PERFORM.



       Display-User-Info.
           DISPLAY "User ID: " USER-ID
           DISPLAY "User Name: " USER-NAME
           DISPLAY "User Address: " USER-ADDRESS
           DISPLAY "User Contact: " USER-CONTACT.

       Read-Books-File.
           MOVE 0 TO BOOKS-COUNT.

           OPEN INPUT BOOKS-FILE.
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ BOOKS-FILE INTO BOOKS-RECORD
                   AT END SET EOF-FLAG TO 'Y'
               END-READ.

               IF EOF-FLAG = 'N'
                   ADD 1 TO BOOKS-COUNT.
                   MOVE BOOKS-RECORD.BOOK-ID TO ID-BOOK-ARR(BOOKS-COUNT).
                   MOVE BOOKS-RECORD.BOOK-TITLE TO ARR-BOOK-TITLE(BOOKS-COUNT).
                   MOVE BOOKS-RECORD.BOOK-AUTHOR TO ARR-BOOK-AUTHOR(BOOKS-COUNT)
                   MOVE BOOKS-RECORD.BOOK-STATUS TO ARR-BOOK-STATUS(BOOKS-COUNT).
               END-IF
           END-PERFORM.

           CLOSE BOOKS-FILE.

       Read-Borrowed-Books.
           MOVE 0 TO BOOKS-COUNT.

           OPEN INPUT BORROWED-BOOKS-FILE.
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ BORROWED-BOOKS-FILE INTO BOOKS-RECORD
                   AT END SET EOF-FLAG TO 'Y'
               END-READ.

               IF EOF-FLAG = 'N'
                   ADD 1 TO BOOKS-COUNT.
                   MOVE BOOKS-RECORD.BORROWED-BOOK TO ID-BOOK-ARR(BOOKS-COUNT).
               END-IF
           END-PERFORM.

           CLOSE BORROWED-BOOKS-FILE.
