       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOOKiT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Menu-Option PIC 9.
       01 Borrow-Limit PIC 9 VALUE 2.
       01 Borrowed-Books-Count PIC 9 VALUE 0.
       
       01 User-Info.
          05 Username       PIC X(30).
          05 User-Address   PIC X(50).
          05 Contact-No     PIC X(15).
       
       01 Book-Info.
          05 Book-ID        PIC 9(4).
          05 Book-Title     PIC X(30).
          05 Book-Writer    PIC X(20).
          05 User-Status    PIC X(1) VALUE 'A'.
       01 Borrowed-Index PIC 9(3).
       01 Borrowed-Books OCCURS 2 TIMES.
          05 Borrowed-ID   PIC 9(4).
          05 Borrowed-Title PIC X(20).
       
       01 Response PIC X(3).
       01 User-Input-ID PIC 9(4).
       01 Book-Found PIC X(1) VALUE 'N'.
       
       PROCEDURE DIVISION.
       PERFORM Fill-User-Form
       DISPLAY "     "
       DISPLAY "     "
       DISPLAY "     "
       PERFORM Display-Inventory.
       DISPLAY "     "
       DISPLAY "     "
       DISPLAY "     "
       DISPLAY "============================================"
       DISPLAY "             WELCOME TO BOOKiT!"
       DISPLAY "         What would you like to do?".
       Main-Menu.
          DISPLAY "--------------------------------------------"
          DISPLAY "      Menu |1|Borrow.|2|Return.|0|Exit.".
          ACCEPT Menu-Option.
          DISPLAY "============================================"

          EVALUATE Menu-Option
              WHEN 1
                  PERFORM Borrow-Books
              WHEN 2
                  PERFORM Return-Books
              WHEN 0
                  PERFORM Display-User-Info
                  PERFORM Display-Borrowed-Books
                  DISPLAY "*****Thank you for using our Program!*****"
                  STOP RUN
              WHEN OTHER
                  DISPLAY "*****Invalid option. Please try again*****"
                  PERFORM Main-Menu
          END-EVALUATE.
       
          STOP RUN.
       
       Borrow-Books.
           DISPLAY "Borrow by entering the ID of the book |0|Exit".
           ACCEPT User-Input-ID.
       
           IF User-Input-ID = 0
               PERFORM Main-Menu
           ELSE
               IF User-Input-ID >= 1001 AND User-Input-ID <= 1012
                   PERFORM Check-Borrow-Limit
                   PERFORM Borrow-Book-Details
                   MOVE User-Input-ID 
                       TO Borrowed-Books(Borrowed-Books-Count + 1)
                   ADD 1 TO Borrowed-Books-Count
                   MOVE 'B' TO User-Status
                   PERFORM Display-Borrowed-Books
                   DISPLAY "   *****Book borrowed successfully!*****"
                   DISPLAY "==========================================="
               ELSE
                   DISPLAY "==========================================="
                   DISPLAY "             ID not available"
                   DISPLAY "==========================================="
                   PERFORM Borrow-Books
               END-IF
           END-IF.
       
           PERFORM Main-Menu.

       Check-Borrow-Limit.
          IF Borrowed-Books-Count >= Borrow-Limit
              DISPLAY "============================================"
              DISPLAY "     WARNING: You can only borrow ", Borrow-Limit
              DISPLAY "============================================"
              PERFORM Main-Menu
          END-IF.
       
       Borrow-Book-Details.
          PERFORM Varying Book-ID FROM 1001 BY 11 UNTIL Book-ID > 1122
              IF Book-ID = User-Input-ID
                  PERFORM Set-Book-Details
                  EXIT PERFORM
              END-IF
          END-PERFORM.
       
       Set-Book-Details.
          EVALUATE User-Input-ID
              WHEN 1001
                  MOVE "One Piece" TO Book-Title
                  MOVE "Eiichiro Oda" TO Book-Writer
              WHEN 1002
                  MOVE "A Game of Thrones" TO Book-Title
                  MOVE "George R. R. Martin" TO Book-Writer
              WHEN 1003
                  MOVE "Percy Jackson" TO Book-Title
                  MOVE "Rick Riordan" TO Book-Writer
              WHEN 1004
                  MOVE "Pride and Prejudice" TO Book-Title
                  MOVE "Jane Austen" TO Book-Writer
              WHEN 1005
                  MOVE "The Rain in Pureza" TO Book-Title
                  MOVE "Gwy Saludes" TO Book-Writer
              WHEN 1006
                  MOVE "Threads of Pain" TO Book-Title
                  MOVE "J. Leiden" TO Book-Writer
              WHEN 1007
                  MOVE "Delusions of Agony" TO Book-Title
                  MOVE "Lucia West" TO Book-Writer
              WHEN 1008
                  MOVE "Angel's Guide" TO Book-Title
                  MOVE "Kareem Abdul Jabar" TO Book-Writer
              WHEN 1009
                  MOVE "Basic Lang Algebra" TO Book-Title
                  MOVE "Marshall D. Teach" TO Book-Writer
              WHEN 1010
                  MOVE "Five Nights of Pain" TO Book-Title
                  MOVE "Scott Cawthon" TO Book-Writer
              WHEN 1011
                  MOVE "Frankenstein" TO Book-Title
                  MOVE "Mary Shelley" TO Book-Writer
              WHEN 1012
                  MOVE "Chi no Wadachi" TO Book-Title
                  MOVE "Shūzō Oshimi" TO Book-Writer
              WHEN OTHER
                  MOVE "Unknown" TO Book-Title
                  MOVE "Unknown Author" TO Book-Writer
          END-EVALUATE.

       Display-Borrowed-Books.
          DISPLAY "Books".
          DISPLAY "ID    ".
              
          PERFORM Varying Borrowed-Index 
              FROM 1 BY 1 UNTIL Borrowed-Index > Borrowed-Books-Count
                  DISPLAY Borrowed-ID(Borrowed-Index)
          END-PERFORM.
          DISPLAY "********************************************".

       
       Return-Books.
           IF Borrowed-Books-Count > 0
               PERFORM Display-User-Info
               DISPLAY "***BOOKS RETURNED***"
               PERFORM Display-Borrowed-Books
           ELSE
               SET Borrowed-Index TO 0.
               SET Borrowed-Books-Count TO 0.
               PERFORM Display-User-Info
               DISPLAY "***BOOKS RETURNED***"
               PERFORM Display-Borrowed-Books
               PERFORM Borrow-Again.

       Borrow-Again.
          DISPLAY "Do you want to borrow again?"
          DISPLAY "|1|Yes or |2|No".
          ACCEPT Menu-Option.
       
          EVALUATE Menu-Option
             WHEN 1
                PERFORM Main-Menu
             WHEN 2
                DISPLAY "*****Thank you for using our Program!*****".
                STOP RUN.

       Display-Inventory.
          DISPLAY "********************************************"
          DISPLAY "            Inventory of Books".
          DISPLAY "********************************************"
          DISPLAY "1001"
          DISPLAY "Genre : Action/Adventure"
          DISPLAY "Title : One Piece"
          DISPLAY "Author: Eiichiro Oda"
          DISPLAY "--------------------------------------------"
          DISPLAY "1002"
          DISPLAY "Genre : Fantasy/Action"
          DISPLAY "Title : A Game of Thrones"
          DISPLAY "Author: George R. R. Martin"
          DISPLAY "--------------------------------------------"
          DISPLAY "1003"
          DISPLAY "Genre : Fantasy/Action"
          DISPLAY "Title : Percy Jackson: The Lightning Thief"
          DISPLAY "Author: Rick Riordan"
          DISPLAY "--------------------------------------------"
          DISPLAY "1004"
          DISPLAY "Genre : Romance"
          DISPLAY "Title : Pride and Prejudice"
          DISPLAY "Author: Jane Austen"
          DISPLAY "--------------------------------------------"
          DISPLAY "1005"
          DISPLAY "Genre : Romance"
          DISPLAY "Title : The Rain in Pureza"
          DISPLAY "Author: Gwy Saludes"
          DISPLAY "--------------------------------------------"
          DISPLAY "1006"
          DISPLAY "Genre : Romance/Drama"
          DISPLAY "Title : Threads of Pain"
          DISPLAY "Author: J. Leiden"
          DISPLAY "--------------------------------------------"
          DISPLAY "1007"
          DISPLAY "Genre : Drama"
          DISPLAY "Title : Delusions of Agony"
          DISPLAY "Author: Lucia West"
          DISPLAY "--------------------------------------------"
          DISPLAY "1008"
          DISPLAY "Genre : Educational"
          DISPLAY "Title : Angel's Guide to Becoming a Super Human"
          DISPLAY "Author: Kareem Abdul Jabar"
          DISPLAY "--------------------------------------------"
          DISPLAY "1009"
          DISPLAY "Genre : Educational"
          DISPLAY "Title : Basic Lang Algebra (Teacher's Edition)"
          DISPLAY "Author: Marshall D. Teach"
          DISPLAY "--------------------------------------------"
          DISPLAY "1010"
          DISPLAY "Genre : Horror"
          DISPLAY "Title : Five Nights of Pain"
          DISPLAY "Author: Scott Cawthon"
          DISPLAY "--------------------------------------------"
          DISPLAY "1011"
          DISPLAY "Genre : Horror"
          DISPLAY "Title : Frankenstein"
          DISPLAY "Author: Mary Shelley"
          DISPLAY "--------------------------------------------"
          DISPLAY "1012"
          DISPLAY "Genre : Horror"
          DISPLAY "Title : Chi no Wadachi"
          DISPLAY "Author: Shūzō Oshimi"
          DISPLAY "********************************************".

       Display-User-Info.
          DISPLAY "********************************************"
          DISPLAY "User Info".
          DISPLAY "Name       : " Username.
          DISPLAY "Address    : " User-Address.
          DISPLAY "Contact No.: " Contact-No.
          DISPLAY "********************************************".
       Fill-User-Form.
          DISPLAY "--------------------------------------------"
          DISPLAY "Fill out the user form".
          
          Display "Username: "
          ACCEPT Username.
          Display "Address: "
          ACCEPT User-Address.
          Display "Contact No: "
          ACCEPT Contact-No.
          DISPLAY "--------------------------------------------".
       