*Principles of Programming Languages - Project 'MiniHaskell'*

Άγγελος Κορακίτης - 1115201900087

Στην εργασία έχουν υλοποιηθεί όλα τα μέρη όπως επίσης και το Bonus.

- Κατάλογος
  
      .
      ├── README.md
      └── minihaskell
          ├── pdfs
          ├── project22-23.pdf
          ├── src
          │   ├── Intensional.hs
          │   ├── Main.hs
          │   ├── Makefile
          │   ├── Parser.hs
          │   ├── Parser_Bonus.hs
          │   ├── Transform.hs
          │   └── Types.hs
          └── test
              └── .txt

- Σχολιασμός Κώδικα:

Transform.hs:

Αποτελεί το αρχείο που γίνεται η μετατροπή του AST σε Intermediate Representation. Συγκεκριμένα, στην συγκεκριμένη συνάρτηση υλοποιείται ο αλγόριθμος που περιγράφεται στην εκφώνηση
της εργασίας. Γενικά, η transform συνάρτηση παίρνει το FProgram που της δίνει ο Parser, βρίσκει τις συναρτήσεις που ορίζονται στο πρόγραμμα, μετατρέπει τόσο την πρώτη γραμμή-result, όσο και τα definitions, σε intermediate expressions, αποθηκεύοντας ταυτόχρονα τα ορίσματα των συναρτήσεων, ενώ τέλος, προσθέτει τις παραμέτρους των συναρτήσεων και τα ορίσματα που αυτές παίρνουν στο πρόγραμμα, με τη σειρά που αυτά εμφανίζονται. 

Η βασική συνάρτηση είναι η transformExpr :: (FExpr,[(String, Int)],[(String,[IExpr])]) -> (IExpr, [(String, Int)],[(String,[IExpr])]) η οποία μετατρέπει τύπους FExpr σε IExpr. Η συνάρτηση αυτή παίρνει ως είσοδο ένα 3-tuple και επιστρέφει ένα 3-tuple. Με αυτόν τον τρόπο επιτυγχάνεται η αναδρομικότητα στην ανάγνωση του AST η οποία χρειάζεται, ειδικότερα, σε κλήσεις συναρτήσεων που περιέχουν άλλες συναρτήσεις και πράξεις μέσα σε αυτές. Τέλος στο 3-tuple αποθηκεύονται και οι κλήσεις-δείκτες των συναρτήσεων [(String, Int)], για να γνωρίζουμε σε ποια κλήση αντιστοιχούμε τα ανάλογα ορίσματα όπως ακριβώς περιγράφεται στην εκφώνηση της εργασίας.

Οι παρακάτω συναρτήσεις χρησιμοποιούν την transformExpr για να μετατρέψουν λίστες από expressions ή definitions αντίστοιχα σε ΙΕxpressions.

transformExprs :: [FExpr] -> [(String, Int)] -> [(String,[IExpr])] -> ([IExpr], [[(String, Int)]], [[(String,[IExpr])]])
transformDef :: FDefinition -> [(String, Int)] -> (IDefinition, [(String,Int)], [(String,[IExpr])])
transformDefs :: [FDefinition] -> [(String, Int)] -> ([IDefinition], [[(String,Int)]], [[(String,[IExpr])]])

Τέλος, οι ακόλουθες συναρτήσεις χρησιμοποιούνται για την κατάλληλη μετατροπή των Actuals που έχουμε αποθηκεύσει σε (String,[IActuals]).

groupActuals :: [(String, [IExpr])] -> [(String, [IExpr])]
transformActuals :: [(String, [IExpr])] -> [FDefinition] -> IProgram
findFParameters :: [FDefinition] -> String -> [String]
findActuals :: [String] -> [[IExpr]] -> [(String, IExpr)]

Στο αρχείο περιέχονται και άλλες συναρτήσεις οι οποίες είναι βοηθητικού χαρακτήρα.


- Intensional.hs:

Στο συγκεκριμένο αρχείο υλοποιείται η εκτέλεση του νοηματικού κώδικα της transform συνάρτησης. Η συνάρτηση eval μετατρέπει ένα IProgram σε IExpr το οποίο μπορεί να είναι IBool ή INum. H συνάρτηση αυτή καλεί την evalIDef για το νοηματικο κώδικα της πρώτης γραμμής-result και αυτή με τη σειρά της την evalExpr η οποία υπολογίζει τα expressions. Η συνάρτηση evalExpr επιστρέφει (IExpr,IEnv)
όπως ακριβώς περιγράφεται από τον μηχανισμό εκτέλεσης στην εκφώνηση. Στην συνέχεια, ανάλογα με τις εκφράσεις που υπάρχουν, γίνονται οι κατάλληλοι υπολογισμοί-lookups στους ορισμούς, για να παραχθεί το επιθυμητό αποτέλεσμα.


Parser_Bonus.hs:

Χρήση του Parser:
-> Comment 'import Parser (programParser)' και uncomment 'import Parser_Bonus (programParser)'. στο αρχείο Main.hs.

Σε αυτο το αρχείο υλοποιείται ένας Parser για την Minihaskell. Ουσιαστικά ο Parser αποτελείται από δύο τμήματα, έναν Lexer και τον Parser. Ο Lexer αρχικά παίρνει την είσοδο ως ένα String και στη συνέχεια την οργανώνει σε μια λίστα από Tokens όπως αυτοί που είναι ορισμένοι παρακάτω, σύμφωνα με τους συντακτικούς κανόνες που έχουν οριστεί από την εκφώνηση. Αν κάποιο σύμβολο δεν αναγνωριστεί από τον Lexer τότε θα επιστραφεί μήνυμα σφάλματος.

data Token = 
    PPlus | PMinus | PMult | PDiv |
    PAnd | POr | PNot |
    PLtEq | PLt | PGtEq | PGt | PEq | PNeq |
    PIf | PThen | PElse | 
    PIdentifier String | PInt Int | PBool Bool
    PAssign | POpenParen | PCloseParen | PResult | PComma | PSemicolon |

Όλα τα Tokens έχουν ένα πρόθεμα με κεφαλαίο P και στην συνέχεια ακολουθεί η αριθμητική ή άλλη πράξη που αναφέρεται το κάθε Token. 

Περισσότερη επεξήγηση χρειάζονται μερικά Tokens τις τελευταίας γραμμής. Συγκεκριμένα,
- Το PResult αναφέρεται στην αρχική γραμμή 'result = ...'.
- Το PAssign εκφράζει την ανάθεση, δηλαδή το '=', μετα το 'result' ή τους ορισμούς των συναρτήσεων.
- To POpenParen και PCloseParen την αριστερή '(' και δεξιά ')' παρένθεση αντίστοιχα.
- Το PSemicolon αναφέρεται στο ερωτηματικό που υπάρχει στο τέλος κάθε γραμμής και συμβολίζει το τέλος της συγκεκριμένης γραμμής.

Εν συνεχεία, ο Parser μετατρέπει αυτή την λίστα από Tokens σε AST (Abstract Syntax Tree).

Γενικότερα, γίνεται έλεγχος για την τήρηση των συντακτικών κανόνων. Όπως αναγράφεται και στον κώδικα, η συνάρτηση programParser επιστρέφει Either String FProgram , γίνεται δηλαδή το parsing τo προγράμματος, καταναλώνονται Token απο τη λίστα και στο τέλος πρέπει να είναι κενή, αν είναι επιστρέφεται FProgram αλλιώς μήνυμα σφάλματος. Ταυτόχρονα, σε διάφορα σημεία του κώδικα χρησιμοποιείται η συνάρτηση error της βιβλιοθήκης GHC.err η οποία σε περίπτωση σφάλματος, πχ παρένθεση η οποία δεν κλείνει, θα διακόψει την εκτέλεση του προγράμματος και θα επιστρέψει το αντίστοιχο σφάλμα.

Ο Parser χρησιμοποιεί διάφορες συναρτήσεις για τον υπολογισμό του AST. Οι βασικές συναρτήσεις είναι ή parseResult που κάνει parse την result έκφραση αλλά και η parseDefinitions, που αντίστοιχα κάνει parse τους ορισμούς των συναρτήσεων. Αυτές με την σειρά τους καλούν άλλες συναρτήσεις για τον υπολογισμό των AST. Η parseExpression υπολογίζει εκφράσεις, καλεί την parseFactor και την parseTerm. Στην parseFactor γίνονται parsing τελεστές όπως αριθμοί, μεταβλητές, κλήσεις συναρτήσεων κλπ, ενώ στην parseTerm αναλύονται τελεστές-όροι, +,-,/,...,AND,OR,...
Ουσιαστικά οι συναρτήσεις παίρνουν μια λίστα από Token και επιστρέφουν το τμήμα που αναλύσανε, FExpr, και το την υπόλοιπη λίστα που δεν έχουν αναλύσει.
