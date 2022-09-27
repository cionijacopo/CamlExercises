(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 1 |*)
(*---------------*)
(* 1) Definire una Funzione che prende una lista e un valore n, restituendo i primi n elementi della
lista *)

let rec give l n= match l with
    []->[]
  | x::xs -> if n>0 then x::(give xs (n-1)) else give [] n;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 2 |*)
(*---------------*)
(* 2) Funzione che prende una lista e restiuisce true se è ordinata in modo non decrescente (dove ogni
elemento è >= del precedente) *)

let rec ordinamento l= match l with
    [x] -> true
  | x::y::ys -> if x<=y then ordinamento (y::ys) else false ;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 3 |*)
(*---------------*)
(* 3) Definire una funzione drop che data una lista e un numero n restituisce la lista di tutti gli
elementi esclusi i primi n elementi *)

let rec drop l n = match l with
    [] -> []
  | x::xs -> if n>0 then drop xs (n-1) else (x::xs);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 4 |*)
(*---------------*)
(* 4) Definire una funzione ennesimo che, restituisca l'elemento in posizione n della lista *)

let rec ennesimo l n= match l with
    [x] when n=0 -> x
  | x::xs -> if n>0 then ennesimo xs (n-1) else x;;
(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 5 |*)
(*---------------*)
(* 5) Definire ora una funzione sommacostante che prende una lista di coppie e verifica che la somma
degli elementi di ogni coppia sia sempre uguale per tutti gli elementi della lista. *)

let somma_costante l = match l with
    [(x, y)] -> true
  | (x, y)::xs -> let rec somma_costante_aux l a = match l with
        [(x, y)] -> if x+y=a then true else false
      | (x, y)::xs -> if x+y =a then somma_costante_aux xs a else false
      in somma_costante_aux xs (x+y);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 6 |*)
(*---------------*)
(* 6)Definire una funzione doppio che riceve una lista come argomento e restituisce true se
ogni elemento della lista (eccetto il primo) è pari al doppio dell’elemento che lo precede. Restituisce
false altrimenti.   doppio [2;4;8;16;32] = true *)

let rec doppio l = match l with
    [x] -> true
  | x::y::ys -> if y=2*x then doppio(y::ys) else false;;


(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 7 |*)
(*---------------*)
(* 7)Scrivere una funzione twice che controlli se un elemento a compaia esattamente due volte nella
lista. Se così restituisce true, altrimenti false. *)

let twice list n =
  let rec conta l n a = match l with
    | [] -> if a=2 then true else false
    | x::xs -> if x=n then conta xs n (a+1) else conta xs n a
  in conta list n 0;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 8 |*)
(*---------------*)
(* 8) Data una lista di liste di interi fa la somma di tutti gli elementi di ogni lista *)

let rec somma_elementi l = match l with
    [x] -> x
  | x::xs -> x+ somma_elementi xs;;

let rec somma_liste l = match l with
    [] -> []
  | x::xs -> (somma_elementi x) :: (somma_liste xs);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*---------------*)
(*| ESERCIZIO 9 |*)
(*---------------*)
(* 9) Definire una Funzione ricorsiva che cancelli tutti gli elementi ripetuti (contigui e non) *)

let rec member l n= match l with
    []->false
  | x::xs -> if x=n then true else member xs n ;;

let rec elimina_aux l n = match l with
    [x] when x=n -> []
  | x::xs -> if x=n then elimina_aux xs n else x::(elimina_aux xs n);;

let rec elimina_ripetuti l = match l with
    [x]-> x
  | x::xs -> if member xs x then elimina_aux xs x else x::(elimina_ripetuti xs);;

(*Da sistemare ma la logica dovrebbe funzionare bene*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*----------------*)
(*| ESERCIZIO 10 |*)
(*----------------*)
(* 10) Inserire un elemento in una lista ordinata (non decrescente) in modo che il risultato sia ancora
una lista ordinata *)

let rec inserimentoord list n = match list with
    [] -> []
  | [x] -> if x<=n then x::(n::[]) else [x]
  | x::xs -> if n<=x then n::(x::xs) else x :: inserimentoord xs n;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*----------------*)
(*| ESERCIZIO 11 |*)
(*----------------*)
(* 11) Si definisca in CAML, utilizzando la ricorsione esplicita, una funzione
cancellacoppie : (int * int) list -> (int * int) list
che, data una lista di coppie, elimina gli elementi della lista la cui somma degli elementi è 10 *)

let rec cancella_coppie l = match l with
    [(x, y)] -> if x+y = 10 then [] else [(x, y)]
  | (x, y) :: xs -> if x+y = 10 then cancella_coppie xs else (x, y)::cancella_coppie xs;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*----------------*)
(*| ESERCIZIO 12 |*)
(*----------------*)
(* 12) Definire una funzione split che prende una lista e restituisce la coppia (l1,l2 ) dove l1 è la lista
che contiene tutti i numeri negativi ed l2 è quella che contiene tutti i numeri positivi. *)

let dividi_liste l= match l with
    [] -> ([],[])
  | x::xs -> if x<0 then (x::[],[]) else ([],x::[])
  | x::y::ys -> if x<0 then let dividi_liste_aux (lis1, lis2) = dividi_liste (y::ys)
        in (x::lis1, lis2)
      else let dividi_liste_aux (lis1, lis2) = dividi_liste (y::ys)
        in(lis1, x::lis2);;
(*da finire sono troppo stanco e caml mi consuma il cervello.
 maledetto caml schifoso spero ti muoia la mamma troia *)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*-------------------*)
(*| ESERCIZIO BONUS |*)
(*-------------------*)
(*Esercizio per fare ripasso su come si utilizza la foldr. Data una lista si deve sommare tutti i suoi elementi*)
(*RICORDA: La foldr va definita volta volta perchè il compilatore non la riconosce*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let somma l = let somma_aux x y = x+y
  in foldr somma_aux 0 l;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*----------------*)
(*| ESERCIZIO 13 |*)
(*----------------*)
(* 13) Definire in CAML, senza la ricorsione esplicita, la funzione
media : int list -> int
Che, data una lista lis di interi non vuota, restituisce la media aritmetica dei valori in essa contenuti.
La funzione non è definita in caso di lista vuota. *)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let somma l= let somma_aux x y = x+y
  in foldr somma_aux 0 l;;

let lunghezza l= let lunghezza_aux x y= (y+1)
  in foldr lunghezza_aux 0 l;;

let media l = (somma l)/(lunghezza l);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*----------------*)
(*| ESERCIZIO 14 |*)
(*----------------*)
(* 14) Definire in CAML, senza usare la ricorsione esplicita, una funzione
contamax : int list -> int*int
che data una lista di interi, restituisce la coppia (max,n) dove max è il valore massimo in lis e n è il
numero di elementi che precedono la prima occorrenza di max nella lista. *)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let trova_max l = let trova_max_aux x y = if x>=y then x else y
  in foldr trova_max_aux 0 l;;
(*
let lunghezza l= let lunghezza_aux x y = (y+1)
                 in foldr lunghezza_aux 0 l;; *)

let posizione l n = let rec posizione_aux l n cont = match l with
      [x] -> if x=n then (cont+1) else (cont-1)
    | x::xs -> if x = n then (cont+1) else posizione_aux xs n (cont+1)
  in posizione_aux l n 0;;

let contamax l = ((trova_max l),(posizione l (trova_max l)) );;

(*Il programma funziona correttamente, però sono riuscito a risolverlo con la foldr solo in parte perchè non
capivo come fare la funzione posizione senza fare uso della ricorsione esplicita. Dio stambecco*)


(*-------------------------------------------------------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*----------------*)
(*| ESERCIZIO 15 |*)
(*----------------*)
(* 15) Definire una funzione ricorsiva in modo esplicito che dato un intero n e una lista,
sposta i primi n elementi della lista in coda.*)


(*-------------------------------------------------------------------------------------------------------------------------*)

(*----------------*)
(*| ESERCIZIO 16 |*)
(*----------------*)
(* 16) Data una lista di interi, costruire due liste, una con i valori negativi
e una con quelli positivi o nulli*)

let rec split list = match list with
    [] -> ([],[])
  | x::xs - > if x<n then ([x], []) else ([], [x])
  | x::y::ys -> if x<n then let splitdue (luno, ldue) = split (y::ys)
        in (x::luno, ldue)
      else  let splitdue (luno, ldue) = split (y::ys)
        in (luno, x::ldue);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*----------------*)
(*| ESERCIZIO 24 |*)
(*----------------*)

(* 24) Definire in CAML, senza usare la ricorsione esplicita, una funzione
multiset : ’a list -> (’a * int) list
che, data una lista lis di elementi ordinati in modo non decrescente, restituisce una lista
di coppie in cui ciascuna coppia contiene un elemento di lis e il numero di volte in cui
tale elemento occorre in lis. Nella lista risultante tutti i primi elementi delle coppie
devono essere distinti tra loro. Ad esempio:
multiset [2;2;2;3;3;5;6;6;7;7;7] = [(2,3);(3,2);(5,1);(6,2);(7,3)] *)



(*-------------------------------------------------------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------------------------------------------------------*)
(*------------------------------------------------*)
(*| SERIE DI ESERCIZI A CASO TROVATI SU INTERNET |*)
(*------------------------------------------------*)

(* es. Definire una funzione che, applicata ad un intero positivo n,
determini se n è potenza di 2 (la funzione riporterà un booleano).*)

let potenza n =
  if n mod 2 = 0 then true else if n=1 then true else false;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Definire una funzione ricorsiva last in modo esplicito che, applicata ad una lista,
determini l'ultimo elemento della lista.*)

let rec last list = match list with
    [] -> []
  | [x] ->x
  | x::xs -> last (tl list);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Dati due interi, restituisce una lista composta da tutti gli interi compresi tra ilprimo e il secondo,
 estremi compresi.*)

let compresi (n, m)=
  let rec aux_compresi (n, m , list) =
    if n>m then list else aux_compresi (n, m-1, m::list)
  in aux_compresi (n, m, []);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Data una lista list, restituire la sua lunghezza*)

let rec length list = match list with
    [] -> 0
  | x::xs -> 1 + length (xs);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Data una lista restituisce la lista che si ottiene eliminando l’ultimo elemento (con ricorsione esplicita)*)

let rec destroy_that_pussy l = match l with
    [] -> []
  | [x] -> []
  | x::xs -> x::(destroy_that_pussy (xs));;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Data una lista restituisce l’elemento maggiore fra tutti quelli della lista (con ricorsione esplicita)*)

let rec maggiore l = match l with
  | [x] -> x
  | x::(y::ys) -> if x>y then maggiore (x::ys) else maggiore (y::ys);;
(*-------------------------------------------------------------------------------------------------------------------------*)

(*Data la coppia di liste([x1,x2,x3,...,xn];[y1,y2,...,yn])restituisce la lista delle coppie[(x1,y1),(x2,y2),...,(xn,yn)]*)

let rec coppia_liste l = match l with
    ([x], [y]) -> [(x, y)]
  | (x::xs, y::ys) -> [(x, y)] @ coppia_liste(xs, ys);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Eseguire l'esercizio precedente procedendo all'inverso. Data la lista delle coppie restituire..... *)

let rec lista_coppie l = match l with
    [(x, y)] -> ([x], [y])
  | [(x, y):: (ys)] -> ([x], [y]) (*è da finire perchè caml figlio di una troia merdosa e cagna non funziona*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Scrivere una funzione copy: int*’a -> ’a list che, applicata ad una coppia(n,x),
 restituisca la lista di lunghezza n i cui elementi sono tutti uguali a x.*)

let rec copy (n ,x)=
  if n=0 then [] else x::copy(n-1, x);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Scrivere un predicato nondec: int list -> bool che, applicato ad una lista list,
 restituisca true se gli elementi di list sono in ordine non decrescente,false altrimenti*)

let rec nondec l = match l with
    [x] -> true
  | x::y::ys -> if x>y then false else nondec(y::ys);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Scrivere una funzione che, applicata ad una lista xs=[x1;x2;...;xn], duplichi ogni elemento della lista,
cioè restituisca[x1;x1;x2;x2;...;xn;xn]. (Con ricorsione esplicita)*)

let rec duplica l = match l with
    [x] -> [x;x]
  | x::xs -> x::(x::duplica(xs));;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Scrivere una funzione position: ’a list* ’a -> int tale che position(list,x) restituisca la posizione della prima occorrenza
di x in list(contando a partire da 0, il contatore per la posizione è uguale a quello che si usava in c) se x occorre in list (Con ricorsione epliscita)*)

let position (l, n)=
  let rec position_aux (l, n, cont) = match l with
      [x] -> if x=n then cont else 0 (*Ho messo 0 ma in realtà servirebbe qualcosa per dire che se non trova l'occorenza non restituisce la posizione. Con false non funziona.*)
    | x::xs -> if x=n then cont else position_aux((xs), n, (cont+1))
  in position_aux(l, n, 0);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Scrivere una funzione alternate: ’a list -> ’a list che, applicata aduna lista list, restituisca la lista contenente tutti e
solo gli elementi di list che si trovano in posizione dispari. Per convenzione, il primo elemento di una lista si trova in posizione 0
(Con ricorsione esplicita)*)

let pari x = if x mod 2 = 0 then true else false;;

let alternate l =
  let rec alternate_aux (l, a) = match l with
      [x] -> if (pari a) then [] else [x]
    | x::xs -> if (pari a) then alternate_aux (xs, (a+1)) else x::(alternate_aux (xs, (a+1)))
  in alternate_aux (l, 0);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Scrivere un programma che, data una lista di liste di interi, restituisca il valore minimo tra i massimi di ciascuna lista*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Si definisca in CAML una funzione ricorsiva in modo esplicito, inserisci :
 ’a list -> ’a -> ’a -> ’a list
tale che inserisci lis x y inserisce nella lista lis una nuova occorrenza dell’elemento
x immediatamente dopo l’ultima occorrenza dell’elemento y.
Se y non e presente nella lista,x non viene inserito.*)

let rec member n list = match list with
    []->false
  | x::xs-> if x=n then true else member n xs


let rec inserisci list n a = match list with
    []->[]
  | [x]->if x=n then x::(a::[]) else [x]
  | x::xs-> if x=n then if not (member n xs) then x::(a::xs) else x:: inserisci (xs n a) else x::inserisci (xs n a);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*
                                                   ______      ___      .___  ___.  __
                                                  /      |    /   \     |   \/   | |  |
                                                 |  ,----'   /  ^  \    |  \  /  | |  |
                                                 |  |       /  /_\  \   |  |\/|  | |  |
                                                 |  `----. /  _____  \  |  |  |  | |  `----.
                                                  \______|/__/     \__\ |__|  |__| |_______|
                                                                                        ___,------,
                                                _,--.---.                         __,--'         /
                                                ,' _,'_`._ \                    _,-'           ___,|
                                                ;--'       `^-.                ,'        __,---'   ||
                                                ,'               \             ,'      _,-'          ||
                                                /                  \         _,'     ,-'              ||
                                                :                    .      ,'     _,'                 |:
                                                |                    :     `.    ,'                    |:
                                                |           _,-      |       `-,'                      ::
                                                ,'____ ,  ,-'  `.   , |,         `.                     : \
                                                ,'    `-,'       ) / \/ \          \                     : :
                                                |      _\   o _,-'    '-.           `.                    \ \
                                                `o_,-'  `-,-' ____   ,` )-.______,'  `.                   : :
                                                \-\    _,---'    `-. -'.\  `.  /     `.                  \  \
                                                / `--'             `.   \   \:        \                  \,.\
                                                (              ____,  \  |    \\        \                 :\ \\
                                                )         _,-'    `   | |    | \        \                 \\_\\
                                                /      _,-'            | |   ,'-`._      _\                 \,'
                                                `-----' |`-.           ;/   (__ ,' `-. _;-'`\           _,--'
                                                ,'        |   `._     _,' \-._/  Y    ,-'      \      _,-'
                                                /        _ |      `---'    :,-|   |    `     _,-'\_,--'   \
                                                :          `|       \`-._   /  |   '     `.,-' `._`         \
                                                |           _\_    _,\/ _,-'|                     `-._       \
                                                :   ,-         `.-'_,--'    \                         `       \
                                                | ,'           ,--'      _,--\           _,                    :
                                                )         .    \___,---'   ) `-.____,--'                      |
                                                _\    .     `    ||        :            \                      ;
                                                ,'  \    `.    )--' ;        |             `-.                  /
                                                |     \     ;--^._,-'         |                `-._            _/_\
                                                \    ,'`---'                  |                    `--._____,-'_'  \
                                                \_,'                         `._                          _,-'     `
                                                               ,-'  `---.___           __,---'
                                                ,'             `---------'
                                                ,'
*)





(*|-------------------------------------------------|*)
(*|=================================================|*)
(*| SERIE DI ESERCIZI PRESI DA VARI APPELLI D'ESAME |*)
(*|=================================================|*)
(*|-------------------------------------------------|*)


(*--------------------------------------------|====================|-------------------------------------------------------*)
(*--------------------------------------------|APPELLO GENNAIO 2019|-------------------------------------------------------*)
(*--------------------------------------------|====================|-------------------------------------------------------*)

(*Si definisca in CAML, USANDO FOLDR, una funzione prec : ’a list -> ’a list che,
data una lista, restituisce la lista degli elementi che sono preceduti da un elemento minore.
Si precisa che il primo elemento di una lista NON `e preceduto da un elemento minore.
Esempio:prec [3;4;5;2;-1;7;3;8] = [4;5;7;8] *)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let prec l = let prec_aux x y= match y with
      [] -> x::[]
    | y::ys -> if x<y then (x::y::ys) else (x::ys)
  in foldr prec_aux [] l;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Si definisca in caml, mediante l'uso della ricorsione esplicita, l'esercizio precedente*)

let rec prec_aux lis1 = match lis1 with
    [x] -> []
  | x::y::ys -> if x<y then y::(prec_aux(y::ys)) else (prec_aux(y::ys));;

(*--------------------------------------------|=====================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO FEBBRAIO 2019|------------------------------------------------------*)
(*--------------------------------------------|=====================|------------------------------------------------------*)

(*Si definisca in CAML, USANDO la ricorsione esplicita, la funzione
end : int list -> int list
che, data una lista, restituisce la più lunga sottolista finale che non contiene il valore 1.
Esempi:end[3;1;7;1;8;10] = [8;10], end[3;4] = [3;4], end[1;3;1;5;7;1] = []*)

let rec member n l = match l with
    [] -> false
  | x::xs -> if x=n then true else member n xs;;

let rec end_aux l = match l with
    [] -> []
  | [x] -> if x=1 then [] else [x]
  | x::xs -> if x=1 && member 1 xs then end_aux xs
      else if not x=1 && member 1 xs then end_aux xs
      else if x=1 && not member 1 xs then xs
      else (x::xs);;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Si definisca in CAML, USANDO FOLDR, una funzione
init : int list -> int list
che, data una lista, restituisce la più lunga sottolista iniziale che non contiene il valore 1.
Esempi: init[3;5;1;7;1;8] = [3;5], init[3;4;5] = [3;4;5], init[1;3;] = []*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let init l = let init_aux x y = if x=1 then [] else x::y
  in foldr init_aux [] l;;

(*--------------------------------------------|====================|-------------------------------------------------------*)
(*--------------------------------------------|APPELLO GENNAIO 2018|-------------------------------------------------------*)
(*--------------------------------------------|====================|-------------------------------------------------------*)

(*Si definisca in CAML una funzione ricorsiva in modo esplicito
split_inizio : int list -> int list * int lis
tche, data una lista lis di interi, restituisce la coppia(lis1,lis2)tale che
•lis1 e lis2 sono due sottoliste (porzioni possibilmente vuote) di lis,
•lis1 concatenato a lis2 corrisponde esattamente all’intera lista lis
•e lis1 è la sottolista INIZIALE più lunga possibile i cui elementi sono disposti in ordine strettamente crescente.*)

let rec split_inizio l = match l with
    [] -> ([],[])
  | x::y::ys -> if x<y then let (lis1, lis2)= split_inizio (y::ys) (*con let (lis1, lis2) ho dichiarato due varibili nel mezzo del programma*)
        in (x::lis1, lis2)
      else ([x], y::ys);;
(*-------------------------------------------------------------------------------------------------------------------------*)

(*Si definisca in CAML, senza usare la ricorsione esplicita, una funzione
split_fine : int list -> int list * int list
che, data una lista lis di interi, restituisce la coppia(lis1,lis2)tale che
•lis1 e lis2 sono due sottoliste (porzioni possibilmente vuote) di lis,
•lis1 concatenato a lis2 corrisponde esattamente all’intera lista lis
•e lis2 è la sottolista FINALE più lunga possibile i cui elementi sono disposti in ordine strettamente crescente.
es. split_fine [1;2;3;4;5;6;3;2;3;4;5] -> ([1;2;3;4;5;6;3],[2;3;4;5]) *)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let split_fine l = let split_fine_aux x (lis1, lis2) = match (lis1,lis2) with
      ([],[]) -> ([],[x])
    | ([], y::ys) -> if y>x then ([], x::y::ys)
        else ([x], y::ys)
    | (y::ys, lis2) -> (x::y::ys, lis2)
  in foldr split_fine_aux ([],[]) l;;

(*--------------------------------------------|=====================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO FEBBRAIO 2018|------------------------------------------------------*)
(*--------------------------------------------|=====================|------------------------------------------------------*)

(*Si definisca in CAML una funzione ricorsiva in modo esplicito
zerouno : int list -> bool
che, data una lista di interi, restituisce true se la lista contiene solo occorrenze di 0 e 1,
e nellalista ci sono tanti 0 quanti 1.  La funzione restituisce false altrimenti.
Esempi: [1;0;0;0;1;1] -> true , [1;0;1;3] -> false , [1;0;0;1;0] -> false*)

let zero_uno l = let rec zero_uno_aux l a b = match l with
      [] -> if a=b then true else false
    | x::xs -> if x=1 then zero_uno_aux xs (a+1) b
        else if x=0 then zero_uno_aux xs a (b+1) else false
  in zero_uno_aux l 0 0;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Si definisca in CAML, senza usare la ricorsione esplicita, una funzione
zerouno_ordinati : int list -> bool
che, data una lista di interi, restituisce true se la lista contiene solo occorrenze di 0 e 1 ed è ordinata in modo non decrescente
(prima tutti gli 0 e poi tutti gli 1).  La funzione restituisce false altrimenti.*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let solozerouno l = let solozerouno_aux x y = if x=0 || x=1 then true else false
  in foldr solozerouno_aux false l;;

let zerouno_ordinati l = if not (solozerouno l) then false else let zerouno_ordinati_aux x y = match y with
      (true, false) -> if x=1 then (true, false) else (true, true)
    | (true, true) -> if x=0 then (true, true) else (false, true)
    | (false, true) -> (false, true)
    in let (a, b) = foldr zerouno_ordinati_aux (true, false) l
    in a;;


(*--------------------------------------------|===================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO APRILE 2018|------------------------------------------------------*)
(*--------------------------------------------|===================|------------------------------------------------------*)

(*Si definisca in CAML, senza usare la ricorsione esplicita, una funzione
prec : int list -> int -> int -> bool
che, data una lista di interi e due interi n ed m, restituisce true se nella lista tutte le occorrenze
di n precedono tutte le occorrenze di m. La funzione restituisce false altrimenti.*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let prec l n m = let prec_aux x y = match y with
      (true, []) -> (true, x::[])
    | (true, y::ys) -> if y<>m then (true, x::y::ys) else if y=m && x=n then (true, x::y::ys) else (false, x::y::ys)
    | (false, y::ys) -> (false, x::y::ys)
  in let (a, b) = foldr prec_aux (true,[]) l
  in a;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Si definisca in CAML la funzione prec dell’esercizio precedente facendo uso della ricorsione in modo esplicito.*)

let rec prec l n m = match l with
  | x::y::ys -> if x=n && y=n then prec (y::ys) n m
      else if x=n && not y=m then prec (y::ys) n m
      else if not x=n && y=m then false
      else prec (y::ys) n m;;

(*Non funziona, da ricontrollare*)

(*--------------------------------------------|===================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO GIUGNO 2018|------------------------------------------------------*)
(*--------------------------------------------|===================|------------------------------------------------------*)

(*Si definisca in CAML, usando la ricorsione esplicita, una funzione
inizio : int list -> int -> int list
che, data una lista di interi e un intero n, restituisce il massimo prefisso (la piu` lunga porzione
iniziale della lista) in cui la somma degli elementi `e strettamente minore di n.*)

let rec somma l = match l with
    [x]->x
  | x::xs -> x + somma xs;;

let rec elimina_ultimo l = match l with
    [x]->[]
  | x::xs -> x::(elimina_ultimo xs);;

let rec inizio l n = if (somma l) < n then l else inizio (elimina_ultimo l) n;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*
Si definisca in CAML, senza usare la ricorsione esplicita, una funzione
num : int list -> int -> int
che, data una lista di interi e un intero n, calcola la lunghezza del massimo suffisso (la piu`
lunga porzione finale della lista) in cui la somma degli elementi `e strettamente minore di n.
Esempio: [7;6;5;4;3;2;1] 11;; ---> 4 (perche esce la lista [4;3;2;1] e la sua lunghezza è 4) *)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let somma l = let somma_aux x y = x+y
  in foldr somma_aux 0 l;;

let length l = let length_aux x y = 1+y
  in foldr length_aux 0 l;;

let num l n = let num_aux x y = match y with
      ([], 0) -> if x<n then ([x], 0) else ([], 1)
    | (y::ys, 0) -> if (somma(x::y::ys)) < n then (x::y::ys , 0) else ([], length(y::ys))
    | ([], n) -> ([], n)
  in let (a, b) = foldr num_aux ([], 0) l
  in b;;

(*--------------------------------------------|===================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO LUGLIO 2018|------------------------------------------------------*)
(*--------------------------------------------|===================|------------------------------------------------------*)

(* Si definisca in CAML, senza usare la ricorsione esplicita, una funzione
listasomme : int list -> int list
che, data una lista di interi, restituisce la lista delle somme degli elementi che si trovano tra due occorrenze del valore 0.
Ad esempio: listasomme [1;2;0;3;4;0;5;0;8] = [7; 5], listasomme [1;2;3;0]=[], listasomme[2;3]=[], listasomme[0;2;3;0;4]=[5].*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let somma l = let somma_aux x y = x+y
  in foldr somma_aux 0 l;;

let listasomma l = let listasomma_aux x (true, lis1, lis2) = match (true, lis1, lis2) with
      (true, [], []) -> if x=0 then (true, x::[], []) else (true, [], [])
    | (true, lis1, []) -> if x=0 then (true, x::lis1, []) else (false, x::lis1, [])
    | (false, lis1, []) -> if x=0 then (true, [], (somma lis1)::[]) else (false, x::lis1, [])
    | (true, [], lis2) -> if x=0 then (true, x::[], lis2) else (false, x::[], lis2)
    | (false, lis1, lis2) -> if x=0 then (true, [], (somma lis1)::lis2) else (false, x::lis1, lis2)
  in let (a, b, c) = foldr listasomma_aux (true, [], []) l
  in c;;

(*Funziona ma non sempre entra nel match. Da ricontrollare*)

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Si definisca in CAML, usando la ricorsione esplicita, una funzione
inizio : ’a list -> ’a -> ’a list
che, data una lista lis e un elemento n, restituisce la lista degli elementi che compaiono prima
della prima occorrenza di n. Se n non `e presente, restituisce [].
Esempi: [5;4;33;2;7;99;2;7;3] 7;; --> [5;4;33;2]
        [1;2;3;4;5;6] 10;; --> [] *)

let rec member n l = match l with
    [] -> false
  | x::xs -> if x=n then true else member n xs;;

let inizio l n = let rec inizio_aux y = match y with
      ([], true) -> []
    | (x::xs, false) -> if (member n (x::xs)) then
          if x=n then inizio_aux ([], true)
          else x::(inizio_aux (xs, false))
        else inizio_aux ([], true)
  in inizio_aux (l, false);;

(*--------------------------------------------|======================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO SETTEMBRE 2018|------------------------------------------------------*)
(*--------------------------------------------|======================|------------------------------------------------------*)

(*Si definisca in CAML, senza usare la ricorsione esplicita, una funzione
init : ’a list -> ’a list -> ’a list
che, date due liste lis1 e lis2, restituisce la lista degli elementi di lis1 che precedono il primo elemento
di tale lista che `e contenuto anche in lis2. Gli elementi nella lista restituita devono rispettare l’ordine che avevano in lis1.
La funzione restituisce una lista vuota se nessun elemento di lis1 `e contenuto in lis2.
Esempio: lis1= [1;2;3;4;5] e lis2=[6;7;8;5;9] ----> [1;2;3;4]*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let member n l = let member_aux x y = match y with
      false -> if x=n then true else false
    | true -> true
  in foldr member_aux false l;;

let init lis1 lis2 = let init_aux x y = match y with
      (false, []) -> if (member x lis2) then (true, []) else (false,[])
    | (true, []) -> (true, [x])
    | (true, y::ys) -> (true, x::y::ys)
  in let (a, b) = foldr init_aux (false, []) lis1
  in b;;

(*-------------------------------------------------------------------------------------------------------------------------*)

(*Si definisca in CAML, usando la ricorsione esplicita, funzione init descritta nell’Esercizio 3,
con la differenza che in questo caso se nessun elemento di lis1 `e contenuto in lis2,
la funzione deve restituire l’intera lista lis1.*)

let rec member n l = match l with
    [] -> false
  | x::xs -> if x=n then true else member n xs;;

let rec init_ricorsiva lis1 lis2 = match lis1 with
    [] -> []
  | x::xs -> if (member x lis2) then [] else x::(init_ricorsiva xs lis2);;

(*--------------------------------------------|====================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO GENNAIO 2016|------------------------------------------------------*)
(*--------------------------------------------|====================|------------------------------------------------------*)

(*Si definisca in CAML, senza utilizzare ricorsione esplicita, una funzione
somma : int list -> int -> int in modo che (somma lis n) restituisca:
• la somma degli elementi di lis che precedono la prima occorrenza di n, se n occorre in lis
• la somma di tutti gli elementi di lis, se n non occorre in lis Ad esempio:
   somma [2; 3; 4; 5; 4; 7] 4 = 5
   somma [4; 2; 3; 4; 7] 4 = 0
   somma [2; 3; 5] 4 = 10*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let somma_totale l= let somma_aux x y = x+y
  in foldr somma_aux 0 l;;

let somma l n = let somma_aux x y = match y with
      [] -> if x=n then [] else [x]
    | y::ys -> if x=n then [] else x::y::ys
  in let a = foldr somma_aux [] l
  in somma_totale a;;

(*--------------------------------------------|=====================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO FEBBRAIO 2016|------------------------------------------------------*)
(*--------------------------------------------|=====================|------------------------------------------------------*)

(*Si definisca in CAML, senza utilizzare ricorsione esplicita, una funzione
foo : ’a list -> ’a * int * int
in modo che (foo lis) restituisca una terna (max, n, m) in cui max `e il valore massimo presente nella lista,
n `e il numero di occorrenze di max nella lista e m `e il numero di elementi della lista. La funzione non `e definita se la lista `e vuota.
N.B. Non `e consentito l’utilizzo della funzione length.*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let length l = let length_aux x y = y+1
  in foldr length_aux 0 l;;

let maxx l = let max_aux x y = if x>=y then x else y
  in foldr max_aux 0 l;;

let occ l n = let occ_aux x y = if x=n then y+1 else y
  in foldr occ_aux 0 l;;

let foo l = ((maxx l), (occ l (maxx l)), (length l));;

(*--------------------------------------------|===================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO GIUGNO 2016|------------------------------------------------------*)
(*--------------------------------------------|===================|------------------------------------------------------*)

(*Si definisca in CAML, senza utilizzare ricorsione esplicita, una funzione
split : ’a list -> ’a list * ’a list
in modo che (split xs) restituisca la coppia (l1, l2), in cui l1 contiene tutti gli elementi di xs maggiori del primo elemento di xs e
l2 contiene tutti gli elementi di xs minori o uguali al primo elemento di xs. Se xs `e vuota la funzione restituisce la coppia [ ], [ ].
Esempio: split [9;11;8;13;15;7;3] ;; ----> ([11;13;15],[8;7;3])*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let split l = if l=[] then ([],[]) else let split_aux x y = match y with
      (n, [], []) -> if x>n then (n, [x], []) else (n, [], [x])
    | (n, y::ys, []) -> if x>n then (n, x::y::ys, []) else (n, y::ys, [x])
    | (n, [], y::ys) -> if x>n then (n, [x], y::ys) else (n, [], x::y::ys)
    | (n, y::ys, z::zs) -> if x>n then (n, x::y::ys, z::zs) else (n, y::ys, x::z::zs)
    in let (a, b, c) = foldr split_aux ((hd l), [], []) (tl l)
    in (b, c);;

(*--------------------------------------------|===================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO LUGLIO 2016|------------------------------------------------------*)
(*--------------------------------------------|===================|------------------------------------------------------*)

(*Si definisca in CAML, senza utilizzare ricorsione esplicita, una funzione
minfirst : ’a list -> ’a list
in modo che (minfirst xs) sia una lista che contiene tutti gli elementi di xs, in cui le occorrenze dell’elemento minimo sono tutte in testa
(l’ordine in cui gli elementi non minimi di xs occorrono nella lista risultato `e irrilevante). Ad esempio
minfirst [2;3;4;1;3;1;6] = [1;1] @ L
dove L `e una qualunque permutazione della lista [2;3;4;3;6]*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let minn l = let min_aux x y = match y with
      [] -> [x]
    | y::ys -> if x<=y then x::ys else y::ys
  in let a = foldr min_aux [] l
  in (hd a);;

let minfirst l = let minfirst_aux x (n, lis1, lis2) = match (n, lis1, lis2) with
      (n, [], []) -> if x=n then (n, [], [x]) else (n, [x], [])
    | (n, y::ys, []) -> if x=n then (n, y::ys, [x]) else (n, x::y::ys, [])
    | (n, [], y::ys) -> if x=n then (n, [], x::y::ys) else (n, [x], y::ys)
    | (n, y::ys, z::zs) -> if x=n then (n, y::ys, x::z::zs) else (n, x::y::ys, z::zs)
  in let (a, b, c) = foldr minfirst_aux ((minn l), [], []) l
  in c@b;;

(*--------------------------------------------|======================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO SETTEMBRE 2016|------------------------------------------------------*)
(*--------------------------------------------|======================|------------------------------------------------------*)

(*Si supponga data la seguente funzione CAML
let ins el lis =
   let rec insa el l1 l2 =
      match l1 with
        [] -> l2@[el]
      | x::xs when x>=el -> l2@[el]@l1
      | x::xs when x<el  -> insa el xs (l2@[x])
in
   insa el lis [];;
Si definisca, senza utilizzare ricorsione esplicita ma utilizzando ins, la funzione sort : ’a list -> ’a list
tale che (sort lis) ordini lis in modo non decrescente.*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

(*Non ho capito cosa ci devo fare con quella funzione e quindi non fo l'esercizio. "Grandi problemi necessitano di grandi soluzioni;
problemi incomprensibili necessitano della madonna puttana"*)

(*--------------------------------------------|====================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO GENNAIO 2015|------------------------------------------------------*)
(*--------------------------------------------|====================|------------------------------------------------------*)

(*Si definisca in CAML, senza utilizzare ricorsione esplicita, una funzione insord : ‘a -> ‘a list -> ‘a list in  modo  che (insord x lis),
supponendo lis ordinata  in  modo  non  decrescente,  restituisca  la  lista  ottenuta inserendo x in lis e ordinata nello stesso modo.
Ad esempio: insord 4 [-1; 0; 3; 3; 6; 10] = [-1; 0; 3; 3; 4; 6; 10] *)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let insord n l = let insord_aux x y = match y with
      (false, [], t) -> if x <= t then (true, x::t::[], t) else (false, x::[], t)
    | (false, y::ys, t) -> if x<=t then (true, x::t::y::ys, t) else (false, x::y::ys, t)
    | (true, y::ys, t) -> (true, x::y::ys, t)
  in let (a, b, c) = foldr insord_aux (false, [], n) l
  in b;;
(*mi sono accorto dopo che la varibile t in realtà è superflua. mi faceva fatica cambiarla e riscrivere il codice*)
(*-------------------------------------------------------------------------------------------------------------------------*)

(*Scrivere in CAML una funzione multiset : ’a list -> (’a * int) list che, data una lista lis di elementi, restituisce una lista di coppie
in cui ciascuna coppia contiene un elemento di lis e il numero di volte in cui tale elemento occorre in lis.
Nella lista risultante tutti i primi elementi delle coppie devono essere distinti tra loro.
Ad esempio: multiset [20;40;30;20;50;20;30] = [(20,3); (40,1); (30,2); (50,1)]*)

(*METODO MIO*)

let rec member n l = match l with
    [] -> false
  | x::xs -> if x=n then true else member n xs;;

let rec conta n l = match l with
    [] -> 0
  | x::xs -> if x=n then 1+(conta n xs) else (conta n xs);;

let rec cancella_uguali l = match l with
    x::xs -> if (member x xs) then let rec cancella_aux n l = match l with
        [] ->

(*Da finire questo metodo*)

(*METODO BARBUTI*)

          let rec multiset lis = let rec ins el l = match l with
                [] -> [(el, 1)]
              | (x, y)::zs -> if x=el then (x, y+1)::zs else (x, y)::(ins el zs)
            in match lis with
              [] -> []
            | x::xs -> ins x (multiset xs);;

(*--------------------------------------------|=====================|------------------------------------------------------*)
(*--------------------------------------------|APPELLO FEBBRAIO 2015|------------------------------------------------------*)
(*--------------------------------------------|=====================|------------------------------------------------------*)


(* ESERCIZI A CASO*)

(*---------------------------------------------------------*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let rec somma l = match l with
    [] -> 0
  | x::xs -> x + somma(xs);;

let sumflat ll = let sumflat_aux x y = match y with
      [] -> (somma x)::[]
    | y::ys -> (somma x)::y::ys
  in foldr sumflat_aux [] ll;;

(*---------------------------------------------------------*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let rec sum_aux l = match l with
    [] -> 0
  | x::xs -> x + sum_aux (xs);;

let somma l n = let somma_aux x y = match y with
      [] -> if x=n then [] else x::[]
    | y::ys -> if x=n then [] else x::y::ys
  in let a = foldr somma_aux [] l
  in sum_aux a;;

(*---------------------------------------------------------*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let maxx l = let maxx_aux x y = if x>=y then x else y
  in foldr maxx_aux 0 l ;;

let foo l = let foo_aux x y = match y with
      (max, 0, 0) -> if x = max then (max, 1, 1) else (max, 0, 1)
    | (max, 0, m) -> if x = max then (max, 1, m+1) else (max, 0, m+1)
    | (max, n, m) -> if x = max then (max, n+1, m+1) else (max, n, m+1)
  in foldr foo_aux ((maxx l), 0, 0) l;;

(*---------------------------------------------------------*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let controlla_segni l = let controlla_segni_aux x y = match y with
      (true, []) -> (true, [x])
    | (false, []) -> (false, [x])
    | (true, y::[]) -> (true, x::y::[])
    | (false, y::[]) -> (false, x::y::[])
    | (true, y::z::[]) -> if ((y>0 && z>0) || (y<0 && z<0) || (y=0 || z=0)) then (true, [x]) else (false, [x])
    | (false, y::z::[]) -> if ((y>0 && z>0) || (y<0 && z<0)) then (true, [x])
        else if (y=0 || z=0) then (false, [x]) else (false, [x])
  in let (a, b) = foldr controlla_segni_aux (true, []) l
  in a;;

(*---------------------------------------------------------*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;


let member_bestia l = let member_bestia_aux x y = match y with
      (y, cont) -> if x = y then (y, cont+1) else (y, cont)
  in foldr member_bestia_aux ((Hd l), 0) l;;

let multiset l = let multiset_aux x y = match y with
      ([], num, cont) -> if (num = 0 && cont = 0) then ([], x, 1)
        else if num = x then ([], num, cont+1) else ([(num, cont)], x, 1)
    | (y::ys, num, cont) -> if num = x then (y::ys, num , cont+1) else ((num, cont)::y::ys, x, 1)
  in let (a, b, c) = foldr multiset_aux ([], 0, 0) l
  in (member_bestia l)::a;;

(*
                 let rec foldr f a lis = match lis with
                        [] -> a
                        | x::xs -> f x (foldr f a xs);;
                 let multiset l = let multiset_aux x y = match y with
                                                         ([], num, cont) -> if (num = 0 && cont = 0) then ([], x, 1)
                                                                            else if num = x then ([], num, cont+1) else ([(num, cont)], 0, 0)
                                                         | (y::ys, num, cont) -> if (num = 0 && cont = 0) then (y::ys, x, 1)
                                                                                 else if num = x then (y::ys, num , cont+1) else ((num, cont)::y::ys, 0, 0)
                                  in let (a, b, c) = foldr multiset_aux ([], 0, 0) l
                                  in a;;
*)

(*------------------------------------------------ESERCIZIO 3 ESAME 18/6/2019---------*)

let rec foldr f a lis = match lis with
    [] -> a
  | x::xs -> f x (foldr f a xs);;

let endx l n = let endx_aux x y = match y with
      ([], num) -> if x>0 && num<>0 then ([x], (num-1)) else ([], num)
    | (y::ys, num) -> if x>0 && num<>0 then (x::y::ys, (num-1)) else (y::ys, num)
  in let (a, b) = foldr endx_aux ([], n) l
  in a;;

(*------------------------------------------------ESERCIZIO 4 ESAME 18/6/2019---------*)

let rec length list = match list with
    [] -> 0
  | x::xs -> 1 + length (xs);;

let tl l = match l with
    [] ->[]
  | x::xs -> xs;;

let rec positivo l = match l with
    [] -> []
  | x::xs -> if x>0 then x::(positivo xs) else (positivo xs);;

let rec endxx l n = if (length(positivo l)) <= n then (positivo l) else endxx (tl(positivo l)) n;;
