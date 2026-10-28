
(* -----------------------------------------------------------------------------
   cr Q1 Q30 projet
   Marie Ange Nader <Marie-ange.nader@edu.univ-grenoble-alpes.fr>  
   Mira Hajj diab <Mira.Hajj-diab@edu.univ-grenoble-alpes.fr> 
   Mira Sadek <Mira.sadek@edu.univ-grenoble-alpes.fr> 
   Lilia Si Mohand <lilia.Si-Mohand@edu.univ-grenoble-alpes.fr> 
   -------------------------------------------------------------------------- *)


(*Définition des types*)

type dimension = int;; (*restreint aux entiers strictement positifs*)
type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
             | Libre 
             | Code of string (*une chaine restreinte a 3 caracteres*);;
type case_coloree = case * couleur;;
type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)         
type coup = Du of case * case | Sm of case list;;
type case_option = None | Some of case;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
  (i+j+k=0);;

let rec associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = 
  match l with
  |[]-> defaut
  |(x,y)::tl-> 
      if a=x then y
      else associe a tl defaut;; 
  
(* Q1 

1- cette condition correspond aux cases dans la zone de départ
2- les cases dans la zone d'arrivée
3- les cases dans la zone de coté en haut à gauche
4- la case au sommet du camp d'arrivée
5- la case au sommet gauche du camp de départ
6- le grand triangle qui a pour sommet le camps d'arrivé
  
Q2 *)
let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool = 
  (j >= -dim && j<= dim && k >= -dim && k<= dim) ;;

(*nous avons tester toutes les cases qui sont dans le losange et en dehors du 
losange, la fonction marche comme prévue . Voici des exemples en dim 3:*)
est_dans_losange (1, -1, 0) 3;; (*True*)
est_dans_losange (2, 2, -4) 3;; (*False*)

(* Q3

Nous avons decider de représenter l'étoile comme une réunion de 3 losanges, donc 
on a 3 expression booléenne différentes:
(j >= -dim && j<= dim && k >= -dim && k<= dim) pour le premier
(i >= -dim && i<= dim && k >= -dim && k<= dim) pour le deuxieme
(j >= -dim && j<= dim && i >= -dim && i<= dim) pour le troisième *)

let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool =
  ((j >= -dim && j<= dim && k >= -dim && k<= dim) || 
   (i >= -dim && i<= dim && k >= -dim && k<= dim) || 
   (j >= -dim && j<= dim && i >= -dim && i<= dim) ) && (i+j+k=0)
;;

(*Nous avons içi aussi testé toutes les cases de l'etoile ainsi que des 
cases en dehors, la fonctions marche comme prévue:*)

est_dans_etoile (1, -1, 0) 3;; (*True*)
est_dans_etoile (6, -6, 0) 3;; (*False*)


(* Q4 *)

let rec tourner_une_case (m:int)((x,y,z):case) : case =
  match m with
  |1 -> (x+y , y+z , z+x) (*nous avons constaté que c'est ce qui se passe lors 
                            d'une rotation d'une case 1/6 eme de fois*)
  |_ -> tourner_une_case (m-1) (x+y , y+z , z+x) (*appel récursif pour touner plus 
                                                   qu'une fois*)
;;

(* Exemple: pour la case (2, 2, -4) , ses rotations sont :(4, -2, -2); 
   (2, -4, 2); (-2, -2, 4); (-4, 2, 2) et (-2, 4, -2)
   En testant la fonctions, nous obtenons les bonnes réponses *)
tourner_une_case 1 (2, 2, -4);;

(* Q5 *)

let translate ((x,y,z):case)((v1,v2,v3):vecteur): case = 
  (x+v1,y+v2,z+v3) 
;; 


(* Q6 *)

let diff_case ((x,y,z):case)((a,b,c):case): vecteur = 
  (x-a,y-b,z-c) 
;;
diff_case (0,0,0) (0,-2,2);;

(*Q7*)

(*Si deux cases sont voisines, la somme des valeurs absolues 
des coordonnés de ces cases est égale à 2*)
let sont_cases_voisines ((x,y,z):case)((a,b,c):case):bool=
  abs(x-a)+abs(y-b)+abs(z-c)=2 
;;

(*Exemples*)

sont_cases_voisines (1,0,-1) (1, 1, -2);;
sont_cases_voisines (1,0,-1) (2, 0, -2);;
sont_cases_voisines (1,0,-1) (2, -1, -1);;
sont_cases_voisines (1,0,-1) (0, 0, 0);;
sont_cases_voisines (1,0,-1) (0, 1, -1);;
sont_cases_voisines (1,0,-1) (6,-3,-3);;


(*Q8*)

let calcul_pivot ((x,y,z):case)((a,b,c):case): case_option =
  if (x+a) mod 2 =0 && (y+b) mod 2 =0 && (z+c) mod 2 =0 then (*On vérifie l'existance d'un pivot*)
    (*calcul du milieu*)
    let xp = (x+a)/2 in
    let yp = (y+b)/2 in
    let zp = (z+c)/2 in
    Some(xp,yp,zp) 
  else
    None 
;;
(*Exemples*)
calcul_pivot (-4,3,1) (-3,2,1) ;; (*Dans ce cas le pivot n'existe pas*)
calcul_pivot (-1,-2,3) (-1,2,-1) ;;



(*Q9*)

let vec_et_dist ((x,y,z):case)((a,b,c):case) : vecteur*int = 
  let (m,n,p) = diff_case (a,b,c) (x,y,z)    in (*en utilisant cette fonction 
  et en supposant que toutes les cases sont alignés, une des coordonées sera 
  nulle,et les deux autres seront un chiffre et son opposé*)
  let d = (abs(m)+abs(n)+abs(p))/2 in 
  let v=if d<>0 then (m/d,n/d,p/d) else (0,0,0) in
  (v,d) 
;;
vec_et_dist (0, 2, -2) (0,0,0);; 
vec_et_dist (0, 2, -2) (0,0,0) = ((0,1,-1),2);;
 

  
(* Q10 *)


let tourner_liste (lis: 'a list) : 'a list=
  match lis with
  |[] -> []
  |h::tl -> tl@[h]
;; 

(*Tests*)

tourner_liste [1;2;3;4];;
tourner_liste [Vert;Rouge;Jaune];;

let rec der_liste (lis: 'a list):'a  =
  match lis with
  |[] -> failwith "listevide"
  |[x] -> x
  |h::tl -> der_liste tl 
;;

der_liste [1;2;3];;
der_liste [Jaune;Vert;Rouge;Bleu] ;;
der_liste [(0,0,0);(1,2,3);(4,5,6)];;


(* Q11 *) 

(* équations récursives :
   m=1= (i,j,k)
   m>1 = (i,j,k) + remplir_segment (m-1) (i,j+1,k-1) *) 

let rec remplir_segment (m: int) ((i,j,k):case) : case list=
  match m with 
  |1 -> [(i,j,k)]
  |m -> [(i, j, k)]@remplir_segment (m - 1) (i, j + 1, k - 1) 
;; 

(* Tests *)
remplir_segment 3 (-4,1,3) ;;
remplir_segment 4 (2,0,-2) ;;

(* Q12 *)

(* équations récursives :
   m=1= (i,j,k)
   m>1 = (i,j,k) + remplir_segment (m-1) (i-1,j+1,k) *) 

let rec remplir_triangle_bas (m: int) (c:case) : case list=
  match m,c with 
  |1,(i,j,k) -> [(i,j,k)]
  |m,(i,j,k) -> (remplir_segment m c)@(remplir_triangle_bas (m-1) (i-1,j+1,k))
;; 
(*On rempli le triangle ligne par ligne jusqu'au sommet , pour les triangles
des joueurs, l'entier m représente la dimension du plateau puisque les camps
possèdent dim lignes*)
  
remplir_triangle_bas 3 (-4,1,3) ;;

(* Q13 *)

(* équations récursives :
   m=1= (i,j,k)
   m>1 = (i,j,k) + remplir_segment (m-1) (i+1,j,k-1) *) 

let rec remplir_triangle_haut (m: int) (c:case) : case list=
  match m,c with 
  |1,(i,j,k) -> [(i,j,k)]
  |m,(i,j,k) -> (remplir_segment m c)@(remplir_triangle_haut (m-1) (i+1,j,k-1))
;; 

remplir_triangle_haut 3 (-3,4,-1);; 

(* Q14 *)

let rec colorie (coul:couleur)(cl:case list) : case_coloree list=
  match cl with 
  |[]->[]
  |[(i,j,k)] -> [((i,j,k),coul)]
  |h::tl -> [(h,coul)]@(colorie coul tl) 
;;

colorie Vert [(1,-1,0);(-3,5,-2);(-4,1,3)] ;;
(******************************************************************************)
(* fonctions pour l'affichage *)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
  if m = (4 * dim) + 1 then " " (*fin de ligne*)
  else
    let c = transfo m n in
    if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
      "   "^ affiche_ligne n (m + 1) config
    else (*ceci est une case ou bien en dehors du plateau*)
      (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
  let rec affiche_aux n =
    if n = - 2 * dim - 1 then ()
    else
      begin
        print_endline (affiche_ligne n (-4*dim-1) config);
        print_endline "\n";
        affiche_aux (n - 1)
      end
  in
  affiche_aux (2*dim+1)
;;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;

(******************************************************************************)

(* Q15  *) 

(* fonction intermédière qui permet de tourner une liste de cases ce qui facilite
   l'écriture de la fonction principale*)
let rec touliste (liste1 : case_coloree list) (liste2:couleur list)  : case_coloree list = 
  match liste1 with 
  |[] -> [] 
  |(x,y)::tl -> 
      match x with
      |(i,j,k) -> [tourner_une_case (6/List.length liste2) (i,j,k), y]@touliste tl liste2
;;

touliste [((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge);((2,0,-2),Vert)];;

  
let tourner_config ((liste1,liste2,dim) : configuration) : configuration = 
  match liste1,liste2,dim with 
  |[],[],_ -> ([],[],dim)
  |l1,l2,_ -> ((touliste l1 l2), (tourner_liste l2), dim)
;; 
  

tourner_config ([((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge)],[Rouge;Vert;Jaune],3);;




(* Q16 *)
(*Pour réaliser la fonctio principale de cette question ,nous qvons eu besoin
  de pas pal de fonctions intermédières*)

(*On avait besoin de la meme fonction de tounerliste mais qui prend en parametre
  juste une liste de cases sans couleurs*)
let rec touliste2 (liste1 : case list) (liste2:couleur list)  : case list = 
  match liste1 with 
  |[] -> [] 
  |x::tl -> match x with
    |(i,j,k) -> [tourner_une_case (6/List.length liste2) (i,j,k)]@touliste2 tl liste2 
;;

(*Cette fonction génere la liste des coordonnéés des camps des joueurs en une
  seule liste*)
let rec genere (cpt:int)(dim:int)(lis:couleur list): case list =
  match cpt with
  |1-> remplir_triangle_bas dim (-dim-1,1,dim)
  |_ -> remplir_triangle_bas dim (-dim-1,1,dim) @ (touliste2 (genere (cpt-1) dim lis) lis)
;;
(* Test *)
genere 3 3 [Vert;Rouge;Jaune];;
genere 2 2 [Vert;Rouge];;


(* Ces fonctions divisent la liste des coordonnées en parties égales pour
   représenter chaque couleurs *)

let rec divise (n:int) (lst:'a list): 'a list * 'a list =
  if n <= 0 then [], lst
  else match lst with
    | [] -> [], []
    | hd :: tl ->
        let prefix, suffix = divise (n - 1) tl in
        hd :: prefix, suffix
;; 
let diviseplusieur (m:int) (lis:case list) : case list list = 
  let rec boucle acc compte reste =
    match compte, reste with
    | 0, _ -> List.rev acc
    | _, [] -> List.rev acc
    | _, _ ->
        let partie, res = divise ((List.length lis + m - 1) / m) reste in
        boucle(partie :: acc) (compte - 1) res in
  boucle [] m lis 
;; 
(* Test *)
diviseplusieur 3 [(0,0,0);(1,1,1);(2,2,2);(3,3,3);(4,4,4);(5,5,5);(6,6,6)];;

(* Une autre version de la fonctionn colorie mais avec  une liste de couleurs *)
let rec colorie2 (cases: case list list) (lis2: couleur list): case_coloree list= 
  match cases, lis2 with
  | [], _ -> []
  | _, [] -> []
  | (hd::tl), (hd2::tl2) -> (colorie hd2 hd) @ (colorie2 tl tl2)
;; 

(* Pour les joueurs, donner une couleure a chaque Code of string *)
let rec donnercoul (num: int) (lis: couleur list): couleur list =
  match num, lis with
  | 0, _ -> []
  | _, [] -> failwith "pas assez de couleurs"
  | n, hd::tl -> hd :: donnercoul (n - 1) tl
;;


(* finalement, la fonction principale *)

let remplir_init (joueurs: couleur list) (dim: int): configuration = 
  let couls = donnercoul (List.length joueurs) [Rouge; Vert; Jaune; Noir; Bleu; Marron] in
  (colorie2 (diviseplusieur dim (genere dim dim couls)) couls), couls, dim
;;

(* Test *)

affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;


(*2.2*)

(*Q17*) 

let quelle_couleur (c: case) (cases,a,b: configuration) : couleur = 
  match associe c cases Libre with 
  |Libre -> Libre
  |_ -> associe c cases Libre;;

quelle_couleur (-4,1,3) ([((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge)],[Rouge;Vert;Jaune],3)


(*Q18*) 

let rec supprime (c:case) (cl:case_coloree list):case_coloree list = 
                                                                     
  (*fct auxiliaure qui supprime la case coloree dans la liste*)
  match cl with
  | [] -> []
  | ((i, j, k), couleur)::tl ->
      if (i, j, k) = c then
        tl   (*si la case = c on la supprime comme voulu*)
      else
        ((i, j, k), couleur)::(supprime c tl) (*sinon on repete jusqu'a la trouver*)

                                              
let supprime_dans_config (conf:configuration) (c:case):configuration =
  let (cases, couleurs, dim) = conf in (*def des elements de conf*) 
  let nouvelles_case = supprime c cases in
  (nouvelles_case,couleurs,dim)
;;




(*2.3*)
(* Cette partie est en commentaire puisque nous avons repris toutes ces fonctions
   avec une autre formulation dans la partie suivante *)

(*Q19*)
(*
  let est_coup_valide (case_coloreel,cl,dim:configuration) (c:coup):bool=
    match c with
    | Sm(_) -> failwith "Saut multiples non implementes"
    | Du(c1,c2) -> (sont_cases_voisines c1 c2)&&(est_dans_losange c2 dim)&&((quelle_couleur c2 (case_coloreel,cl,dim))=Libre)&&((quelle_couleur c1 (case_coloreel,cl,dim))=List.hd cl );;




(*Q20*)

  let appliquer_coup (case_coloreel,cl,dim:configuration) (c : coup) : configuration = 
    let Du(c1,c2)=c in
    let x,y,z=(supprime_dans_config (case_coloreel,cl,dim) c1) in
    (tourner_config (x@[(c2,List.hd y)],y,z))
  (* Il y a un warning de pattern-matching non exhaustive mais ce cas est résolu dans la prochaine fonction*)
  ;;




(*Q21*)

  let mettre_a_jour_configuration (case_coloreel,cl,dim:configuration) (c : coup) : configuration=
    if (est_coup_valide (case_coloreel,cl,dim) c) then 
      match c with
      |Sm(_)-> failwith "Saut multiples non implementes"
      |_-> appliquer_coup (case_coloreel,cl,dim) c
    else failwith "Ce coup n'est pas valide,le joueur doit rejouer"
;;  
      
mettre_a_jour_configuration ([((0,0,0),Vert);((3,0,-3),Rouge)],[Vert;Jaune;Rouge],6) (Du((0,0,0),(1,0,-1)))



*)



(*Q22*)

(*
Profil:val est_libre case->configuration->bool
Semantique:Verifie si la case est dans la config ou non *)
let rec est_libre (c:case)(conf:configuration):bool =
  match conf with
  |([],_,_) -> true
  |(hd::tl),col,d-> (fst(hd)<> c) && (est_libre c (tl,col,d))
;;

(*
Profil:val f : case -> case -> vecteur -> int -> configuration -> bool
Semantique: f verifie si tous les elements d'une ligne sont libre et renvoie true si c'est le cas*) 
let rec f (e:case)(c2:case)((a,b,c):vecteur)(d:int)(conf:configuration):bool=
  if d=0 then true
  else 
  if  not(est_libre e conf) then false 
  else
    f (translate e (a,b,c) ) c2 (a,b,c) (d-1) conf
;;

(*verifie si tout le segment est libre*)
let rec est_libre_seg (c1:case) (c2:case) (conf:configuration) : bool =
  let v, d = vec_et_dist c1 c2 in
  f c1 c2 v d conf
;; 

(*tests*)
let confi = ([(1, -1, 0),Vert; (2, 0, -2),Vert; (2, 1, -3),Vert; (1, 2, -3),Vert; (0, 2, -2),Vert; (0, 1, -1),Vert;
              (-1, 1, 0),Rouge; (-2, 0, 2),Rouge; (0, -1, 1),Rouge; (1, 0, -1),Rouge; (2, -2, 0),Rouge; (2, -4, 2),Rouge;
              (-4, 2, 2),Jaune; (-3, 1, 2),Jaune; (-3, 2, 1),Jaune; (-3, 3, 0),Jaune; (-2, 1, 1),Jaune; (-1, 0, 1),Jaune ], [Vert;Rouge;Jaune], 3);;
est_libre_seg (-2, -1, 3) (-2, 5, -3) confi;;
est_libre_seg (-5, 3, 2) (-1, 3, -2) confi;; 
est_libre_seg (-3, -2, 5) (1, -2, 1) confi;;
est_libre_seg (-2, 2, 0) (-2, 5, -3) confi;; 

(*Q23*)

(* 
Profil:val pivot : case_option -> case
Semantique:pivot prend un argument de type case_option plus precisement 
Some of case(on a pas besoin du None dans ce cas ) et renvoie la case*) 
let pivot(Some(case):case_option)=case;;

let est_saut (depart:case)(arrivee:case)(conf:configuration):bool=
  let(casel,coul,dim)=conf in
  let (a,b,c),d = vec_et_dist depart arrivee in
  (est_dans_etoile arrivee dim)&&
  (est_libre arrivee conf) &&
  ((d mod 2 =0) && (not(est_libre (pivot(calcul_pivot depart arrivee)) conf))&& 
   (est_libre_seg (translate depart (a,b,c)) (pivot(calcul_pivot depart arrivee)) conf)&&  
   (est_libre_seg arrivee (pivot(calcul_pivot depart arrivee)) conf) )                                                                   
;;

(* Tests *) 
vec_et_dist(-2,2,0)(-2,0,2);;
translate (-2,2,0)(0,1,-1);;
est_saut (-1, 1, 0) (1, 1, -2) confi;;
vec_et_dist (-1, 1, 0) (1, 1, -2);;
est_saut (-1, 1, 0) (0, 0,  0) confi;;
est_saut (-2, 1, 1) (-2, 2, 0) confi;;
est_saut (-4, 2, 2) (-2, 0, 2) confi;; 


(*Q24*)


let rec est_saut_multiple (cl:case list) (conf:configuration):bool =
  match cl with
  |[]|[_]->true 
  |hd::x::tl-> (est_saut hd x conf) && (est_saut_multiple (x::tl) conf)
                                       
;;

(*test*)
let config =([(-3, 2, 1),Vert; (-3, 1, 2),Vert; (-2, 1, 1),Vert; (-2, 0, 2),Vert; (0, 0, 0),Vert],[Vert],3);;
est_saut_multiple [(-3, 2, 1); (-3, 0, 3); (-2, -1, 3)] config;;

              
(*Q25*) 

let est_coup_valide (conf : configuration) (c : coup) : bool =
  let (case_coloreel,cl,dim) = conf in
  match c with
  | Sm(x) -> (est_saut_multiple x conf) && (est_dans_losange (der_liste x) dim) 
  | Du(c1,c2) -> (sont_cases_voisines c1 c2)&&(est_dans_losange c2 dim)&&
                 ((quelle_couleur c2 (case_coloreel,cl,dim))=Libre)&&((quelle_couleur c1 (case_coloreel,cl,dim))=List.hd cl)
;;


(*Tests *)
vec_et_dist (-2, 1, 1) (2, 1, -3);;
est_coup_valide ([(-2, 1, 1),Vert;(0, 1, -1),Rouge],[Vert;Rouge],3) (Sm[(-2, 1, 1); (2, 1, -3)]) ;;
est_coup_valide ([(-2, 1, 1),Vert;(0, 1, -1),Rouge],[Vert;Rouge],3) (Sm[(-2, 1, 1); (1, 1, -2)]) ;;
est_coup_valide ([(-3, 2, 1),Jaune;(-2, 1, 1),Jaune;(-1, 0, 1),Rouge],[Jaune;Rouge],3) (Sm[(-3, 2, 1); (1, -2, 1)]) ;;

let config =([(-3, 2, 1),Vert; (-3, 1, 2),Vert; (-2, 1, 1),Vert; (-2, 0, 2),Vert; (0, 0, 0),Vert],[Vert],3);;
est_coup_valide config (Sm[(-3, 2, 1); (-3, 0, 3); (-2, -1, 3)]);;



let appliquer_coup (case_coloreel,cl,dim:configuration) (c : coup) : configuration = 
  match c with
  |Du(c1,c2) -> let x,y,z=(supprime_dans_config (case_coloreel,cl,dim) c1) in
      (tourner_config (x@[(c2,List.hd y)],y,z))
  |Sm(li) -> let x,y,z=(supprime_dans_config (case_coloreel,cl,dim) (List.hd li)) in
      tourner_config (x @ [(der_liste li),(List.hd y)] , y , z ) 
  (* Il y a un warning de pattern-matching non exhaustive mais ce cas est résolu dans la prochaine fonction*)
;; 


let mettre_a_jour_configuration (case_coloreel,cl,dim:configuration) (c : coup) : configuration=
  if (est_coup_valide (case_coloreel,cl,dim) c) then 
    appliquer_coup (case_coloreel,cl,dim) c
  else failwith "Ce coup n'est pas valide,le joueur doit rejouer"
              
(*partie 3*)

(*Q26*)

(*Profil:val prm : 'a * 'b * 'c -> 'a
Semantique : prm renvoie le 1eme element d'un tuple*)
let prm (a,b,c) = a;;


let score ((cases,couleurs,dim):configuration):int=
  List.fold_left (fun acc e-> if (snd e)=(List.hd couleurs) then acc + prm (fst e) else acc+0) 0 cases 
;;
          
(* Test *) 
let config =([(-3, 2, 1),Vert; (-3, 1, 2),Vert; (-2, 1, 1),Vert; (-2, 0, 2),Vert; (0, 0, 0),Vert;(-1,0,1),Rouge],[Vert;Rouge],3);; 
score config;;             
              

              
let score_gagnant (d:int) =
  List.fold_left(fun acc e -> acc + prm e ) 0 (remplir_triangle_haut d (d+1,-d,-1))
;;


(*Q27*)

let gagne ((cases,couleurs,dim):configuration) :bool=
  score (cases,couleurs,dim)  = (score_gagnant dim)
;;
  
(*tests*)

let coup = Sm[(-3, 0, 3); (1, 0, -1)];;
let configuration=([(0, 1, -1),Rouge; (0, 0, 0),Rouge; (-3, 0, 3),Rouge; (-1, -1, 2),Jaune; (-1, 0, 1),Jaune; (1, -2, 1),Vert; (0, 2, -2),Vert],[Rouge;Jaune;Vert],3);; 
est_coup_valide configuration coup;;

(*Q28*)

(*Profil:val sec : 'a * 'b * 'c -> 'b
Semantique : sec renvoie le 2eme element d'un tuple*)
let sec (a,b,c)=b;;

(*Profil:val der : 'a * 'b * 'c -> 'c
Semantique : der renvoie le 3eme element d'un tuple*)
let der(a,b,c)=c;;


let est_partie (conf : configuration) (coups : coup list) : couleur = 
  let conf_finale =tourner_config(tourner_config( List.fold_left (fun acc coup ->  mettre_a_jour_configuration acc coup) conf coups)) in
  let score_protagoniste = score conf_finale in
  let score_gagnant = score_gagnant (der conf_finale) in
  if score_protagoniste = score_gagnant then List.hd (sec conf_finale) else Libre
    
;;

(* test *)

let lcoupss = [Du((-4, 2, 2), (-3, 2, 1));
               Du((-4, 2, 2), (-3, 1, 2));
               Du((-4, 2, 2), (-3, 1, 2));
               Sm([(-4, 3, 1); (-2, 1, 1)]);
               Sm([(-5, 3, 2); (-1, -1, 2)]);
               Sm([(-4, 1, 3); (-2, 1, 1)]);
               Sm([(-3, 2, 1); (-1, 0, 1)]);
               Sm([(-4, 1, 3); (-2, 1, 1)]);
               Du((-2, 1, 1), (-1, 1, 0));
               Sm([(-1, 0, 1); (3, 0, -3)]);
               Sm([(-3, 1, 2); (3, -3, 0)]);
               Du((-1, 1, 0), (0, 0, 0));
               Sm([(-4, 1, 3); (0, 1, -1)]);
               Sm([(-1, -1, 2); (3, -1, -2)]);
               Du((0, 0, 0), (1, -1, 0));
               Sm([(-2, 1, 1); (2, 1, -3)]);
               Sm([(-2, 1, 1); (2, 1, -3)]);
               Du((1, -1, 0), (2, -1, -1));
               Sm([(-5, 2, 3); (-1, 2, -1)]);
               Du((2, 1, -3), (2, 0, -2));
               Sm([(-4, 3, 1); (-2, 1, 1); (0, -1, 1); (4, -1, -3)]);
               Du((-5, 3, 2), (-4, 3, 1));
               Sm([(2, 0, -2); (4, -2, -2)]);
               Du((-3, 1, 2), (-2, 1, 1));
               Sm([(2, 1, -3); (4, -1, -3)]);
               Du((-4, 3, 1), (-3, 2, 1));
               Du((-5, 3, 2), (-4, 3, 1));
               Du((0, 1, -1), (1, 0, -1));
               Sm([(-3, 2, 1); (1, 2, -3)]);
               Sm([(-2, 1, 1); (0, 1, -1); (4, -3, -1)]);
               Sm([(-1, 2, -1); (3, -2, -1)]);
               Sm([(-6, 3, 3); (-4, 1, 3)]);
               Du((-5, 2, 3), (-5, 3, 2));
               Du((-4, 3, 1), (-3, 2, 1));
               Sm([(-5, 2, 3); (-3, 0, 3); (-1, -2, 3)]);
               Du((2, -1, -1), (3, -1, -2));
               Du((-3, 2, 1), (-2, 2, 0));
               Du((-1, -2, 3), (0, -2, 2));
               Sm([(-4, 3, 1); (-2, 3, -1); (0, 3, -3)]);
               Du((1, 0, -1), (2, -1, -1));
               Du((4, -2, -2), (5, -2, -3));
               Du((-5, 3, 2), (-4, 3, 1));
               Sm([(2, -1, -1); (4, -3, -1)]);
               Du((3, -1, -2), (4, -2, -2));
               Sm([(-4, 3, 1); (-2, 3, -1)]);
               Du((3, -2, -1), (4, -2, -2));
               Sm([(-4, 1, 3); (-2, -1, 3)]);
               Du((3, -1, -2), (4, -2, -2));
               Sm([(3, 0, -3); (5, -2, -3)]);
               Du((1, 2, -3), (1, 1, -2));
               Du((-2, 3, -1), (-1, 2, -1));
               Du((-6, 3, 3), (-5, 3, 2));
               Sm([(1, 1, -2); (3, -1, -2); (5, -3, -2)]);
               Du((-6, 3, 3), (-5, 3, 2));
               Du((-2, 2, 0), (-1, 2, -1));
               Sm([(-2, -1, 3); (0, -1, 1); (4, -1, -3)]);
               Du((-5, 3, 2), (-4, 3, 1));
               Du((-5, 3, 2), (-4, 3, 1));
               Du((3, -3, 0), (4, -3, -1));
               Du((0, 3, -3), (1, 2, -3));
               Du((-1, 2, -1), (0, 2, -2));
               Sm([(4, -3, -1); (6, -3, -3)]);
               Du((-4, 3, 1), (-3, 3, 0));
               Du((0, 2, -2), (1, 1, -2));
               Sm([(0, -2, 2); (2, -2, 0)]);
               Du((-3, 3, 0), (-2, 3, -1));
               Du((1, 1, -2), (2, 0, -2));
               Du((2, -2, 0), (3, -2, -1));
               Du((1, 2, -3), (2, 1, -3));
               Du((-4, 3, 1), (-3, 2, 1));
               Du((3, -2, -1), (4, -3, -1))]
;;


let lconf=([(3, -6, 3),Jaune; (3, -5, 2),Jaune; (3, -4, 1),Jaune; (2, -5, 3),Jaune; (2, -4, 2),Jaune; (1, -4, 3),Jaune; (3, 1, -4),Rouge;
            (3, 2, -5),Rouge; (3, 3, -6),Rouge; (2, 2, -4),Rouge; (2, 3, -5),Rouge; (1, 3, -4),Rouge; (-4, 1, 3),Vert; (-4, 2, 2),Vert;
            (-4, 3, 1),Vert; (-5, 2, 3),Vert; (-5, 3, 2),Vert; (-6, 3, 3),Vert] ,[Vert;Jaune;Rouge],3)
;;

est_partie lconf lcoupss;;



(*Q29*)

(*
Profil:val cases_adjacentes : case -> case list
Semantique: renvoie la liste des coordonnées des cases adjacentes *)
let cases_adjacentes ((i, j, k) : case) : case list =
  
  [(i + 1, j - 1, k); (i + 1, j, k - 1); (i, j + 1, k - 1);
   (i - 1, j + 1, k); (i - 1, j, k + 1); (i, j - 1, k + 1)] 
;;

(*
Profil: val case_pivot : case -> case -> case
Semantique:case_pivot prend une case de depart et un pivot et renvoie la case d'arrivee d'un saut*)
let case_pivot (case:case)(pivot:case):case= 
  let (a,b,c),d =vec_et_dist case pivot in
  translate pivot (a*d,b*d,c*d)     
;;

(*
Profil :val coups_possibles_unitaire : configuration -> case -> (case * coup) list 
Semantique:coups_possibles_unitaire renvoie tout les coups unitaires possible*)
let coups_possibles_unitaire (conf : configuration) (c : case) : (case * coup) list = 
  let cases_adjacentes = cases_adjacentes c in
  List.fold_left (fun acc adj ->if est_coup_valide conf (Du(c, adj)) then (adj, Du(c, adj)) :: acc else acc) [] cases_adjacentes
;;


(*
Profil : val cases_nL : case_coloree list -> case list
Semantique:cases_nL prend en argument une confi et rend la liste des cases  de cet confi*)
let rec cases_nL (lis :case_coloree list):case list=
  match lis with
  |[] ->[]
  |((x,y,z),_)::tl->(x,y,z)::cases_nL tl
;;


(*
Profil: val coup_possible_multiple: configuration->case->(case*coup) list
Semantique:coup_possible_multiple renvoie les sauts 'simples' possibles.*)

let coup_possible_multiple (cases, coul, dim) c =
  let case_coloree_vide = cases_nL cases in
  let valid_moves =
    List.filter (fun hd -> est_coup_valide (cases, coul, dim) (Sm([c; (case_pivot c hd)]))) case_coloree_vide
  in
  List.map (fun hd -> (case_pivot c hd, Sm([c; case_pivot c hd]))) valid_moves
;;


(*
Profil: val nouvelle_config : case -> configuration-> configuration
    Semantique:nouvelle_config prend une configuration et une case et ajoute la case a la configuration*)
let nouvelle_config (c:case)((case,coul,dim):configuration):configuration=
  ((case @ [(c , List.hd coul)]),coul,dim)
;;


(*
Profil: val coup_m : configuration->case->(case*coup) list
Semantique:coup_m prend en argument une configuration et une case et rend les sauts 'doubles' possibles de la configuration*)

let coup_m ( conf :configuration ) (c:case) :(case*coup) list=
  List.fold_left (fun acc e ->(let nouvelle = nouvelle_config (fst e) conf 
                               in (coup_possible_multiple nouvelle (fst e)) @ acc) )  [] (coup_possible_multiple conf c)
                          
;;

(*Profil: val convert: (case*coup) list -> case -> (case*coup) list
Semantique: convert prend une case et une liste de coup en argument et ajoute la case a chaque coup .C'est pour ajouter la case aux sauts'doubles'
Exemple :
 convert [((0,2,-2) ,Sm[(2, 0, -2); (0, 2, -2)])] (0,0,0) = [((0, 2, -2), Sm [(0, 0, 0); (2, 0, -2); (0, 2, -2)])] *)

let rec convert (lis:(case*coup) list)(c:case):(case*coup) list=   
  match lis with
  |[]->[]
  |(a,Sm(b))::tl-> (a,Sm(c::b)):: convert tl c
;;
convert [((0,2,-2) ,Sm[(2, 0, -2); (0, 2, -2)])] (0,0,0)=[((0, 2, -2), Sm [(0, 0, 0); (2, 0, -2); (0, 2, -2)])]

 
let coups_possible (conf:configuration) (c:case) :(case*coup) list= (*coups_possible regroupe les sauts unitaires 
                                                                    et les sauts 'simple' et 'double' et rend les coups possibles*)
  (coups_possibles_unitaire conf c)@(coup_possible_multiple conf c )@ (convert ((coup_m conf c)) c)
;;
(*Pour coups_possible nous avouns juste reussi a la faire pour les coups unitaires et sauts 'simple' et 'double',
  mais nous avons pas reussi a la faire pour un saut 'complet'*)

(*test*)
let example_config = ([((1, -2, 1),Rouge); ((0, -1, 1),Vert); ((0, 0, 0),Vert); ((0, 1, -1),Jaune);( (1, 1, -2),Rouge);((3, -1, -2),Rouge)],[Vert;Jaune; Rouge],3);;

let example_case : case = (0, 0, 0) ;;

coups_possible example_config example_case ;;


(* Q30 *)


