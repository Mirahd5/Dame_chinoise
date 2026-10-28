(* -----------------------------------------------------------------------------
   cr Q1 Q9 projet
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

let associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = defaut;;

  
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

(*nous avons tester toutes les cases qui sont dans le losange et en dehors du losange, la fonction marche comme prévue . Voici des exemples en dim 3:*)
est_dans_losange (1, -1, 0) 3;; (*True*)
est_dans_losange (2, 2, -4) 3;; (*False*)

(* Q3

Nous avons decider de représenter l'étoile comme une réunion de 3 losanges, donc on
a 3 expression booléenne différentes:
(j >= -dim && j<= dim && k >= -dim && k<= dim) pour le premier
(i >= -dim && i<= dim && k >= -dim && k<= dim) pour le deuxieme
(j >= -dim && j<= dim && i >= -dim && i<= dim) pour le troisième *)

let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool =
  (j >= -dim && j<= dim && k >= -dim && k<= dim) || 
  (i >= -dim && i<= dim && k >= -dim && k<= dim) || 
  (j >= -dim && j<= dim && i >= -dim && i<= dim) 
;;

(*Nous avons içi aussi testé toutes les cases de l'etoile ainsi que des cases en dehors, la fonctions marche comme prévue:*)
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

(* Exemple: pour la case (2, 2, -4) , ses rotations sont :(4, -2, -2); (2, -4, 2); (-2, -2, 4); (-4, 2, 2) et (-2, 4, -2)
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
(*Si deux cases sont voisines, la somme des valeurs absolues des coordonnés de ces cases est égale à 2*)
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
  let (m,n,p) = diff_case (x,y,z) (a,b,c) in (*en utilisant cette fonction et en supposant 
  que toutes les cases sont alignés, une des coordonées sera nulle,
  et les deux autres seront un chiffre et son opposé*)
  let d = (abs(m)+abs(n)+abs(p))/2 in 
  let v=(m/d,n/d,p/d) in
  (v,d) 
  
;;
(*Exemple*)
vec_et_dist  (0, 2, -2) (0,0,0);;

                                               


