(* Q10 pas rec *)

let tourner_liste (lis: 'a list) : 'a list=
  match lis with
  |[] -> []
  |h::tl -> tl@[h]
;;

tourner_liste [1;2;3];; 

let rec der_liste (lis: 'a list): 'a =
  match lis with
  |[] -> failwith "liste vide"
  |[x] -> x
  |h::tl -> der_liste tl 
;;

der_liste [1;2;3];;

(* Q11 *) 

let rec remplir_segment (m: int) (c:case) : case list=
  match m,c with 
  |1,(i,j,k) -> [(i,j,k)]
  |m,(i,j,k) -> [(i, j, k)]@remplir_segment (m - 1) (i, j + 1, k - 1)
;;

remplir_segment 3 (-4,1,3) ;;

(* Q12 *)

let rec remplir_triangle_bas (m: int) (c:case) : case list=
  match m,c with 
  |1,(i,j,k) -> [(i,j,k)]
  |m,(i,j,k) -> (remplir_segment m c)@(remplir_triangle_bas (m-1) (i-1,j+1,k))
;;
  
remplir_triangle_bas 3 (-4,1,3) ;;

(* Q13 *)

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

(* Q15 *)

let rec touliste (liste1 : case_coloree list) (liste2:couleur list)  : case_coloree list = 
  match liste1 with 
  |[] -> [] 
  |(x,y)::tl -> match x with
    |(i,j,k) -> [tourner_une_case (6/List.length liste2) (i,j,k), y]@touliste tl liste2
;;
  
let tourner_config ((liste1,liste2,dim) : configuration) : configuration = 
  match liste1,liste2,dim with 
  |[],[],_ -> ([],[],dim)
  |l1,l2,_ -> ((touliste l1 l2), (tourner_liste l2), dim)
;; 
  
let conf= ([((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge)],[Rouge;Vert;Jaune],3);;

tourner_config conf;;

(* Q16 *)
    
let rec tou_liste (liste1 : case list) (liste2:couleur list)  : case list = 
  match liste1 with 
  |[] -> [] 
  |x::tl -> match x with
    |(i,j,k) -> [tourner_une_case (6/List.length liste2) (i,j,k)]@tou_liste tl liste2 
;;

let rec genere (cpt:int)(dim:int)(lis:couleur list): case list =
  match cpt with
  |1-> remplir_triangle_bas dim (-dim-1,1,dim)
  |_ -> remplir_triangle_bas dim (-dim-1,1,dim) @ tou_liste (genere (cpt-1) dim lis) lis
;;


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

let rec colorie2 (cases: case list list) (lis2: couleur list): case_coloree list= 
  match cases, lis2 with
  | [], _ -> []
  | _, [] -> []
  | (hd::tl), (hd2::tl2) -> (colorie hd2 hd) @ (colorie2 tl tl2)
;; 

let rec donnercoul (num: int) (lis: couleur list): couleur list =
  match num, lis with
  | 0, _ -> []
  | _, [] -> failwith "pas assez de couleurs"
  | n, hd::tl -> hd :: donnercoul (n - 1) tl
;;

let remplir_init (joueurs: joueur list) (dim: int): configuration = 
  let couls = donnercoul (List.length joueurs) [Rouge; Vert; Jaune; Noir; Bleu; Marron] in
  (colorie2 (diviseplusieur dim (genere dim dim couls)) couls), couls, dim
;; 


(*2.2*)

(*Q17*)

let rec associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = 
  match l with
  |[]-> defaut
  |(x,y)::tl-> 
      if a=x then y
      else associe a tl defaut;;




let quelle_couleur (c: case) (cases,a,b: configuration) : couleur = 
  match associe c cases Libre with 
  |Libre -> Libre
  |_ -> associe c cases Libre;;




(*Q18*) 

let rec supprime (c:case) (cl:case_coloree list):case_coloree list = (fct auxiliaure qui supprime la case coloree dans la liste)
  match cl with
  | [] -> []
  | ((i, j, k), couleur)::tl ->
      if (i, j, k) = c then
        tl   (si la case = c on la supprime comme voulu)
      else
        ((i, j, k), couleur)::(supprime c tl) (sinon on repete jusqu'a la trouver)

                                              
let supprime_dans_config (conf:configuration) (c:case):configuration =
  let (cases, couleurs, dim) = conf in (def des elements de conf)
  let nouvelles_case = supprime c cases in
  (nouvelles_case,couleurs,dim);;




(*2.3*)

(*Q19*)

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
  if (est_coup_valide (case_coloree1,cl,dim) c) then 
    match c with
    |Sm(_)-> failwith "Saut multiples non implementes"
    |_-> appliquer_coup (case_coloreel,cl,dim) c
  else failwith "Ce coup n'est pas valide,le joueur doit rejouer"
;;
