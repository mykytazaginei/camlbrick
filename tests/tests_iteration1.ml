#use "CPtest.ml";; 

#load "CPtestfonc.cma";;
open CPtestfonc;;


(* Initialisation du rapport de test. *)
test_reset_report();;

type t_vec2 = {dx : int ; dy : int};;

let make_vec2(x,y : int * int) : t_vec2 = 
  {dx = x ; dy = y}
;;

(** Test d une valeur positive. 
    @autor Hau NGUYEN*)
let test_fonc_make_vec2 () : unit =
  let res : t_vec2 t_test_result =
     test_exec(make_vec2, "test avec positive values" ,(3,5)) in
  assert_equals_result_m("Test dx,dy", {dx = 3; dy = 5}, res)
;;

(**Test de l'addition de vecteurs. 
    @autor Hau NGUYEN*)
let test_fonc_vec2_add () : unit =
  let vec1 = {dx = 1; dy = 2 } in
  let vec2 = {dx = 3; dy = 4 } in
  let res : t_vec2 t_test_result =
    test_exec(vec2_add, "test de l'addition de vecteurs",  (vec1,vec2)) in
  assert_equals_result_m ("Test résultat de l'addition", {dx = 4; dy = 6}, res)
  ;;

  (**test d'addition scalaire au vecteur
      @autor Hau NGUYEN*)
  let test_fonc_vec2_add_scalar () : unit =
    let vec = { dx = 1; dy = 2 } in
    let scalar_x = 3 in
    let scalar_y = 4 in
    let res : t_vec2 t_test_result =
      test_exec (vec2_add_scalar, "test d'addition scalaire au vecteur", (vec, scalar_x, scalar_y)) in
    assert_equals_result_m ("Test ajout scalaire x", {dx=4; dy=6} ,res)
  ;;
  
  (**test de multiplication de vecteurs
      @autor Hau NGUYEN*)
  let test_fonc_vec2_mult () : unit =
    let vec1 = { dx = 2; dy = 3 } in
    let vec2 = { dx = 4; dy = 5 } in
    let res : t_vec2 t_test_result =
      test_exec (vec2_mult, "test de multiplication de vecteurs", (vec1, vec2)) in
    assert_equals_result_m ("Test résultat de la multiplication", { dx = 8; dy = 15 }, res)
    ;;

    (**test de multiplication scalaire au vecteur
      @autor Hau NGUYEN*)
    let test_fonc_vec2_mult_scalar () : unit =
      let vec = { dx = 2; dy = 3 } in
      let scalar_x = 4 in
      let scalar_y = 5 in
      let res : t_vec2 t_test_result =
        test_exec (vec2_mult_scalar, "test de multiplication scalaire au vecteur", (vec, scalar_x, scalar_y)) in
      assert_equals_result_m ("Test résultat de la multiplication scalaire", { dx = 8; dy = 15 }, res);;



(* Appels des fonctions de test *)
test_fonc_make_vec2 ();
test_fonc_vec2_add ();
test_fonc_vec2_add_scalar ();
test_fonc_vec2_mult ();
test_fonc_vec2_mult_scalar ();
  
(* Affiche le rapport de test *)
  test_report();;  

