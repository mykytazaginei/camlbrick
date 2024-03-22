#use "CPtest.ml";; 

#load "CPtestfonc.cma";;
open CPtestfonc;;


(* Initialisation du rapport de test. *)
test_reset_report();;

type t_vec2 = {dx : int ; dy : int};;

let make_vec2(x,y : int * int) : t_vec2 = 
  {dx = x ; dy = y}
;;

(** Test d une valeur positive. *)
let test_fonc_make_vec2 () : unit =
  let res : t_vec2 t_test_result =
     test_exec(make_vec2, "test avec positive values" ,{3;5})
  assert_equals_result_m("Test dx,dy", {dx = 3; dy = 5}, res)
;;

(* Appels des fonctions de test *)
test_fonc_make_vec2 ();

  
(* Affiche le rapport de test *)
  test_report();;  

