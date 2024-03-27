#use "CPtest.ml";; 
#load "CPtestfonc.cma";;
open CPtestfonc;;
#use "camlbrick.ml";;


let test_ball_color() : unit =
  assert(ball_color(make_camlbrick(), make_ball(0, 0, 1)) = YELLOW);
  assert(ball_color(make_camlbrick(), make_ball(0, 0, 2)) = ORANGE);
  assert(ball_color(make_camlbrick(), make_ball(0, 0, 3)) = RED);




(* Appels des fonctions de test *)
test_ball_color();;






(* Affiche le rapport de test *)
  test_report();;  