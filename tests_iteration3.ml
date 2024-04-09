#use "CPtest.ml";;
#use "camlbrick.ml";;


(*refaire*)
let test_ball_modif_speed () =
  (* Initialisation des valeurs pour le test *)
  let game : t_camlbrick = {
    params = make_camlbrick_param ();
    ball = {
      position = ref { dx = 0; dy = 0 };
      velocity = ref { dx = 1; dy = 1 }; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  
  let ball = {
    position = ref { dx = 0; dy = 0 };
    velocity = ref { dx = 1; dy = 1 };
    size = BS_SMALL;
  } in
  
  let dv = { dx = 2; dy = 3 } in
  
  (* Appel de la fonction à tester *)
  ball_modif_speed (game, ball, dv);
  
  (* Assertions *)
  assert_equals !(ball.velocity).dx 3;
  assert_equals !(ball.velocity).dy 4
;;

(*refaire*)
let test_ball_modif_speed_sign () =
  (* Initialisation des valeurs pour le test *)
  let game : t_camlbrick = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(0, 0));
      velocity = ref { dx = 1; dy = 1 };
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  let sv = { dx = 2; dy = 3 } in
  let expected_velocity = { dx = 2; dy = 3 } in
  
  (* Appel de la fonction à tester *)
  ball_modif_speed_sign (game, game.ball, sv);
  
  (* Assertion *)
  assert_equals game.ball.velocity expected_velocity
;;

(**Cette fonction de test vérifie si la fonction is_inside_circle fonctionne correctement en vérifiant si les points sont à l'intérieur ou à l'extérieur du cercle spécifié.

Pour le test à l'intérieur du cercle, elle utilise les coordonnées (3, 4), qui sont à l'intérieur d'un cercle de rayon 5 centré à l'origine.
Pour le test à l'extérieur du cercle, elle utilise les coordonnées (6, 8), qui sont à l'extérieur du même cercle.
Si la fonction is_inside_circle fonctionne correctement, le premier test devrait renvoyer true et le deuxième test devrait renvoyer false.
@author Hau NGUYEN
    *)
let test_is_inside_circle () =
  (* Initialisation des valeurs pour le test *)
  let cx = 0 in
  let cy = 0 in
  let rad = 5 in
  (* Test à l'intérieur du cercle *)
  let x_inside = 3 in
  let y_inside = 4 in
  let result_inside = is_inside_circle (cx, cy, rad, x_inside, y_inside) in
  assert_true result_inside;
  (* Test à l'extérieur du cercle *)
  let x_outside = 6 in
  let y_outside = 8 in
  let result_outside = is_inside_circle (cx, cy, rad, x_outside, y_outside) in
  assert_false result_outside
;;

(**Cette fonction de test vérifie si la fonction is_inside_quad fonctionne correctement en vérifiant si un point donné est à l'intérieur du quadrilatère spécifié.

Les coordonnées du quadrilatère sont (0, 0) pour le coin supérieur gauche et (100, 100) pour le coin inférieur droit.
Les coordonnées du point à vérifier sont (50, 50).
Si la fonction is_inside_quad fonctionne correctement, le test devrait renvoyer true, car le point (50, 50) est à l'intérieur du quadrilatère défini.
@author Hau NGUYEN*)
let test_is_inside_quad () =
  (* Initialisation des valeurs pour le test *)
  let x1, y1, x2, y2 = (0, 0, 100, 100) in
  let x, y = (50, 50) in
  
  (* Appel de la fonction à tester *)
  let result = is_inside_quad (x1, y1, x2, y2, x, y) in
  
  (* Assertion *)
  assert_true result
;;

(*refaire*)
let test_ball_remove_out_of_border () =
  (* Initialisation des valeurs pour le test *)
  let game : t_camlbrick = {
    params = make_camlbrick_param ();
    ball = {
      position = ref { dx = 0; dy = 0 };
      velocity = ref { dx = 0; dy = 1 }; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  
  let balls = [
    { position = ref { dx = 0; dy = 100 }; velocity = ref { dx = 0; dy = 1 }; size = BS_SMALL };
    { position = ref { dx = 0; dy = 200 }; velocity = ref { dx = 0; dy = 1 }; size = BS_SMALL };
  ] in
  
  (* Appel de la fonction à tester *)
  let result = ball_remove_out_of_border (game, balls) in
  
  (* Assertion *)
  assert_equals (List.length result) 1
;;

(**Ce test vérifie si la fonction ball_hit_paddle fonctionne correctement en vérifiant si la balle entre en collision avec la raquette.

Les coordonnées de la balle sont { dx = 100; dy = 300 } et sa taille est BS_MEDIUM.
Les coordonnées de la raquette sont position = ref 50 et sa taille est PS_SMALL.
La balle et la raquette se trouvent dans une position où elles doivent se toucher. Si la fonction ball_hit_paddle fonctionne correctement, le test devrait renvoyer true, indiquant que la balle a bien touché la raquette.
@author Hau NGUYEN*)
let test_ball_hit_paddle () =
  (* Initialisation des valeurs pour le test *)
  let game : t_camlbrick = {
    params = make_camlbrick_param ();
    ball = {
      position = ref { dx = 100; dy = 300 };
      velocity = ref { dx = 0; dy = 1 }; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  
  let ball = {
    position = ref { dx = 100; dy = 300 };
    velocity = ref { dx = 0; dy = 1 };
    size = BS_SMALL;
  } in
  
  let paddle = {
    position = ref 50;
    size = PS_SMALL;
  } in
  
  (* Appel de la fonction à tester *)
  let result = ball_hit_paddle (game, ball, paddle) in
  
  (* Assertion *)
  assert_true result
;;

(**Ce test vérifie si la fonction ball_hit_corner_brick fonctionne correctement en vérifiant si la balle entre en collision avec un coin d'une brique.

Les coordonnées de la balle sont { dx = 100; dy = 100 } et sa taille est BS_MEDIUM.
Les coordonnées de la brique sont calculées en fonction des indices i = 0 et j = 0, et les dimensions des briques sont définies par les paramètres du jeu.
La balle et la brique se trouvent dans une position où elles doivent se toucher. Si la fonction ball_hit_corner_brick fonctionne correctement, le test devrait renvoyer true, indiquant que la balle a bien touché un coin de la brique.
@author Hau NGUYEN*)
let test_ball_hit_corner_brick () =
  (* Initialisation des valeurs pour le test *)
  let game : t_camlbrick = {
    params = make_camlbrick_param ();
    ball = {
      position = ref { dx = 100; dy = 100 };
      velocity = ref { dx = 0; dy = 1 }; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  
  let ball = {
    position = ref { dx = 100; dy = 100 };
    velocity = ref { dx = 0; dy = 1 };
    size = BS_SMALL;
  } in
  
  let i = 0 in
  let j = 0 in
  
  (* Appel de la fonction à tester *)
  let result = ball_hit_corner_brick (game, ball, i, j) in
  
  (* Assertion *)
  assert_true result
;;

(**Ce test vérifie si la fonction ball_hit_side_brick fonctionne correctement en vérifiant si la balle entre en collision avec un côté d'une brique.

Les coordonnées de la balle sont { dx = 100; dy = 100 } et sa taille est BS_MEDIUM.
Les coordonnées de la brique sont calculées en fonction des indices i = 0 et j = 0, et les dimensions des briques sont définies par les paramètres du jeu.
La balle et la brique se trouvent dans une position où elles doivent se toucher. Si la fonction ball_hit_side_brick fonctionne correctement, le test devrait renvoyer true, indiquant que la balle a bien touché un côté de la brique.
@author Hau NGUYEN*)
let test_ball_hit_side_brick () =
  (* Initialisation des valeurs pour le test *)
  let game : t_camlbrick = {
    params = make_camlbrick_param ();
    ball = {
      position = ref { dx = 100; dy = 100 };
      velocity = ref { dx = 0; dy = 1 }; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  
  let ball = {
    position = ref { dx = 100; dy = 100 };
    velocity = ref { dx = 0; dy = 1 };
    size = BS_SMALL;
  } in
  
  let i = 0 in
  let j = 0 in
  
  (* Appel de la fonction à tester *)
  let result = ball_hit_side_brick (game, ball, i, j) in
  
  (* Assertion *)
  assert_true result
;;

(* Appels des fonctions de test *)
test_ball_modif_speed ();;
test_ball_modif_speed_sign () ;;
test_is_inside_circle ();;
test_is_inside_quad ();;
test_ball_remove_out_of_border();;
test_ball_hit_paddle();;
test_ball_hit_corner_brick ();;
test_ball_hit_side_brick ()

(* Affiche le rapport de test *)
test_report();;  