#use "CPtest.ml";;
#use "camlbrick.ml";;


(*refaire*)
let test_ball_modif_speed () =
  let initial_velocity = { dx = 1; dy = 1 } in
  let dv = { dx = 2; dy = 3 } in
  let ball = {
    position = ref { dx = 0; dy = 0 };
    velocity = ref initial_velocity;
    size = BS_SMALL;
  } in  
  let game : t_camlbrick = {
    params = make_camlbrick_param ();
    ball = ball;
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  ball_modif_speed (game, ball, dv);
  assert_equals !(ball.velocity).dx (initial_velocity.dx + dv.dx);
  assert_equals !(ball.velocity).dy (initial_velocity.dy + dv.dy)
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