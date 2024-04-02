#use "CPtest.ml";;
#use "camlbrick.ml";;

let test_ball_color() : unit =
  assert(ball_color(make_camlbrick(), make_ball(0, 0, 1)) = YELLOW);
  assert(ball_color(make_camlbrick(), make_ball(0, 0, 2)) = ORANGE);
  assert(ball_color(make_camlbrick(), make_ball(0, 0, 3)) = RED);


(** [test_paddle_x ()] est un test unitaire qui vérifie si la fonction [paddle_x] renvoie la bonne coordonnée x de la pagaie dans l'état du jeu. 
    Le test crée un état de jeu avec une pagaie positionnée à la coordonnée x 50. Il affirme ensuite que la valeur renvoyée par la fonction [paddle_x] est égale à 50.
    @author Mykyta ZAGINEI 
    *)
let test_paddle_x () : unit =
  let game : t_camlbrick = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(0, 0));
      velocity = 0; 
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
  let res : int ref = paddle_x (game) in
  let expected_position : int ref = ref 50 in
  assert_equals(50, !res);;

(** [test_paddle_size_pixel ()] est une fonction de test unitaire qui teste la fonction [paddle_size_pixel].
    Elle crée trois instances de jeu avec différentes tailles de pagaies (petite, moyenne et grande) et affirme que la fonction [paddle_size_pixel] 
    renvoie les valeurs attendues pour chaque instance de jeu.
    @author Hlib TOTSKYI  
*)
let test_paddle_size_pixel () : unit =
  let game_small : t_camlbrick = 
    { params = make_camlbrick_param ();
      ball = {
        position = ref (make_vec2(0, 0));
        velocity = 0; 
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

  let game_medium : t_camlbrick = 
    { params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(0, 0));
      velocity = 0; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_MEDIUM;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
    } in
  let game_big : t_camlbrick = 
    { params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(0, 0));
      velocity = 0; 
      size = BS_MEDIUM;
    };
    paddle = {
      position = ref 50;
      size = PS_BIG;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
    } in
  let res_small : int t_test_result = test_exec(paddle_size_pixel, "test avec pagaie petite", game_small) in
  let res_medium : int t_test_result = test_exec(paddle_size_pixel, "test avec pagaie moyenne", game_medium) in
  let res_big : int t_test_result = test_exec(paddle_size_pixel, "test avec pagaie grande", game_big) in
  assert_equals_result (60, res_small) ;
  assert_equals_result (80, res_medium) ;
  assert_equals_result (100, res_big) ;;

(
(** [test_has_ball_true ()] est une fonction de test unitaire qui teste la fonction [has_ball]
    en s'attendant à ce qu'elle renvoie [true]. Elle crée un état de jeu avec une balle,
    une raquette, des briques, un score et un état de jeu. Elle exécute ensuite la fonction [has_ball] sur l'état du jeu et affirme qu'elle renvoie [true].
    sur l'état du jeu et affirme que le résultat est [false].
    @author Mykyta ZAGINEI
*)
let test_has_ball_true() : unit = 
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(0, 0));
      velocity = 0; 
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
  let res : bool t_test_result = test_exec(has_ball,"test avec true", game)  in
  assert_false_result (res);;
(*
  Ce code définit une fonction de test nommée `test_balls_count_0` qui teste la fonction `balls_count`.
  Elle crée un état de jeu avec 0 balle, une raquette, un tableau de briques vide, un score de 0, et un état de jeu GAMEOVER.
  La fonction `test_exec` est alors appelée avec la fonction `balls_count` et l'état du jeu comme arguments.
  Le résultat du test est stocké dans la variable `res`.
  Enfin, la fonction `assert_equals_result` est utilisée pour affirmer que le résultat est égal à 0.
  @author Hlib TOTSKYI
*)
let test_balls_count_0() : unit = 
  let game = 
  {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(0, 0));
      velocity = 0; 
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
  let res : int t_test_result = test_exec(balls_count,"test avec 0 balle", game) in
  assert_equals_result (0, res);;
(**
  Ce code définit une fonction de test nommée `test_balls_count_1` qui teste la fonction `balls_count`.
  Elle crée un état de jeu avec 1 balle, une raquette, un tableau de briques vide, un score de 0, et un état de jeu GAMEOVER.
  La fonction `test_exec` est alors appelée avec la fonction `balls_count` et l'état du jeu comme arguments.
  Le résultat du test est stocké dans la variable `res`.
  Enfin, la fonction `assert_equals_result` est utilisée pour affirmer que le résultat est égal à 1.
  @author Hlib TOTSKYI    
*)
let test_balls_count_1() : unit = 
  let game = 
  {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
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
  let res : int t_test_result = test_exec(balls_count,"test avec 1 balle", game) in
  assert_equals_result (1, res);;
(** [test_balls_get ()] est un test unitaire pour la fonction [balls_get].
    Il crée un état de jeu avec une balle et teste si la fonction [balls_get]
    renvoie le bon résultat.
    Le test affirme que le résultat de [balls_get] est égal à [game.ball]. 
    @author Mykyta ZAGINEI    
*)
let test_balls_get() : unit =
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
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
  let res : t_ball list t_test_result = test_exec(balls_get,"test avec 1 balle", game) in
  assert_equals_result ([game.ball], res);;
(** 
[test_ball_get ()] est un test unitaire pour la fonction [ball_get].
  Ce code définit une fonction de test `test_ball_get()` qui teste la fonction `ball_get`.
  Elle crée un état de jeu avec une balle, une raquette, des briques, un score et un état de jeu.
  La fonction `test_exec` est utilisée pour exécuter la fonction `ball_get` avec l'état du jeu et un index de balle.
  Le résultat est ensuite comparé à l'état de la balle attendu à l'aide de la fonction `assert_equals_result`.
  @author Hlib TOTSKYI
*)
let test_ball_get() : unit = 
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
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
  let res : t_ball t_test_result = test_exec(ball_get,"test avec 1 balle", (game, 1)) in
  assert_equals_result (game.ball, res);;
(**
  Ce code définit une fonction de test nommée `test_ball_x` qui teste la fonction `ball_x`.
  Elle crée un état de jeu avec une balle, une raquette, des briques, un score et un état de jeu.
  La fonction `test_exec` est alors appelée avec la fonction `ball_x` et l'état du jeu comme arguments.
  Le résultat du test est stocké dans la variable `res`.
  Enfin, la fonction `assert_equals_result` est utilisée pour affirmer que le résultat est égal à 1.
  @author Mykyta ZAGINEI  
*)
let test_ball_x() : unit =
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
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
  let res : int t_test_result = test_exec(ball_x, "test for ball_x", (game, game.ball)) in
  assert_equals_result (1, res);;
(**
  Ce code définit une fonction de test nommée `test_ball_y` qui teste la fonction `ball_y`.
  Elle crée un état de jeu avec une balle, une raquette, des briques, un score et un état de jeu.
  La fonction `test_exec` est alors appelée avec la fonction `ball_y` et l'état du jeu comme arguments.
  Le résultat du test est stocké dans la variable `res`.
  Enfin, la fonction `assert_equals_result` est utilisée pour affirmer que le résultat est égal à 1.
  @author Mykyta ZAGINEI    
*)
let test_ball_y() : unit =
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
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
  let res : int t_test_result = test_exec(ball_x, "test for ball_y", (game, game.ball)) in
  assert_equals_result (1, res);;

(**
  Ce code définit une fonction de test nommée `test_ball_size_pixel` qui teste la fonction `ball_size_pixel`.
  Elle crée un état de jeu avec une balle, une raquette, des briques, un score et un état de jeu.
  La fonction `test_exec` est alors appelée avec la fonction `ball_size_pixel` et l'état du jeu comme arguments.
  Le résultat du test est stocké dans la variable `res`.
  Enfin, la fonction `assert_equals_result` est utilisée pour affirmer que le résultat est égal à 10.
  @author Hlib TOTSKYI   
*)
let test_ball_size_pixel() : unit =
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(1, 1));
      velocity = 0; 
      size = BS_SMALL;
    };
    paddle = {
      position = ref 50;
      size = PS_SMALL;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  let res : int t_test_result = test_exec(ball_size_pixel, "test for ball_size_pixel", (game, game.ball)) in
  assert_equals_result (10, res);;
(**
  Ce code définit une fonction de test nommée `test_ball_size_pixel_return_type` qui teste le type de retour de la fonction `ball_size_pixel`.
  Elle crée un état de jeu avec une balle, une raquette, des briques, un score et un état de jeu.
  La fonction `test_exec` est alors appelée avec la fonction `ball_size_pixel` et l'état du jeu comme arguments.
  Le résultat du test est stocké dans la variable `res`.
  Enfin, la fonction `assert_equals_result` est utilisée pour affirmer que le résultat est égal à 10.
  @author Hlib TOTSKYI    
*)
let test_ball_color() : unit = 
  let ball = {
    position = ref (make_vec2(0, 0));
    velocity = 0;
    size = BS_SMALL;
    } in
  let game = {
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
  let res : color t_test_result = test_exec(ball_color, "test avec balle de taille SMALL", (game, ball)) in
  assert_equals_result (YELLOW, res);
  ball.size <- BS_MEDIUM;
  let res : color t_test_result = test_exec(ball_color, "test avec balle de taille MEDIUM", (game, ball)) in
  assert_equals_result (ORANGE, res);
  ball.size <- BS_BIG;
  let res : color t_test_result = test_exec(ball_color, "test avec balle de taille BIG", (game, ball)) in
  assert_equals_result (RED, res);
;;
(* Appels des fonctions de test *)
test_ball_color();;
test_paddle_x();;
test_paddle_x_return_type();;
test_paddle_size_pixel();;
test_paddle_size_pixel_return_type();;
test_has_ball_true();;
test_balls_count_0();;
test_balls_count_1();;
test_balls_get();;
test_ball_get();;
test_ball_x();;
test_ball_y();;
test_ball_size_pixel();;

(* Affiche le rapport de test *)
  test_report();;  