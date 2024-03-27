#use "CPtest.ml";; 
#load "CPtestfonc.cma";;
open CPtestfonc;;
#use "camlbrick.ml";;

(** 
  crée un vecteur avec les valeurs positives données (3, 5). 
  Si le résultat ne correspond pas au résultat attendu {dx = 3; dy = 5}, 
  cela déclenchera un échec d'assertion. 
  @autor Hau NGUYEN
*)
let test_fonc_make_vec2 () : unit =
  let res : t_vec2 t_test_result =
     test_exec(make_vec2, "test avec positive values" ,(3,5)) in
  assert_equals_result_m("Test dx,dy", {dx = 3; dy = 5}, res)
;;

(**
  crée vecteur1,2 avec les valeurs. Si le résultat ne correspond pas au résultat attendu {dx = 4; dy = 6},
  cela déclenchera un échec d'assertion.
  @autor Hau NGUYEN
*)
let test_fonc_vec2_add () : unit =
  let vec1 = {dx = 1; dy = 2 } in
  let vec2 = {dx = 3; dy = 4 } in
  let res : t_vec2 t_test_result =
    test_exec(vec2_add, "test de l'addition de vecteurs",  (vec1,vec2)) in
  assert_equals_result_m ("Test résultat de l'addition", {dx = 4; dy = 6}, res)
  ;;

  (**crée un vecteur v et scalar_x, scalar_y. Si le résultat ne correspond pas au résultat attendu {dx = 4; dy = 6}, cela déclenchera un échec d'assertion.
      @autor Hau NGUYEN*)
let test_fonc_vec2_add_scalar () : unit =
    let vec = { dx = 1; dy = 2 } in
    let scalar_x = 3 in
    let scalar_y = 4 in
    let res : t_vec2 t_test_result =
      test_exec (vec2_add_scalar, "test d'addition scalaire au vecteur", (vec, scalar_x, scalar_y)) in
    assert_equals_result_m ("Test ajout scalaire x", {dx=4; dy=6} ,res)
  ;;
  
(**
  crée vecteur vec1, vec2. Si le résultat ne correspond pas au 
  résultat attendu {dx = 8; dy = 15}, cela déclenchera un échec d'assertion.
  @autor Hau NGUYEN
*)
  let test_fonc_vec2_mult () : unit =
    let vec1 = { dx = 2; dy = 3 } in
    let vec2 = { dx = 4; dy = 5 } in
    let res : t_vec2 t_test_result =
      test_exec (vec2_mult, "test de multiplication de vecteurs", (vec1, vec2)) in
    assert_equals_result_m ("Test résultat de la multiplication", { dx = 8; dy = 15 }, res)
    ;;

(**
  crée un vecteur v et scalar_x, scalar_y. Si le résultat ne correspond pas au résultat attendu 
  {dx = 8; dy = 15}, cela déclenchera un échec d'assertion.
  @autor Hau NGUYEN
*)
let test_fonc_vec2_mult_scalar () : unit =
      let vec = { dx = 2; dy = 3 } in
      let scalar_x = 4 in
      let scalar_y = 5 in
      let res : t_vec2 t_test_result =
        test_exec (vec2_mult_scalar, "test de multiplication scalaire au vecteur", (vec, scalar_x, scalar_y)) in
      assert_equals_result_m ("Test résultat de la multiplication scalaire", { dx = 8; dy = 15 }, res);;

(**
  ajoute des valeur et test de récupération des paramètres du jeu. 
  Si le résultat correspond au résultat attendu. C'est correct.
  @autor Hau NGUYEN
*)
let test_fonc_param_get () : unit =
  let params = make_camlbrick_param () in
  let game = { params = params; ball = {position = make_vec2(0,0); velocity = make_vec2(0,0); size = BS_MEDIUM}; paddle = {
    position = make_vec2(0, 0);
    size = PS_SMALL;
    width = 0;
    height = 0;
  }; bricks = [|[||]|]; score = 0; state = GAMEOVER } in
    let expected_result = params in
    let res : t_camlbrick_param t_test_result =
          test_exec (param_get, "test de récupération des paramètres du jeu", game) in
        assert_equals_result_m ("Test résultat de récupération des paramètres", expected_result, res);;      


(** [test_make_camlbrick ()] est un test unitaire pour la fonction [make_camlbrick].
    Il vérifie que l'état initial du jeu CamlBrick est correctement configuré.
    Le test affirme les conditions suivantes :
    - La position initiale de la balle est (0, 0).
    - La vitesse initiale de la balle est (0, 0).
    - La taille de la balle est fixée à [BS_MEDIUM].
    - La position initiale de la raquette est (0, 0).
    - La taille de la raquette est fixée à [PS_SMALL].
    - La largeur de la raquette est de 0.
    - La hauteur du paddle est de 0.
    - Le tableau de briques est vide.
    - Le score est de 0.
    - L'état du jeu est [GAMEOVER]
    @autor ZAGINEI Mykyta
 *)
let test_make_camlbrick () : unit =
  let brick = make_camlbrick () in
  assert_equal (make_vec2 0 0) brick.ball.position;
  assert_equal (make_vec2 0 0) brick.ball.velocity;
  assert_equal BS_MEDIUM brick.ball.size;
  assert_equal (make_vec2 0 0) brick.paddle.position;
  assert_equal PS_SMALL brick.paddle.size;
  assert_equal 0 brick.paddle.width;
  assert_equal 0 brick.paddle.height;
  assert_equal [|[||]|] brick.bricks;
  assert_equal 0 brick.score;
  assert_equal GAMEOVER brick.state
;;


(** [test_string_of_gamestate ()] est une fonction de test unitaire qui teste la fonction [string_of_gamestate].
    Elle crée trois états de jeu [game1], [game2] et [game3] avec des états différents et vérifie que la représentation sous forme de chaîne de chaque état de jeu correspond aux valeurs attendues.
    Ce test garantit que la fonction [string_of_gamestate] convertit correctement l'énumération des états de jeu en une représentation sous forme de chaîne de caractères.
    Exemple :
    - [game1] state : GAMEOVER, chaîne attendue : "GAMEOVER"
    - [game2] état : PLAYING, chaîne attendue : "GAMEOVER" : PLAYING, chaîne attendue : "PLAYING"
    - [game3] state : PAUSING, chaîne attendue : "PAUSING" 
    @autor ZAGINEI Mykyta
*)
let test_string_of_gamestate () : unit =
  let game1 = {
    params = make_camlbrick_param ();
    ball = {
      position = make_vec2(0, 0);
      velocity = make_vec2(0, 0);
      size = BS_MEDIUM;
    };
    paddle = {
      position = make_vec2(0, 0);
      size = PS_SMALL;
      width = 0;
      height = 0;
    };
    bricks = [|[||]|];
    score = 0;
    state = GAMEOVER;
  } in
  let game2 = {
    game1 with state = PLAYING;
  } in
  let game3 = {
    game1 with state = PAUSING;
  } in
  assert_equal "GAMEOVER" (string_of_gamestate game1);
  assert_equal "PLAYING" (string_of_gamestate game2);
  assert_equal "PAUSING" (string_of_gamestate game3)
;;

(*brick_get*)
(*
  Cette fonction `test_brick_get` est utilisée pour tester la fonction `brick_get`.
  Elle crée un tableau 2D de briques et affirme les valeurs attendues renvoyées par la fonction `brick_get`.
  La fonction `brick_get` prend en compte le tableau 2D de briques, ainsi que les indices de ligne et de colonne, et renvoie le type de brique à cette position.
  @autor ZAGINEI Mykyta
*)
let test_brick_get () : unit =
  let bricks = [|
    [| {kind = BK_normal; hp = 1}; {kind = BK_empty; hp = 0} |];
    [| {kind = BK_empty; hp = 0}; {kind = BK_normal; hp = 1} |]
  |] in
  assert_equal BK_normal (brick_get (bricks, 0, 0));
  assert_equal BK_empty (brick_get (bricks, 0, 1));
  assert_equal BK_empty (brick_get (bricks, 1, 0));
  assert_equal BK_normal (brick_get (bricks, 1, 1));
  assert_equal BK_empty (brick_get (bricks, -1, 0));
  assert_equal BK_empty (brick_get (bricks, 0, -1));
  assert_equal BK_empty (brick_get (bricks, 2, 0));
  assert_equal BK_empty (brick_get (bricks, 0, 2))
;;

(**
  Cette fonction `test_brick_hit` est utilisée pour tester la fonction `brick_hit`.
  Elle crée un tableau 2D de briques et affirme les valeurs attendues renvoyées par la fonction `brick_hit`.
  La fonction `brick_hit` prend en compte le jeu, ainsi que les indices de ligne et de colonne, et renvoie le type de brique à cette position après avoir été touchée par la balle.
  @autor SARDIN Alexandre  
*)
let test_brick_hit () : unit =
  let bricks = [|
    [| {kind = BK_empty; hp = 0}; {kind = BK_simple; hp = 1} |];
    [| {kind = BK_double; hp = 1}; {kind = BK_block; hp = 2} |]
  |] in
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = make_vec2(0, 0);
      velocity = make_vec2(0, 0);
      size = BS_MEDIUM;
    };
    paddle = {
      position = make_vec2(0, 0);
      size = PS_SMALL;
      width = 0;
      height = 0;
    };
    bricks = bricks;
    score = 0;
    state = GAMEOVER;
  } in
  assert_equal BK_empty (brick_hit (game, 0, 0));  (* BK_empty -> BK_empty *)
  assert_equal BK_empty (brick_hit (game, 0, 1));  (* BK_simple -> BK_empty *)
  assert_equal BK_simple (brick_hit (game, 1, 0)); (* BK_double -> BK_simple *)
  assert_equal BK_block (brick_hit (game, 1, 1))   (* BK_block -> BK_block *)
;;

(**
  Cette fonction `test_brick_color` est utilisée pour tester la fonction `brick_color`.
  Elle crée un tableau 2D de briques et affirme les valeurs attendues renvoyées par la fonction `brick_color`.
  La fonction `brick_color` prend en compte le jeu, ainsi que les indices de ligne et de colonne, et renvoie la couleur de la brique à cette position.
  @autor Alexandre SARDIN    
*)
let test_brick_color _ =
  let bricks = [|
    [| {kind = BK_empty; hp = 0}; {kind = BK_simple; hp = 1} |];
    [| {kind = BK_double; hp = 1}; {kind = BK_block; hp = 2} |]
  |] in
  let game = {
    params = make_camlbrick_param ();
    ball = {
      position = make_vec2(0, 0);
      velocity = make_vec2(0, 0);
      size = BS_MEDIUM;
    };
    paddle = {
      position = make_vec2(0, 0);
      size = PS_SMALL;
      width = 0;
      height = 0;
    };
    bricks = bricks;
    score = 0;
    state = GAMEOVER;
  } in
  assert_equal WHITE (brick_color (game, 0, 0));  (* BK_empty -> WHITE *)
  assert_equal BLUE (brick_color (game, 0, 1));   (* BK_simple -> BLUE *)
  assert_equal YELLOW (brick_color (game, 1, 0)); (* BK_double -> YELLOW *)
  assert_equal GREEN (brick_color (game, 1, 1))   (* BK_block -> GREEN *)
;;

(* Appels des fonctions de test *)
test_fonc_make_vec2 ();
test_fonc_vec2_add ();
test_fonc_vec2_add_scalar ();
test_fonc_vec2_mult ();
test_fonc_vec2_mult_scalar ();
test_fonc_param_get ();
test_make_camlbrick ();
test_string_of_gamestate ();
test_brick_get ();;
test_brick_hit ();;
test_brick_color ();;
(* Affiche le rapport de test *)
  test_report();;  
