(**
Ce module Camlbrick représente le noyau fonctionnel du jeu de casse-brique nommé <b>camlbrick</b>
(un jeu de mot entre le jeu casse-brique et le mot ocaml).

Le noyau fonctionnel consiste à réaliser l'ensemble des structures et autres fonctions capables
d'être utilisées par une interface graphique. Par conséquent, dans ce module il n'y a aucun
aspect visuel! Vous pouvez utiliser le mode console.

Le principe du jeu de casse-brique consiste à faire disparaître toutes les briques d'un niveau
en utilisant les rebonds d'une balle depuis une raquette contrôlée par l'utilisateur.

@author ZAGINEI Mykyta
@author NGUYEN Hau 
@author TOTSKYI Hlib
@author SARDIN Alexandre

@version 1
*)

(** Compteur utilisé en interne pour afficher le numéro de la frame du jeu vidéo. 
    Vous pouvez utiliser cette variable en lecture, mais nous ne devez pas modifier
    sa valeur! *)
let frames = ref 0;;

(**
  type énuméré représentant les couleurs gérables par notre moteur de jeu. Vous ne pouvez pas modifier ce type!
  @deprecated Ne pas modifier ce type! 
*)
type t_camlbrick_color = WHITE | BLACK | GRAY | LIGHTGRAY | DARKGRAY | BLUE | RED | GREEN | YELLOW | CYAN | MAGENTA | ORANGE | LIME | PURPLE;;

(**
  Cette structure regroupe tous les attributs globaux,
  pour paramétrer notre jeu vidéo.
  <b>Attention:</b> Il doit y avoir des cohérences entre les différents paramètres:
  <ul>
  <li> la hauteur totale de la fenêtre est égale à la somme des hauteurs de la zone de briques du monde et
  de la hauteur de la zone libre.</li>
  <li>la hauteur de la zone des briques du monde est un multiple de la hauteur d'une seule brique. </li>
  <li>la largeur du monde est un multiple de la largeur d'une seule brique. </li>
  <li>initialement la largeur de la raquette doit correspondre à la taille moyenne.</li>
  <li>la hauteur initiale de la raquette doit être raisonnable et ne pas toucher un bord de la fenêtre.</li>
  <li>La variable <u>time_speed</u> doit être strictement positive. Et représente l'écoulement du temps.</li>
  </ul>
*)
type t_camlbrick_param = {
  world_width : int; (** largeur de la zone de dessin des briques *)
  world_bricks_height : int; (** hauteur de la zone de dessin des briques *)
  world_empty_height : int; (** hauteur de la zone vide pour que la bille puisse évoluer un petit peu *)

  brick_width : int; (** largeur d'une brique *)
  brick_height : int; (** hauteur d'une brique *)

  paddle_init_width : int; (** largeur initiale de la raquette *)
  paddle_init_height : int; (** hauteur initiale de la raquette *)

  time_speed : int ref; (** indique l'écoulement du temps en millisecondes (c'est une durée approximative) *)
};;

(** Enumeration des différents types de briques. 
  Vous ne devez pas modifier ce type.    
*)
type t_brick_kind = BK_empty | BK_simple | BK_double | BK_block | BK_bonus;;

(**
  Cette fonction renvoie le type de brique pour représenter les briques de vide.
  C'est à dire, l'information qui encode l'absence de brique à un emplacement sur la grille du monde.
  @return Renvoie le type correspondant à la notion de vide.
  @deprecated  Cette fonction est utilisé en interne.    
*)

let make_empty_brick() : t_brick_kind = 
  BK_empty
;;

(** 
    Enumeration des différentes tailles des billes. 
    La taille  normale d'une bille est [BS_MEDIUM]. 
  
    Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)
type t_ball_size = BS_SMALL | BS_MEDIUM | BS_BIG;;

(** 
  Enumeration des différentes taille de la raquette. Par défaut, une raquette doit avoir la taille
  [PS_SMALL]. 

  Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)
type t_paddle_size = PS_SMALL | PS_MEDIUM | PS_BIG;;


(** 
  Enumération des différents états du jeu. Nous avons les trois états de base:
    <ul>
    <li>[GAMEOVER]: qui indique si une partie est finie typiquement lors du lancement du jeu</li>
    <li>[PLAYING]: qui indique qu'une partie est en cours d'exécution</li>
    <li>[PAUSING]: indique qu'une partie en cours d'exécution est actuellement en pause</li>
    </ul>
    
    Dans le cadre des extensions, vous pouvez modifier ce type pour adopter d'autres états du jeu selon
    votre besoin.
*)
type t_gamestate = GAMEOVER | PLAYING | PAUSING;;


(** 
  Cette fonction permet de créer un vecteur 2D à partir de deux entiers.
  Les entiers représentent la composante en X et en Y du vecteur.

  Vous devez modifier cette fonction.
  @param x première composante du vecteur
  @param y seconde composante du vecteur
  @return Renvoie le vecteur dont les composantes sont (x,y).
  @deprecated Cette fonction est utilisée en interne.
  @autor Mykyta ZAGINEI 
  *)
(* Itération 1 *)
type t_vec2 = {dx : int ; dy : int};;


(**
  Cette fonction permet de créer un vecteur 2D à partir de deux entiers.
  Les entiers représentent la composante en X et en Y du vecteur.

  Vous devez modifier cette fonction.
  @param x première composante du vecteur
  @param y seconde composante du vecteur
  @return Renvoie le vecteur dont les composantes sont (x,y).
  @author Mykyta ZAGINEI
*)
let make_vec2(x,y : int * int) : t_vec2 = 
  {dx = x ; dy = y}
;;

(**
  Cette fonction renvoie un vecteur qui est la somme des deux vecteurs donnés en arguments.
  @param a premier vecteur
  @param b second vecteur
  @return Renvoie un vecteur égale à la somme des vecteurs.
  @author Zaginei Mykyta
*)
let vec2_add(a,b : t_vec2 * t_vec2) : t_vec2 =
  {dx = a.dx + b.dx; dy = a.dy + b.dy}
;;


(**
  Cette fonction renvoie un vecteur égale à la somme d'un vecteur
  donné en argument et un autre vecteur construit à partir de (x,y).
  
  @param a premier vecteur
  @param x composante en x du second vecteur
  @param y composante en y du second vecteur
  @return Renvoie un vecteur qui est la résultante du vecteur 
  @author Mykyta ZAGINEI
*)
let vec2_add_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
   {dx = a.dx + x; dy = a.dy + y}
;;


(**
  Cette fonction calcul un vecteur où 
  ses composantes sont la résultante de la multiplication  des composantes de deux vecteurs en entrée.
  Ainsi,
    {[
    c_x = a_x * b_x
    c_y = a_y * b_y
    ]}
  @param a premier vecteur
  @param b second vecteur
  @return Renvoie un vecteur qui résulte de la multiplication des composantes. 
  @author Mykyta ZAGINEI
*)
let vec2_mult(a,b : t_vec2 * t_vec2) : t_vec2 = 
  {dx = a.dx * b.dx; dy = a.dy * b.dy}
;;

(**
  Cette fonction calcul la multiplication des composantes du vecteur a et du vecteur construit à partir de (x,y).
  Cette fonction est une optimisation du code suivant (que vous ne devez pas faire en l'état):
  {[
let vec2_mult_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
  vec2_mult(a, make_vec2(x,y))
;;
  ]}
  @param a premier vecteur
  @param x composante en x du second vecteur
  @param y composante en y du second vecteur
  @return Renvoie un vecteur qui résulte de la multiplication des composantes.
  @author Mykyta ZAGINEI
*)
let vec2_mult_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
  {dx = a.dx * x; dy = a.dy * y}
;;

(** 
  Type pour ball
  @author Hau NGUYEN
  @autor ZAGINEI Mykyta
  *)
  type t_ball = {
    position : t_vec2 ref;
    velocity : t_vec2 ref;
    size : t_ball_size;
  }
  ;;
  
(** 
  Type pour paddle
  @autor Hau NGUYEN 
*)
  type t_paddle = {
    position :(int ref) * int ;
    size : t_paddle_size;
  }
  ;;

(**
  Cette structure représente l'état du jeu de casse-brique. 
  Elle contient l'ensemble des informations nécessaires pour représenter le jeu.
  Vous devez modifier cette structure pour ajouter les informations nécessaires pour représenter le jeu.
  @deprecated Cette structure est utilisée en interne.
  @autor Hau NGUYEN    
*)
type t_camlbrick = {
  params : t_camlbrick_param;
  ball : t_ball;
  paddle : t_paddle;
  bricks : t_brick_kind array array;
  state : t_gamestate;
  speed : int ref
}
;;


(**
  Cette fonction construit le paramétrage du jeu, avec des informations personnalisable avec les contraintes du sujet.
  Il n'y a aucune vérification et vous devez vous assurer que les valeurs données en argument soient cohérentes.
  @return Renvoie un paramétrage de jeu par défaut      
*)
let make_camlbrick_param() : t_camlbrick_param = {
   world_width = 800;
   world_bricks_height = 600;
   world_empty_height = 200;

   brick_width = 40;
   brick_height = 20;

   paddle_init_width = 100;
   paddle_init_height = 20;

   time_speed = ref 20;
}
;;


(**
  Cette fonction extrait le paramétrage d'un jeu à partir du jeu donné en argument.
  @param game jeu en cours d'exécution.
  @return Renvoie le paramétrage actuel.
  @autor Hau NGUYEN
  *)
let param_get(game : t_camlbrick) : t_camlbrick_param =
  (* Itération 1 *)
  game.params
;;

(** [make_camlbrick ()] est une fonction qui crée une nouvelle instance de l'état du jeu pour CamlBrick.
    Elle initialise les paramètres du jeu, la balle, la raquette, les briques, le score et l'état.

    @return A new instance of the game state for CamlBrick.
    @autor Mykyta ZAGINEI
    @autor Hau NGUYEN
*)
let make_camlbrick() : t_camlbrick = 
  {
    params = make_camlbrick_param ();
    ball = {
      position = ref (make_vec2(0, 0));
      velocity = ref(make_vec2(0, 0)); 
      size = BS_MEDIUM;
    };
    paddle = {
      position = (ref 0, 0);
      size = PS_MEDIUM;
    };
    bricks = Array.make_matrix 20 30 BK_empty;
    state = PLAYING;
    speed = ref 5
  }
;;

(**
  Cette fonction crée une raquette par défaut au milieu de l'écran et de taille normal.  
  @deprecated Cette fonction est là juste pour le debug ou pour débuter certains traitements de test.
  @author Hau NGUYEN
*)
let make_paddle() : t_paddle =
  (* Itération 2 *)
  {
    position = (ref 0, 0);
    size = PS_MEDIUM;
  }
;;

(** 
  Cette fonction permet de créer une balle par défaut au milieu de l'écran et de taille moyenne.
  @param x la position en x de la balle
  @param y la position en y de la balle
  @param size la taille de la balle
  @return Renvoie une balle par défaut.  
  @autor Hau NGUYEN
*)
let make_ball(x,y, size : int * int * int) : t_ball =
  (* Itération 3 *)
  let ball_size =
    if size = 1 then BS_SMALL
    else if size = 2 then BS_MEDIUM
    else if size = 3 then BS_BIG
    else BS_MEDIUM 
    in
    {
      position = ref(make_vec2 (x, y));
      velocity = ref(make_vec2(10, 10));
      size = ball_size;
    }
;;

(**
  Fonction utilitaire qui permet de traduire l'état du jeu sous la forme d'une chaîne de caractère.
  Cette fonction est appelée à chaque frame, et est affichée directement dans l'interface graphique.
  
  Vous devez modifier cette fonction.

  @param game représente le jeu en cours d'exécution.
  @return Renvoie la chaîne de caractère représentant l'état du jeu.
  @author Hau NGUYEN
*)
let string_of_gamestate(game : t_camlbrick) : string =
  (* Itération 1,2,3 et 4 *)
  if game.state = GAMEOVER 
  then "GAMEOVER"
  else if game.state = PLAYING 
       then "PLAYING"
       else "PAUSING"
;;

(** brick_get est une fonction qui renvoie le type de brique à la position (i, j) dans le jeu.
    @param game est l'état du jeu de type [t_camlbrick].
    @param i est l'indice de ligne de la brique.
    @param j est l'indice de colonne de la brique.
    @return le type de brique à la position (i, j) de type [t_brick_kind]. 
    @autor Mykyta ZAGINEI    
*)
let brick_get (game, i , j : t_camlbrick * int * int) : t_brick_kind =
    game.bricks.(i).(j)
;;

(** 
  Brick_hit qui réalise les modifications dans la zone de brique pour faire évoluer une brique comme si elle était
  touchée par une balle.   
  @param game le jeu en cours
  @param i la ligne de la brique
  @param j la colonne de la brique
  @return Renvoie le type de brique après le choc.
  @autor Totskyi Hlib
*)
let brick_hit(game, i, j : t_camlbrick * int * int)  : t_brick_kind = 
  (* Itération 1 *)
  if game.bricks.(i).(j) = BK_empty then
    BK_empty
  else if game.bricks.(i).(j) = BK_simple then
    BK_empty
  else if game.bricks.(i).(j) = BK_double then
    BK_simple
  else if game.bricks.(i).(j) = BK_block then
    BK_block
  else
    BK_empty
;;

(**
  Cette fonction permet de calculer la couleur d'une brique à partir de son type.
  @param game le jeu en cours
  @param i la ligne de la brique
  @param j la colonne de la brique
  @return Renvoie la couleur de la brique.
  @autor Totskyi Hlib   
*)
let brick_color(game,i,j : t_camlbrick * int * int) : t_camlbrick_color = 
  (* Itération 1 *)
  if game.bricks.(i).(j) = BK_empty then
    WHITE
  else if game.bricks.(i).(j) = BK_simple then
    BLUE
  else if game.bricks.(i).(j) = BK_double then
    YELLOW
  else if game.bricks.(i).(j) = BK_block then
    GREEN
  else
    BLACK
;;

    
(**
  Cette function renvoie la position gauche du rectangle symbolisant la raquette.  
  @param game le jeu en cours
  @return Renvoie la position gauche de la raquette.
  @autor Hau NGUYEN
*)
let paddle_x(game : t_camlbrick) : int = 
  (* Itération 2 *)
  !(fst game.paddle.position)
;;

(**
  Cette function renvoie la largeur en pixel du rectangle. 
  @param game le jeu en cours
  @return Renvoie la largeur en pixel de la raquette.
  @autor Hau NGUYEN   
*)
let paddle_size_pixel(game : t_camlbrick) : int = 
  (* Itération 2 *)
  let parametrs : t_camlbrick_param = param_get game in
  if game.paddle.size = PS_SMALL then
    parametrs.paddle_init_width
  else if game.paddle.size = PS_MEDIUM then
    parametrs.paddle_init_width * 2
  else 
    parametrs.paddle_init_width * 3
;;

(**
  Cette fonction permet de deplacer la position en gauche de la raquette.
  @param game le jeu en cours
  @return Renvoie la position en y de la raquette.
  @autor Sardin Alexandre
  @autor Mykyta ZAGINEI
*)
let paddle_move_left(game : t_camlbrick) : unit = 
  (* Itération 2 *)
  if !(fst game.paddle.position) > -345
    then fst game.paddle.position := !(fst game.paddle.position) - 20
;;

(**
  Cette fonction permet de deplacer la position en droit de la raquette.
  @param game le jeu en cours 
  @return Renvoie la position en y de la raquette.
  @autor Sardin Alexandre
  @autor Mykyta ZAGINEI
*)
let paddle_move_right(game : t_camlbrick) : unit = 
  (* Itération 2 *)
  if !(fst game.paddle.position) + paddle_size_pixel(game) < 545
    then fst game.paddle.position := !(fst game.paddle.position) + 20
;;

(**
  Cette fonction permet de récupérer la position en x de la balle.
  @param game le jeu en cours
  @return Renvoie la position en x de la balle.
  @autor Hau NGUYEN
*)
let has_ball(game : t_camlbrick) : bool =
  (* Itération 2 *)
  !(game.ball.position) <> make_vec2(0, 0) 
;;

(**
  Cette fonction permet de récupérer le nombre de balles dans le jeu.
  @param game le jeu en cours
  @return Renvoie le nombre de balles.
  @autor Hau NGUYEN    
*)
let balls_count(game : t_camlbrick) : int =
  (* Itération 2 *)
  if has_ball(game)
    then 1
    else 0
;;

(**
  Cette fonction permet de récupérer la liste des balles du jeu.
  @param game le jeu en cours
  @return Renvoie la liste des balles.
  @autor Hau NGUYEN
*)
let balls_get(game : t_camlbrick) : t_ball list = 
  (* Itération 2 *)
  [game.ball]
;;

(**
  Cette fonction permet de récupérer une balle à partir de son index.
  @param game le jeu en cours
  @param i l'index de la balle
  @return Renvoie la balle correspondante à l'index.
  @autor Mykyta ZAGINEI
*)
let ball_get(game, i : t_camlbrick * int) : t_ball =
  (* Itération 2 *)
  let balls : t_ball list = (balls_get(game)) in
  List.nth balls i
;;

(**
  Cette fonction permet de récupérer la position en x d'une balle.
  @param game le jeu en cours
  @param ball la balle
  @return Renvoie la position en x de la balle.
  @autor Hau NGUYEN
*)
let ball_x(game, ball : t_camlbrick * t_ball) : int  =
  (* Itération 2 *)
  !(ball.position).dx
;;

(**
  Cette fonction permet de récupérer la position en y d'une balle.
  @param game le jeu en cours
  @param ball la balle
  @return Renvoie la position en y de la balle.
  @autor Hau NGUYEN
*)
let ball_y(game, ball : t_camlbrick * t_ball) : int =
  (* Itération 2 *)
  !(ball.position).dy
;;

(**
  Cette fonction permet de récupérer la taille en pixel d'une balle.
  @param game le jeu en cours
  @param ball la balle
  @return Renvoie la taille en pixel de la balle.
  @autor Hau NGUYEN
*)
let ball_size_pixel(game, ball : t_camlbrick * t_ball) : int =
  (* Itération 2 *)
  if ball.size = BS_SMALL
    then 10
  else if ball.size = BS_MEDIUM
    then 20
  else if ball.size = BS_BIG
    then 30
  else failwith  "Invalid size of the ball" 
;;

(**
  Cette fonction permet de récupérer la couleur d'une balle.
  @param game le jeu en cours
  @param ball la balle
  @return Renvoie la couleur de la balle.
  @autor Hau NGUYEN
*)
let ball_color(game, ball : t_camlbrick * t_ball) : t_camlbrick_color =
  (* Itération 2 *)
  if ball.size = BS_SMALL
    then YELLOW
  else if ball.size = BS_MEDIUM
    then ORANGE
  else RED  
;;

(**
  Cette fonction permet de modifier la vitesse en x d'une balle.
  @param game le jeu en cours
  @param ball la balle
  @return Renvoie la vitesse en x de la balle.
  @autor Mykyta ZAGINEI    
*)
let ball_modif_speed (game, ball, dv : t_camlbrick * t_ball * t_vec2) : unit =
  ball.velocity := vec2_add (!(ball.velocity), dv)
;;

(**
  Cette fonction permet de modifie la vitesse d'une
  balle par multiplication avec un vecteur.
  @param game le jeu en cours
  @param ball la balle
  @return Renvoie la vitesse en x de la balle.
  @autor Mykyta ZAGINEI     
*)
let ball_modif_speed_sign(game, ball, sv : t_camlbrick * t_ball * t_vec2) : unit =
  ball.velocity := vec2_mult (!(ball.velocity), sv)
;;

(**
  Cette fonction permet de déplacer une balle en fonction de sa vitesse.
  @param game le jeu en cours
  @param ball la balle
  @return Renvoie la balle après le déplacement.
  @autor Hlib TOTSKYI
*)
let is_inside_circle(cx, cy, rad, x, y : int * int * int * int * int) : bool =
  (* Itération 3 *)
  let dx: int = x - cx in
  let dy: int = y - cy in
  let distance_squared:int = dx * dx + dy * dy in
  distance_squared <= rad * rad
;;

(**
  Cette fonction permet de vérifier si un point est à l'intérieur d'un rectangle.
  @param x1 la position en x du coin supérieur gauche du rectangle
  @param y1 la position en y du coin supérieur gauche du rectangle
  @param x2 la position en x du coin inférieur droit du rectangle
  @param y2 la position en y du coin inférieur droit du rectangle
  @param x la position en x du point
  @param y la position en y du point
  @return Renvoie vrai si le point est à l'intérieur du rectangle, faux sinon.
  @autor Mykyta ZAGINEI   
*)
let is_inside_quad(x1,y1,x2,y2, x,y : int * int * int * int * int * int) : bool =
  (* Itération 3 *)
  x >= x1 && x <= x2 && y >= y1 && y <= y2
;;

(**
  Renvoie une nouvelle liste sans les balles qui dépassent la zone de rebond.
  @param game le jeu en cours
  @param i la ligne de la brique
  @param j la colonne de la brique
  @param x la position en x du point
  @param y la position en y du point
  @return Renvoie vrai si le point est à l'intérieur de la brique, faux sinon.
  @autor Hlib TOTSKYI   
*)
let ball_remove_out_of_border (game, balls : t_camlbrick * t_ball list ) : t_ball list =
  let fst_ball : t_ball = List.hd balls in

  if !(fst_ball.position).dy >= game.params.world_width then
    List.tl balls
  else
    balls
;;


(** 
  ball_hit_paddle game ball paddle est une fonction qui vérifie si la balle est entrée en collision avec la pagaie dans
  le jeu de Camlbrick.
  @param game est l'état actuel du jeu.
  @param balle est l'objet balle du jeu.
  @param pagaie est l'objet pagaie du jeu.
  @return La fonction renvoie [true] si la balle est entrée en collision avec la raquette, et [false] dans le cas contraire.
  @autor Mykyta ZAGINEI 
*)
let ball_hit_paddle(game, ball, paddle : t_camlbrick * t_ball * t_paddle) : bool =
  let paddle_size : int = paddle_size_pixel(game) in
  let paddle_x : int = !(fst paddle.position) in
  let paddle_right : int = paddle_x + paddle_size in
  let paddle_y : int = game.params.world_bricks_height + game.params.world_empty_height - game.params.paddle_init_height in
  let paddle_top : int = paddle_y in
  let paddle_bottom : int = paddle_y + game.params.paddle_init_height in
  let ball_x : int = !(ball.position).dx in
  let ball_y : int = !(ball.position).dy in
  let ball_radius : int = ball_size_pixel(game, ball) in

  if ball_y - ball_radius <= paddle_bottom && ball_y + ball_radius >= paddle_top then
    if ball_x + ball_radius >= paddle_x && ball_x - ball_radius <= paddle_right then
      true
    else
      false
  else
    false
;;


(** 
    ball_hit_corner_brick vérifie si la balle a touché le coin d'une brique dans la grille de jeu.
    @param game l'état actuel du jeu.
    @param ball l'objet ball dans le jeu.
    @param i la ligne de la brique.
    @param j la colonne de la brique.
    @return La fonction renvoie [true] si la balle a touché le coin de la brique, et [false] dans le cas contraire.
    @autor Mykyta ZAGINEI 
    @autor Alexandre SARDIN
  *)
let ball_hit_corner_brick (game, ball, i, j : t_camlbrick * t_ball * int * int) : bool =
  let brick_x : int = j * game.params.brick_width in
  let brick_y : int = i * game.params.brick_height in
  let corners : (int * int) array  = [|
    (brick_x, brick_y);  (* Coin supérieur gauche *)
    (brick_x + game.params.brick_width, brick_y);  (* Coin supérieur droit *)
    (brick_x, brick_y + game.params.brick_height);  (* Coin inférieur gauche *)
    (brick_x + game.params.brick_width, brick_y + game.params.brick_height)  (* Coin inférieur droit *)
  |] in
  let ball_x : int = !(ball.position).dx in
  let ball_y : int = !(ball.position).dy in
  let ball_radius : int  = ball_size_pixel(game, ball) in
  let collision : bool ref = ref false in
  for k = 0 to Array.length corners - 1 do
    let (cx, cy) = corners.(k) in
    let diff_x : int = abs (ball_x - cx) in
    let diff_y : int = abs (ball_y - cy) in
    if diff_x <= ball_radius && diff_y <= ball_radius then
      collision := true;
  done;
  !collision
;;

(**
  Cette fonction permet de vérifier si la balle a touché le côté d'une brique.
  @param game le jeu en cours
  @param ball la balle
  @param i la ligne de la brique
  @param j la colonne de la brique
  @return Renvoie vrai si la balle a touché le côté de la brique, faux sinon.
  @autor Mykyta ZAGINEI
  @autor Alexandre SARDIN
  *)
let ball_hit_side_brick(game, ball, i, j: t_camlbrick * t_ball * int * int) : bool =
  (* Itération 3 *)
  let brick_x : int = j * game.params.brick_width in
  let brick_y : int = i * game.params.brick_height in
  let brick_center_x : int  = brick_x + game.params.brick_width / 2 in
  let brick_center_y : int = brick_y + game.params.brick_height / 2 in
  let side_points : (int * int) array= [|
    (brick_x, brick_center_y);  (* Milieu du côté gauche *)
    (brick_x + game.params.brick_width, brick_center_y);  (* Milieu du côté droit *)
    (brick_center_x, brick_y);  (* Milieu du côté supérieur *)
    (brick_center_x, brick_y + game.params.brick_height)  (* Milieu du côté inférieur *)
  |] in
  let ball_x : int = !(ball.position).dx in
  let ball_y : int = !(ball.position).dy in
  let ball_radius : int= ball_size_pixel(game, ball) in

  let collision : bool ref= ref false in
  for k = 0 to Array.length side_points - 1 do
    let (px, py) = side_points.(k) in
    let diff_x = abs (ball_x - px) in
    let diff_y = abs (ball_y - py) in
    if diff_x <= ball_radius && diff_y <= ball_radius then
      collision := true;
  done;
  !collision
;;

let game_test_hit_balls(game, balls : t_camlbrick * t_ball list) : unit =
  (* Itération 3 *)
  ()
;;

(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'elle se déplace. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param x l'abscisse de la position de la souris
  @param y l'ordonnée de la position de la souris     
*)
let canvas_mouse_move(game,x,y : t_camlbrick * int * int) : unit = 
  ()
;;

(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'un bouton est enfoncé. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param button numero du bouton de la souris enfoncé.
  @param x l'abscisse de la position de la souris
  @param y l'ordonnée de la position de la souris     
*)
let canvas_mouse_click_press(game,button,x,y : t_camlbrick * int * int * int) : unit =
  ()
;;


(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'un bouton est relaché. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param button numero du bouton de la souris relaché.
  @param x l'abscisse de la position du relachement
  @param y l'ordonnée de la position du relachement   
*)
let canvas_mouse_click_release(game,button,x,y : t_camlbrick * int * int * int) : unit =
  ()
;;



(**
  Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est appuyée.
  Les arguments sont le jeu en cours, la touche enfoncé sous la forme d'une chaine et sous forme d'un code
  spécifique à labltk.
  
  Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
  les identifiées facilement dans nos traitements.

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param keyString nom de la touche appuyée.
  @param keyCode code entier de la touche appuyée. 
  @autor Totskyi Hlib  
*)
let canvas_keypressed(game, keyString, keyCode : t_camlbrick * string * int) : unit =
  print_string("Key pressed: ");
  print_string(keyString);
  print_string(" code=");
  print_int(keyCode);
  print_newline();
  let left_code : int = 65361 in
  let q_code : int = 113 in
  let right_code : int = 65363 in
  let d_code : int = 100 in
  if keyCode = left_code || keyCode = q_code then
    paddle_move_left game
  else if keyCode = right_code || keyCode = d_code then
    paddle_move_right game
  else
    ()
;;

(**
  Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est relachée.
  Les arguments sont le jeu en cours, la touche relachée sous la forme d'une chaine et sous forme d'un code
  spécifique à labltk.
  
  Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
  les identifiées facilement dans nos traitements.

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param keyString nom de la touche relachée.
  @param keyCode code entier de la touche relachée.   
*)
let canvas_keyreleased(game, keyString, keyCode : t_camlbrick * string * int) =
  print_string("Key released: ");
  print_string(keyString);
  print_string(" code=");
  print_int(keyCode);
  print_newline()
;;

(**
  Cette fonction est utilisée par l'interface graphique pour connaitre l'information
  l'information à afficher dans la zone Custom1 de la zone du menu.
  @autor Hau NGUYEN
*)
let custom1_text() : string =
  (* Iteration 4 *)
  "<King Ball>"
;;

(**
  Cette fonction est utilisée par l'interface graphique pour connaitre l'information
  l'information à afficher dans la zone Custom2 de la zone du menu.
*)
let custom2_text() : string =
  (* Iteration 4 *)
  "<Super Lady>"
;;


(**
  Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Start".

  
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @autor Alexandre SARDIN
*)
let start_onclick(game : t_camlbrick) : unit =
  if game.state = PAUSING then
    let games : t_camlbrick = { game with state = PLAYING } in
    ()
  else
    failwith "the game is not pausing"
;;

(**
  Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Stop".

  
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @autor Alexandre SARDIN
*)
let stop_onclick(game : t_camlbrick) : unit =
  if game.state = PLAYING then
    let games : t_camlbrick = { game with state = PAUSING } in
    ()
  else
    failwith "the game is not playing"
;;

(**
  Cette fonction est appelée par l'interface graphique pour connaitre la valeur
  du slider Speed dans la zone du menu.

  Vous pouvez donc renvoyer une valeur selon votre désir afin d'offrir la possibilité
  d'interagir avec le joueur.
  @param game la partie en cours.
  @return Renvoie la valeur du slider Speed.
  @autor Hlib TOTSKYI
*)
let speed_get(game : t_camlbrick) : int = 
  !(game.speed)
;;


(**
  Cette fonction est appelée par l'interface graphique pour indiquer que le 
  slide Speed dans la zone de menu a été modifiée. 
  
  Ainsi, vous pourrez réagir selon le joueur.
  @param game la partie en cours.
  @param xspeed la nouvelle valeur du slider Speed.
  @autor Hlib TOTSKYI
*)
let speed_change(game,xspeed : t_camlbrick * int) : unit=
  print_endline("Change speed : "^(string_of_int xspeed));
  game.speed := xspeed
;;

(**
  Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Reset".

  
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @autor Hlib TOTSKYI    
*)
let animate_action(game : t_camlbrick) : unit =
  if game.state = PLAYING then begin
    let ball = game.ball in
    (* Mise à jour de la position de la balle en fonction de sa vitesse *)
    ball.position := { 
      dx = !(ball.position).dx + !(ball.velocity).dx; 
      dy = !(ball.position).dy + !(ball.velocity).dy 
    };

    (* Gestion des collisions avec les bords de l'écran *)
    if !(ball.position).dx < 0 || !(ball.position).dx > game.params.world_width then
      ball.velocity := { !(ball.velocity) with dx = -(!(ball.velocity).dx) };  (* Rebond sur les bords latéraux *)
    if !(ball.position).dy < 0 then
      ball.velocity := { !(ball.velocity) with dy = -(!(ball.velocity).dy) };  (* Rebond sur le bord supérieur *)
  end
;;

