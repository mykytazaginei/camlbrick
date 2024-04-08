open Camlbrick;;
open Camlbrick_gui;;


let game = make_camlbrick();;
let param = param_get(game);;

let tab : t_brick_kind array = [| BK_simple; BK_double; BK_block; BK_bonus |] in
for x = 0 to Array.length game.bricks - 1 do
  for y = 0 to Array.length game.bricks.(x) - 1 do
    game.bricks.(x).(y) <- tab.(Random.int (Array.length tab))
  done;
done;


(* fonction qui lance le jeu *)
launch_camlbrick(param,game);;