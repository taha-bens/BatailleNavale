open Game

type bot = game_view -> play

(** Place les bateaux sur le plateau de maniere aleatoire.
    @param p Le plateau sur lequel les bateaux doivent être placés. *)
let placer_flotte_aleatoire (p : Plateau.plateau) : unit =
  let random_orientation () = Random.bool () in
  let random_position () = Random.int 10 in
  List.iter (fun taille ->
    let rec try_place () =
      let x = random_position () in
      let y = random_position () in
      let horizontal = random_orientation () in
      if not (Plateau.placer_bateau_valide p y x taille horizontal) then try_place ()
    in
    try_place ()
  ) Plateau.flotte_standard