module Plateau = Bataille_navale.Plateau
module Bot = Bataille_navale.Bot

let demander_placement_bateau (p : Plateau.plateau) : unit =
  let rec demander_coordonnees (longueur : int) =
    print_endline ("Placement du bateau de taille " ^ string_of_int longueur);
    print_endline "Entrez les coordonnées de départ du bateau (x y) :";
    let x = read_int () in
    let y = read_int () in
    print_endline "Le bateau est-il horizontal ? (o/n) :";
    let horizontal = read_line () = "o" in
    if Plateau.placer_bateau_valide p y x longueur horizontal then
      print_endline "Bateau placé avec succès !"
    else (
      print_endline "Placement invalide. Veuillez réessayer.";
      demander_coordonnees longueur)
  in
  List.iter demander_coordonnees Plateau.flotte_standard

let () =
  Random.self_init ();
  let plateau_joueur = Plateau.init_plateau () in
  let plateau_bot = Plateau.init_plateau () in
  demander_placement_bateau plateau_joueur;
  Bot.placer_flotte_aleatoire plateau_bot
