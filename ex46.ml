type bag 

type role = Ranger | Warrior | Magician

type loot = Coin | Chicken_Wing | Sponge

type race = Golem | Boar | Mosquitoes of int

type monster = {
  race: race;
  hp: int;
  item_held: loot;
}

type player = {
  role: role;
  hp: int;
  xp: int;
  bag: (loot * int) list;
}

let rec add_to_bag item bag =
  match bag with
  | [] -> [(item, 1)]
  | (s_item, n)::t -> if s_item = item then 
    (item, succ n)::t else
    (s_item, n)::add_to_bag item t

let rec print_bag bag =
  match bag with
  | [] -> Printf.printf ""
  | (item, n)::t -> Printf.printf "%d %s%s\n" n
      (match item with
      | Coin -> "coin"
      | Chicken_Wing -> "chicken wing"
      | Sponge -> "sponge")
      (if n>1 then "s" else "");
      print_bag t

let random_monster () =
  let _ = Random.self init () in
  { race =
      match Random.int 3 with
      | 0 -> Golem
      | 1 -> Boar
      | 2 -> Mosquitoes (Random.int 5)+1
    hp = (Random.int 5)+1
    item_held =
      match Random.int 3 with
      | 0 -> Chicken_Wing
      | 1 -> Coin
      | 2 -> Sponge}

let hit player = 
  let _ = Random.self init () in
  match player.role with
  | Warrior -> if (Random.int 101) < (31 + (5 * player.xp)) then 10 else 0
  | Ranger -> if (Random.int 101) < (71 + (5 * player.xp)) then 4 else 0
  | Magician -> if (Random.int 101) < (51 + (5 * player.xp)) then 5 else 0

let monster_hit monster =
  let _ = Random.self init () in
  match monster.race with
  | Golem -> 4
  | Boar -> 2
  | Mosquitoes n -> 0.5*n

exception Death

let rec fight player monster =
  monster.hp - hit player in
  player.hp - monster_hit monster in

  if player.hp > 0 then
    (if monster.hp > 0 then 
      fight player monster else
      player.xp + 5
      let _ = add_to_bag monster.item_held player.bag) else
    raise Death



let _ = print_bag [(Chicken_Wing, 2); (Coin, 1); (Sponge, 2)]

let unfortunate_event player =
  let m = random monster ()
  printf "%s has arrived" m.race
  fight player m

let rec game 