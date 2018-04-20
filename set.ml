type 'a set = 'a list

let rec add (set : 'a set) (e : 'a) : 'a set = match set with
    [] -> [e]
  | first :: rest -> if first = e then set else first :: add rest e

let rec u (set1 : 'a set) (set2 : 'a set) : 'a set = match set2 with
    [] -> set1
  | first :: rest -> u (add set1 first) rest

let rec minus (set : 'a set) (e : 'a) : 'a set = match set with
    [] -> []
  | first :: rest -> if first = e then rest else first :: minus rest e
                                                   
