! decide && G (F hready) && ! F (G busreq) && G (! ready1 -> X (! decide)) -> (G (decide -> X (X (incr && locked -> (! ready1 U (hready && ! busreq) || G (! ready1)) && (! (incr && locked) -> ready1))) && (ready1 && X[!] (! decide) -> X ready1) && (ready1 && X decide -> X[!] (! ready1 && X[!] (! ready1)))) && ready1)
